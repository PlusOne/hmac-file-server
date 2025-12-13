// quota.go - Per-user storage quota management

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/go-redis/redis/v8"
	"github.com/sirupsen/logrus"
)

// QuotaConfig holds quota configuration
type QuotaConfig struct {
	Enabled  bool              `toml:"enabled" mapstructure:"enabled"`
	Default  string            `toml:"default" mapstructure:"default"`   // Default quota (e.g., "100MB")
	Tracking string            `toml:"tracking" mapstructure:"tracking"` // "redis" | "memory"
	Custom   map[string]string `toml:"custom" mapstructure:"custom"`     // Custom quotas per JID
}

// QuotaInfo contains quota information for a user
type QuotaInfo struct {
	JID       string `json:"jid"`
	Used      int64  `json:"used"`
	Limit     int64  `json:"limit"`
	Remaining int64  `json:"remaining"`
	FileCount int64  `json:"file_count"`
	IsCustom  bool   `json:"is_custom"`
}

// QuotaExceededError represents a quota exceeded error
type QuotaExceededError struct {
	JID       string `json:"jid"`
	Used      int64  `json:"used"`
	Limit     int64  `json:"limit"`
	Requested int64  `json:"requested"`
}

func (e *QuotaExceededError) Error() string {
	return fmt.Sprintf("quota exceeded for %s: used %d, limit %d, requested %d",
		e.JID, e.Used, e.Limit, e.Requested)
}

// QuotaManager handles per-user storage quotas
type QuotaManager struct {
	config       *QuotaConfig
	redisClient  *redis.Client
	defaultQuota int64
	customQuotas map[string]int64

	// In-memory fallback when Redis is unavailable
	memoryUsage map[string]int64
	memoryFiles map[string]map[string]int64 // jid -> filePath -> size
	mutex       sync.RWMutex
}

var (
	quotaManager *QuotaManager
	quotaOnce    sync.Once
)

// Redis key patterns
const (
	quotaUsedKey  = "quota:%s:used"  // quota:{jid}:used -> int64
	quotaFilesKey = "quota:%s:files" // quota:{jid}:files -> HASH {path: size}
	quotaInfoKey  = "quota:%s:info"  // quota:{jid}:info -> JSON
)

// InitQuotaManager initializes the quota manager
func InitQuotaManager(config *QuotaConfig, redisClient *redis.Client) error {
	var initErr error
	quotaOnce.Do(func() {
		quotaManager = &QuotaManager{
			config:       config,
			redisClient:  redisClient,
			customQuotas: make(map[string]int64),
			memoryUsage:  make(map[string]int64),
			memoryFiles:  make(map[string]map[string]int64),
		}

		// Parse default quota
		if config.Default != "" {
			quota, err := parseSize(config.Default)
			if err != nil {
				initErr = fmt.Errorf("invalid default quota: %w", err)
				return
			}
			quotaManager.defaultQuota = quota
		} else {
			quotaManager.defaultQuota = 100 * 1024 * 1024 // 100MB default
		}

		// Parse custom quotas
		for jid, quotaStr := range config.Custom {
			quota, err := parseSize(quotaStr)
			if err != nil {
				log.Warnf("Invalid custom quota for %s: %v", jid, err)
				continue
			}
			quotaManager.customQuotas[strings.ToLower(jid)] = quota
		}

		log.Infof("Quota manager initialized: enabled=%v, default=%s, custom=%d users, tracking=%s",
			config.Enabled, config.Default, len(config.Custom), config.Tracking)
	})

	return initErr
}

// GetQuotaManager returns the singleton quota manager
func GetQuotaManager() *QuotaManager {
	return quotaManager
}

// GetLimit returns the quota limit for a user
func (q *QuotaManager) GetLimit(jid string) int64 {
	if jid == "" {
		return q.defaultQuota
	}

	jidLower := strings.ToLower(jid)
	if custom, ok := q.customQuotas[jidLower]; ok {
		return custom
	}
	return q.defaultQuota
}

// GetUsage returns the current storage usage for a user
func (q *QuotaManager) GetUsage(ctx context.Context, jid string) (used, limit int64, err error) {
	if !q.config.Enabled {
		return 0, 0, nil
	}

	limit = q.GetLimit(jid)

	// Try Redis first
	if q.redisClient != nil && q.config.Tracking == "redis" {
		key := fmt.Sprintf(quotaUsedKey, jid)
		usedStr, err := q.redisClient.Get(ctx, key).Result()
		if err == redis.Nil {
			return 0, limit, nil
		}
		if err != nil {
			log.Warnf("Failed to get quota from Redis, falling back to memory: %v", err)
		} else {
			used, _ = strconv.ParseInt(usedStr, 10, 64)
			return used, limit, nil
		}
	}

	// Fallback to memory
	q.mutex.RLock()
	used = q.memoryUsage[jid]
	q.mutex.RUnlock()

	return used, limit, nil
}

// GetQuotaInfo returns detailed quota information for a user
func (q *QuotaManager) GetQuotaInfo(ctx context.Context, jid string) (*QuotaInfo, error) {
	used, limit, err := q.GetUsage(ctx, jid)
	if err != nil {
		return nil, err
	}

	fileCount := int64(0)

	// Get file count
	if q.redisClient != nil && q.config.Tracking == "redis" {
		key := fmt.Sprintf(quotaFilesKey, jid)
		count, err := q.redisClient.HLen(ctx, key).Result()
		if err == nil {
			fileCount = count
		}
	} else {
		q.mutex.RLock()
		if files, ok := q.memoryFiles[jid]; ok {
			fileCount = int64(len(files))
		}
		q.mutex.RUnlock()
	}

	_, isCustom := q.customQuotas[strings.ToLower(jid)]

	return &QuotaInfo{
		JID:       jid,
		Used:      used,
		Limit:     limit,
		Remaining: limit - used,
		FileCount: fileCount,
		IsCustom:  isCustom,
	}, nil
}

// CanUpload checks if a user can upload a file of the given size
func (q *QuotaManager) CanUpload(ctx context.Context, jid string, size int64) (bool, error) {
	if !q.config.Enabled {
		return true, nil
	}

	used, limit, err := q.GetUsage(ctx, jid)
	if err != nil {
		// On error, allow upload but log warning
		log.Warnf("Failed to check quota for %s, allowing upload: %v", jid, err)
		return true, nil
	}

	return used+size <= limit, nil
}

// RecordUpload records a file upload for quota tracking
func (q *QuotaManager) RecordUpload(ctx context.Context, jid, filePath string, size int64) error {
	if !q.config.Enabled || jid == "" {
		return nil
	}

	// Try Redis first with atomic operation
	if q.redisClient != nil && q.config.Tracking == "redis" {
		pipe := q.redisClient.TxPipeline()

		usedKey := fmt.Sprintf(quotaUsedKey, jid)
		filesKey := fmt.Sprintf(quotaFilesKey, jid)

		pipe.IncrBy(ctx, usedKey, size)
		pipe.HSet(ctx, filesKey, filePath, size)

		_, err := pipe.Exec(ctx)
		if err != nil {
			log.Warnf("Failed to record upload in Redis: %v", err)
		} else {
			return nil
		}
	}

	// Fallback to memory
	q.mutex.Lock()
	defer q.mutex.Unlock()

	q.memoryUsage[jid] += size

	if q.memoryFiles[jid] == nil {
		q.memoryFiles[jid] = make(map[string]int64)
	}
	q.memoryFiles[jid][filePath] = size

	return nil
}

// RecordDelete records a file deletion for quota tracking
func (q *QuotaManager) RecordDelete(ctx context.Context, jid, filePath string, size int64) error {
	if !q.config.Enabled || jid == "" {
		return nil
	}

	// If size is 0, try to get it from tracking
	if size == 0 {
		size = q.getFileSize(ctx, jid, filePath)
	}

	// Try Redis first
	if q.redisClient != nil && q.config.Tracking == "redis" {
		pipe := q.redisClient.TxPipeline()

		usedKey := fmt.Sprintf(quotaUsedKey, jid)
		filesKey := fmt.Sprintf(quotaFilesKey, jid)

		pipe.DecrBy(ctx, usedKey, size)
		pipe.HDel(ctx, filesKey, filePath)

		_, err := pipe.Exec(ctx)
		if err != nil {
			log.Warnf("Failed to record delete in Redis: %v", err)
		} else {
			return nil
		}
	}

	// Fallback to memory
	q.mutex.Lock()
	defer q.mutex.Unlock()

	q.memoryUsage[jid] -= size
	if q.memoryUsage[jid] < 0 {
		q.memoryUsage[jid] = 0
	}

	if q.memoryFiles[jid] != nil {
		delete(q.memoryFiles[jid], filePath)
	}

	return nil
}

// getFileSize retrieves the size of a tracked file
func (q *QuotaManager) getFileSize(ctx context.Context, jid, filePath string) int64 {
	// Try Redis
	if q.redisClient != nil && q.config.Tracking == "redis" {
		key := fmt.Sprintf(quotaFilesKey, jid)
		sizeStr, err := q.redisClient.HGet(ctx, key, filePath).Result()
		if err == nil {
			size, _ := strconv.ParseInt(sizeStr, 10, 64)
			return size
		}
	}

	// Try memory
	q.mutex.RLock()
	defer q.mutex.RUnlock()

	if files, ok := q.memoryFiles[jid]; ok {
		return files[filePath]
	}

	return 0
}

// SetCustomQuota sets a custom quota for a user
func (q *QuotaManager) SetCustomQuota(jid string, quota int64) {
	q.mutex.Lock()
	defer q.mutex.Unlock()
	q.customQuotas[strings.ToLower(jid)] = quota
}

// RemoveCustomQuota removes a custom quota for a user
func (q *QuotaManager) RemoveCustomQuota(jid string) {
	q.mutex.Lock()
	defer q.mutex.Unlock()
	delete(q.customQuotas, strings.ToLower(jid))
}

// GetAllQuotas returns quota info for all tracked users
func (q *QuotaManager) GetAllQuotas(ctx context.Context) ([]QuotaInfo, error) {
	var quotas []QuotaInfo

	// Get from Redis
	if q.redisClient != nil && q.config.Tracking == "redis" {
		// Scan for all quota keys
		iter := q.redisClient.Scan(ctx, 0, "quota:*:used", 100).Iterator()
		for iter.Next(ctx) {
			key := iter.Val()
			// Extract JID from key
			parts := strings.Split(key, ":")
			if len(parts) >= 2 {
				jid := parts[1]
				info, err := q.GetQuotaInfo(ctx, jid)
				if err == nil {
					quotas = append(quotas, *info)
				}
			}
		}
		return quotas, iter.Err()
	}

	// Get from memory
	q.mutex.RLock()
	defer q.mutex.RUnlock()

	for jid, used := range q.memoryUsage {
		limit := q.GetLimit(jid)
		fileCount := int64(0)
		if files, ok := q.memoryFiles[jid]; ok {
			fileCount = int64(len(files))
		}
		_, isCustom := q.customQuotas[strings.ToLower(jid)]

		quotas = append(quotas, QuotaInfo{
			JID:       jid,
			Used:      used,
			Limit:     limit,
			Remaining: limit - used,
			FileCount: fileCount,
			IsCustom:  isCustom,
		})
	}

	return quotas, nil
}

// Reconcile recalculates quota usage from actual file storage
func (q *QuotaManager) Reconcile(ctx context.Context, jid string, files map[string]int64) error {
	if !q.config.Enabled {
		return nil
	}

	var totalSize int64
	for _, size := range files {
		totalSize += size
	}

	// Update Redis
	if q.redisClient != nil && q.config.Tracking == "redis" {
		usedKey := fmt.Sprintf(quotaUsedKey, jid)
		filesKey := fmt.Sprintf(quotaFilesKey, jid)

		pipe := q.redisClient.TxPipeline()
		pipe.Set(ctx, usedKey, totalSize, 0)
		pipe.Del(ctx, filesKey)

		for path, size := range files {
			pipe.HSet(ctx, filesKey, path, size)
		}

		_, err := pipe.Exec(ctx)
		if err != nil {
			return fmt.Errorf("failed to reconcile quota in Redis: %w", err)
		}
		return nil
	}

	// Update memory
	q.mutex.Lock()
	defer q.mutex.Unlock()

	q.memoryUsage[jid] = totalSize
	q.memoryFiles[jid] = files

	return nil
}

// CheckQuotaMiddleware is a middleware that checks quota before upload
func CheckQuotaMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		qm := GetQuotaManager()
		if qm == nil || !qm.config.Enabled {
			next.ServeHTTP(w, r)
			return
		}

		// Only check for upload methods
		if r.Method != http.MethodPut && r.Method != http.MethodPost {
			next.ServeHTTP(w, r)
			return
		}

		// Get JID from context/headers
		jid := r.Header.Get("X-User-JID")
		if jid == "" {
			// Try to get from authorization context
			if claims, ok := r.Context().Value(contextKey("bearerClaims")).(*BearerTokenClaims); ok {
				jid = claims.User
			}
		}

		if jid == "" {
			next.ServeHTTP(w, r)
			return
		}

		// Check quota
		ctx := r.Context()
		canUpload, err := qm.CanUpload(ctx, jid, r.ContentLength)
		if err != nil {
			log.Warnf("Error checking quota: %v", err)
			next.ServeHTTP(w, r)
			return
		}

		if !canUpload {
			used, limit, _ := qm.GetUsage(ctx, jid)

			// Log to audit
			AuditQuotaExceeded(r, jid, used, limit, r.ContentLength)

			// Return 413 with quota info
			w.Header().Set("Content-Type", "application/json")
			w.Header().Set("X-Quota-Used", strconv.FormatInt(used, 10))
			w.Header().Set("X-Quota-Limit", strconv.FormatInt(limit, 10))
			w.Header().Set("X-Quota-Remaining", strconv.FormatInt(limit-used, 10))
			w.WriteHeader(http.StatusRequestEntityTooLarge)

			_ = json.NewEncoder(w).Encode(map[string]interface{}{
				"error":     "quota_exceeded",
				"message":   "Storage quota exceeded",
				"used":      used,
				"limit":     limit,
				"requested": r.ContentLength,
			})
			return
		}

		// Add quota headers
		used, limit, _ := qm.GetUsage(ctx, jid)
		w.Header().Set("X-Quota-Used", strconv.FormatInt(used, 10))
		w.Header().Set("X-Quota-Limit", strconv.FormatInt(limit, 10))
		w.Header().Set("X-Quota-Remaining", strconv.FormatInt(limit-used, 10))

		next.ServeHTTP(w, r)
	})
}

// UpdateQuotaAfterUpload updates quota after successful upload
func UpdateQuotaAfterUpload(ctx context.Context, jid, filePath string, size int64) {
	qm := GetQuotaManager()
	if qm == nil || !qm.config.Enabled || jid == "" {
		return
	}

	if err := qm.RecordUpload(ctx, jid, filePath, size); err != nil {
		log.WithFields(logrus.Fields{
			"jid":   jid,
			"file":  filePath,
			"size":  size,
			"error": err,
		}).Warn("Failed to update quota after upload")
	}
}

// UpdateQuotaAfterDelete updates quota after file deletion
func UpdateQuotaAfterDelete(ctx context.Context, jid, filePath string, size int64) {
	qm := GetQuotaManager()
	if qm == nil || !qm.config.Enabled || jid == "" {
		return
	}

	if err := qm.RecordDelete(ctx, jid, filePath, size); err != nil {
		log.WithFields(logrus.Fields{
			"jid":   jid,
			"file":  filePath,
			"size":  size,
			"error": err,
		}).Warn("Failed to update quota after delete")
	}
}

// DefaultQuotaConfig returns default quota configuration
func DefaultQuotaConfig() QuotaConfig {
	return QuotaConfig{
		Enabled:  false,
		Default:  "100MB",
		Tracking: "redis",
		Custom:   make(map[string]string),
	}
}

// StartQuotaReconciliation starts a background job to reconcile quotas
func StartQuotaReconciliation(interval time.Duration) {
	if quotaManager == nil || !quotaManager.config.Enabled {
		return
	}

	go func() {
		ticker := time.NewTicker(interval)
		defer ticker.Stop()

		for range ticker.C {
			log.Debug("Running quota reconciliation")
			// This would scan the storage and update quotas
			// Implementation depends on how files are tracked
		}
	}()
}
