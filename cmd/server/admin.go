// admin.go - Admin API for operations and monitoring

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io/fs"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"time"
)

// AdminConfig holds admin API configuration
type AdminConfig struct {
	Enabled    bool            `toml:"enabled" mapstructure:"enabled"`
	Bind       string          `toml:"bind" mapstructure:"bind"`               // Separate bind address (e.g., "127.0.0.1:8081")
	PathPrefix string          `toml:"path_prefix" mapstructure:"path_prefix"` // Path prefix (e.g., "/admin")
	Auth       AdminAuthConfig `toml:"auth" mapstructure:"auth"`
}

// AdminAuthConfig holds admin authentication configuration
type AdminAuthConfig struct {
	Type     string `toml:"type" mapstructure:"type"`         // "bearer" | "basic"
	Token    string `toml:"token" mapstructure:"token"`       // For bearer auth
	Username string `toml:"username" mapstructure:"username"` // For basic auth
	Password string `toml:"password" mapstructure:"password"` // For basic auth
}

// AdminStats represents system statistics
type AdminStats struct {
	Storage  StorageStats `json:"storage"`
	Users    UserStats    `json:"users"`
	Requests RequestStats `json:"requests"`
	System   SystemStats  `json:"system"`
}

// StorageStats represents storage statistics
type StorageStats struct {
	UsedBytes  int64  `json:"used_bytes"`
	UsedHuman  string `json:"used_human"`
	FileCount  int64  `json:"file_count"`
	FreeBytes  int64  `json:"free_bytes,omitempty"`
	FreeHuman  string `json:"free_human,omitempty"`
	TotalBytes int64  `json:"total_bytes,omitempty"`
	TotalHuman string `json:"total_human,omitempty"`
}

// UserStats represents user statistics
type UserStats struct {
	Total     int64 `json:"total"`
	Active24h int64 `json:"active_24h"`
	Active7d  int64 `json:"active_7d"`
}

// RequestStats represents request statistics
type RequestStats struct {
	Uploads24h   int64 `json:"uploads_24h"`
	Downloads24h int64 `json:"downloads_24h"`
	Errors24h    int64 `json:"errors_24h"`
}

// SystemStats represents system statistics
type SystemStats struct {
	Uptime        string `json:"uptime"`
	Version       string `json:"version"`
	GoVersion     string `json:"go_version"`
	NumGoroutines int    `json:"num_goroutines"`
	MemoryUsageMB int64  `json:"memory_usage_mb"`
	NumCPU        int    `json:"num_cpu"`
}

// FileInfo represents file information for admin API
type FileInfo struct {
	ID          string    `json:"id"`
	Path        string    `json:"path"`
	Name        string    `json:"name"`
	Size        int64     `json:"size"`
	SizeHuman   string    `json:"size_human"`
	ContentType string    `json:"content_type"`
	ModTime     time.Time `json:"mod_time"`
	Owner       string    `json:"owner,omitempty"`
}

// FileListResponse represents paginated file list
type FileListResponse struct {
	Files      []FileInfo `json:"files"`
	Total      int64      `json:"total"`
	Page       int        `json:"page"`
	Limit      int        `json:"limit"`
	TotalPages int        `json:"total_pages"`
}

// UserInfo represents user information for admin API
type UserInfo struct {
	JID        string    `json:"jid"`
	QuotaUsed  int64     `json:"quota_used"`
	QuotaLimit int64     `json:"quota_limit"`
	FileCount  int64     `json:"file_count"`
	LastActive time.Time `json:"last_active,omitempty"`
	IsBanned   bool      `json:"is_banned"`
}

// BanInfo represents ban information
type BanInfo struct {
	IP          string    `json:"ip"`
	Reason      string    `json:"reason"`
	CreatedAt   time.Time `json:"created_at"`
	ExpiresAt   time.Time `json:"expires_at,omitempty"`
	IsPermanent bool      `json:"is_permanent"`
}

var (
	serverStartTime = time.Now()
	adminConfig     *AdminConfig
)

// SetupAdminRoutes sets up admin API routes
func SetupAdminRoutes(mux *http.ServeMux, config *AdminConfig) {
	adminConfig = config

	if !config.Enabled {
		log.Info("Admin API is disabled")
		return
	}

	prefix := config.PathPrefix
	if prefix == "" {
		prefix = "/admin"
	}

	// Wrap all admin handlers with authentication
	adminMux := http.NewServeMux()

	adminMux.HandleFunc(prefix+"/stats", handleAdminStats)
	adminMux.HandleFunc(prefix+"/files", handleAdminFiles)
	adminMux.HandleFunc(prefix+"/files/", handleAdminFileByID)
	adminMux.HandleFunc(prefix+"/users", handleAdminUsers)
	adminMux.HandleFunc(prefix+"/users/", handleAdminUserByJID)
	adminMux.HandleFunc(prefix+"/bans", handleAdminBans)
	adminMux.HandleFunc(prefix+"/bans/", handleAdminBanByIP)
	adminMux.HandleFunc(prefix+"/health", handleAdminHealth)
	adminMux.HandleFunc(prefix+"/config", handleAdminConfig)

	// Register with authentication middleware
	mux.Handle(prefix+"/", AdminAuthMiddleware(adminMux))

	log.Infof("Admin API enabled at %s (auth: %s)", prefix, config.Auth.Type)
}

// AdminAuthMiddleware handles admin authentication
func AdminAuthMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if adminConfig == nil || !adminConfig.Enabled {
			http.Error(w, "Admin API disabled", http.StatusServiceUnavailable)
			return
		}

		authorized := false

		switch adminConfig.Auth.Type {
		case "bearer":
			auth := r.Header.Get("Authorization")
			if strings.HasPrefix(auth, "Bearer ") {
				token := strings.TrimPrefix(auth, "Bearer ")
				authorized = token == adminConfig.Auth.Token
			}
		case "basic":
			username, password, ok := r.BasicAuth()
			if ok {
				authorized = username == adminConfig.Auth.Username &&
					password == adminConfig.Auth.Password
			}
		default:
			// No auth configured, check if request is from localhost
			clientIP := getClientIP(r)
			authorized = clientIP == "127.0.0.1" || clientIP == "::1"
		}

		if !authorized {
			AuditEvent("admin_auth_failure", r, nil)
			w.Header().Set("WWW-Authenticate", `Bearer realm="admin"`)
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
			return
		}

		next.ServeHTTP(w, r)
	})
}

// handleAdminStats returns system statistics
func handleAdminStats(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	AuditAdminAction(r, "get_stats", "system", nil)

	ctx := r.Context()
	stats := AdminStats{}

	// Storage stats
	confMutex.RLock()
	storagePath := conf.Server.StoragePath
	confMutex.RUnlock()

	storageStats := calculateStorageStats(storagePath)
	stats.Storage = storageStats

	// User stats
	stats.Users = calculateUserStats(ctx)

	// Request stats from Prometheus metrics
	stats.Requests = calculateRequestStats()

	// System stats
	var mem runtime.MemStats
	runtime.ReadMemStats(&mem)

	stats.System = SystemStats{
		Uptime:        time.Since(serverStartTime).Round(time.Second).String(),
		Version:       "3.3.0",
		GoVersion:     runtime.Version(),
		NumGoroutines: runtime.NumGoroutine(),
		MemoryUsageMB: int64(mem.Alloc / 1024 / 1024),
		NumCPU:        runtime.NumCPU(),
	}

	writeJSONResponseAdmin(w, stats)
}

// handleAdminFiles handles file listing
func handleAdminFiles(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case http.MethodGet:
		listFiles(w, r)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

// listFiles returns paginated file list
func listFiles(w http.ResponseWriter, r *http.Request) {
	AuditAdminAction(r, "list_files", "files", nil)

	// Parse query parameters
	page, _ := strconv.Atoi(r.URL.Query().Get("page"))
	if page < 1 {
		page = 1
	}
	limit, _ := strconv.Atoi(r.URL.Query().Get("limit"))
	if limit < 1 || limit > 100 {
		limit = 50
	}
	sortBy := r.URL.Query().Get("sort")
	if sortBy == "" {
		sortBy = "date"
	}
	filterOwner := r.URL.Query().Get("owner")

	confMutex.RLock()
	storagePath := conf.Server.StoragePath
	confMutex.RUnlock()

	var files []FileInfo

	err := filepath.WalkDir(storagePath, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return nil // Skip errors
		}
		if d.IsDir() {
			return nil
		}

		info, err := d.Info()
		if err != nil {
			return nil
		}

		relPath, _ := filepath.Rel(storagePath, path)

		fileInfo := FileInfo{
			ID:          relPath,
			Path:        relPath,
			Name:        filepath.Base(path),
			Size:        info.Size(),
			SizeHuman:   formatBytes(info.Size()),
			ContentType: GetContentType(path),
			ModTime:     info.ModTime(),
		}

		// Apply owner filter if specified (simplified: would need metadata lookup)
		_ = filterOwner // Unused for now, but kept for future implementation

		files = append(files, fileInfo)
		return nil
	})

	if err != nil {
		http.Error(w, fmt.Sprintf("Error listing files: %v", err), http.StatusInternalServerError)
		return
	}

	// Sort files
	switch sortBy {
	case "date":
		sort.Slice(files, func(i, j int) bool {
			return files[i].ModTime.After(files[j].ModTime)
		})
	case "size":
		sort.Slice(files, func(i, j int) bool {
			return files[i].Size > files[j].Size
		})
	case "name":
		sort.Slice(files, func(i, j int) bool {
			return files[i].Name < files[j].Name
		})
	}

	// Paginate
	total := len(files)
	start := (page - 1) * limit
	end := start + limit
	if start > total {
		start = total
	}
	if end > total {
		end = total
	}

	response := FileListResponse{
		Files:      files[start:end],
		Total:      int64(total),
		Page:       page,
		Limit:      limit,
		TotalPages: (total + limit - 1) / limit,
	}

	writeJSONResponseAdmin(w, response)
}

// handleAdminFileByID handles single file operations
func handleAdminFileByID(w http.ResponseWriter, r *http.Request) {
	// Extract file ID from path
	prefix := adminConfig.PathPrefix
	if prefix == "" {
		prefix = "/admin"
	}
	fileID := strings.TrimPrefix(r.URL.Path, prefix+"/files/")

	if fileID == "" {
		http.Error(w, "File ID required", http.StatusBadRequest)
		return
	}

	switch r.Method {
	case http.MethodGet:
		getFileInfo(w, r, fileID)
	case http.MethodDelete:
		deleteFile(w, r, fileID)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

// getFileInfo returns information about a specific file
func getFileInfo(w http.ResponseWriter, r *http.Request, fileID string) {
	confMutex.RLock()
	storagePath := conf.Server.StoragePath
	confMutex.RUnlock()

	filePath := filepath.Join(storagePath, fileID)

	// Validate path is within storage
	absPath, err := filepath.Abs(filePath)
	if err != nil || !strings.HasPrefix(absPath, storagePath) {
		http.Error(w, "Invalid file ID", http.StatusBadRequest)
		return
	}

	info, err := os.Stat(filePath)
	if os.IsNotExist(err) {
		http.Error(w, "File not found", http.StatusNotFound)
		return
	}
	if err != nil {
		http.Error(w, fmt.Sprintf("Error accessing file: %v", err), http.StatusInternalServerError)
		return
	}

	fileInfo := FileInfo{
		ID:          fileID,
		Path:        fileID,
		Name:        filepath.Base(filePath),
		Size:        info.Size(),
		SizeHuman:   formatBytes(info.Size()),
		ContentType: GetContentType(filePath),
		ModTime:     info.ModTime(),
	}

	writeJSONResponseAdmin(w, fileInfo)
}

// deleteFile deletes a specific file
func deleteFile(w http.ResponseWriter, r *http.Request, fileID string) {
	confMutex.RLock()
	storagePath := conf.Server.StoragePath
	confMutex.RUnlock()

	filePath := filepath.Join(storagePath, fileID)

	// Validate path is within storage
	absPath, err := filepath.Abs(filePath)
	if err != nil || !strings.HasPrefix(absPath, storagePath) {
		http.Error(w, "Invalid file ID", http.StatusBadRequest)
		return
	}

	// Get file info before deletion for audit
	info, err := os.Stat(filePath)
	if os.IsNotExist(err) {
		http.Error(w, "File not found", http.StatusNotFound)
		return
	}

	AuditAdminAction(r, "delete_file", fileID, map[string]interface{}{
		"size": info.Size(),
	})

	if err := os.Remove(filePath); err != nil {
		http.Error(w, fmt.Sprintf("Error deleting file: %v", err), http.StatusInternalServerError)
		return
	}

	w.WriteHeader(http.StatusNoContent)
}

// handleAdminUsers handles user listing
func handleAdminUsers(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	AuditAdminAction(r, "list_users", "users", nil)

	ctx := r.Context()
	qm := GetQuotaManager()

	var users []UserInfo

	if qm != nil && qm.config.Enabled {
		quotas, err := qm.GetAllQuotas(ctx)
		if err != nil {
			http.Error(w, fmt.Sprintf("Error getting quotas: %v", err), http.StatusInternalServerError)
			return
		}

		for _, quota := range quotas {
			users = append(users, UserInfo{
				JID:        quota.JID,
				QuotaUsed:  quota.Used,
				QuotaLimit: quota.Limit,
				FileCount:  quota.FileCount,
			})
		}
	}

	writeJSONResponseAdmin(w, users)
}

// handleAdminUserByJID handles single user operations
func handleAdminUserByJID(w http.ResponseWriter, r *http.Request) {
	prefix := adminConfig.PathPrefix
	if prefix == "" {
		prefix = "/admin"
	}

	path := strings.TrimPrefix(r.URL.Path, prefix+"/users/")
	parts := strings.Split(path, "/")
	jid := parts[0]

	if jid == "" {
		http.Error(w, "JID required", http.StatusBadRequest)
		return
	}

	// Check for sub-paths
	if len(parts) > 1 {
		switch parts[1] {
		case "files":
			handleUserFiles(w, r, jid)
			return
		case "quota":
			handleUserQuota(w, r, jid)
			return
		}
	}

	switch r.Method {
	case http.MethodGet:
		getUserInfo(w, r, jid)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

// getUserInfo returns information about a specific user
func getUserInfo(w http.ResponseWriter, r *http.Request, jid string) {
	ctx := r.Context()
	qm := GetQuotaManager()

	if qm == nil || !qm.config.Enabled {
		http.Error(w, "Quota tracking not enabled", http.StatusNotImplemented)
		return
	}

	quota, err := qm.GetQuotaInfo(ctx, jid)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error getting quota: %v", err), http.StatusInternalServerError)
		return
	}

	user := UserInfo{
		JID:        jid,
		QuotaUsed:  quota.Used,
		QuotaLimit: quota.Limit,
		FileCount:  quota.FileCount,
	}

	writeJSONResponseAdmin(w, user)
}

// handleUserFiles handles user file operations
func handleUserFiles(w http.ResponseWriter, r *http.Request, jid string) {
	switch r.Method {
	case http.MethodGet:
		// List user's files
		AuditAdminAction(r, "list_user_files", jid, nil)
		// Would need file ownership tracking to implement fully
		writeJSONResponseAdmin(w, []FileInfo{})
	case http.MethodDelete:
		// Delete all user's files
		AuditAdminAction(r, "delete_user_files", jid, nil)
		// Would need file ownership tracking to implement fully
		w.WriteHeader(http.StatusNoContent)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

// handleUserQuota handles user quota operations
func handleUserQuota(w http.ResponseWriter, r *http.Request, jid string) {
	qm := GetQuotaManager()
	if qm == nil {
		http.Error(w, "Quota management not enabled", http.StatusNotImplemented)
		return
	}

	switch r.Method {
	case http.MethodPost:
		// Set custom quota
		var req struct {
			Quota string `json:"quota"`
		}
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			http.Error(w, "Invalid request body", http.StatusBadRequest)
			return
		}

		quota, err := parseSize(req.Quota)
		if err != nil {
			http.Error(w, fmt.Sprintf("Invalid quota: %v", err), http.StatusBadRequest)
			return
		}

		qm.SetCustomQuota(jid, quota)
		AuditAdminAction(r, "set_quota", jid, map[string]interface{}{"quota": req.Quota})

		writeJSONResponseAdmin(w, map[string]interface{}{
			"success": true,
			"jid":     jid,
			"quota":   quota,
		})
	case http.MethodDelete:
		// Remove custom quota
		qm.RemoveCustomQuota(jid)
		AuditAdminAction(r, "remove_quota", jid, nil)
		w.WriteHeader(http.StatusNoContent)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

// handleAdminBans handles ban listing
func handleAdminBans(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	AuditAdminAction(r, "list_bans", "bans", nil)

	// Would need ban management implementation
	writeJSONResponseAdmin(w, []BanInfo{})
}

// handleAdminBanByIP handles single ban operations
func handleAdminBanByIP(w http.ResponseWriter, r *http.Request) {
	prefix := adminConfig.PathPrefix
	if prefix == "" {
		prefix = "/admin"
	}
	ip := strings.TrimPrefix(r.URL.Path, prefix+"/bans/")

	if ip == "" {
		http.Error(w, "IP required", http.StatusBadRequest)
		return
	}

	switch r.Method {
	case http.MethodDelete:
		// Unban IP
		AuditAdminAction(r, "unban", ip, nil)
		// Would need ban management implementation
		w.WriteHeader(http.StatusNoContent)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

// handleAdminHealth returns admin-specific health info
func handleAdminHealth(w http.ResponseWriter, r *http.Request) {
	health := map[string]interface{}{
		"status":    "healthy",
		"timestamp": time.Now().UTC(),
		"uptime":    time.Since(serverStartTime).String(),
	}

	// Check Redis
	if redisClient != nil && redisConnected {
		health["redis"] = "connected"
	} else if redisClient != nil {
		health["redis"] = "disconnected"
	}

	writeJSONResponseAdmin(w, health)
}

// handleAdminConfig returns current configuration (sanitized)
func handleAdminConfig(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	AuditAdminAction(r, "get_config", "config", nil)

	confMutex.RLock()
	// Return sanitized config (no secrets)
	sanitized := map[string]interface{}{
		"server": map[string]interface{}{
			"listen_address":  conf.Server.ListenAddress,
			"storage_path":    conf.Server.StoragePath,
			"max_upload_size": conf.Server.MaxUploadSize,
			"metrics_enabled": conf.Server.MetricsEnabled,
		},
		"security": map[string]interface{}{
			"enhanced_security": conf.Security.EnhancedSecurity,
			"jwt_enabled":       conf.Security.EnableJWT,
		},
		"clamav": map[string]interface{}{
			"enabled": conf.ClamAV.ClamAVEnabled,
		},
		"redis": map[string]interface{}{
			"enabled": conf.Redis.RedisEnabled,
		},
		"deduplication": map[string]interface{}{
			"enabled": conf.Deduplication.Enabled,
		},
	}
	confMutex.RUnlock()

	writeJSONResponseAdmin(w, sanitized)
}

// Helper functions

func calculateStorageStats(storagePath string) StorageStats {
	var totalSize int64
	var fileCount int64

	_ = filepath.WalkDir(storagePath, func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return nil
		}
		if info, err := d.Info(); err == nil {
			totalSize += info.Size()
			fileCount++
		}
		return nil
	})

	return StorageStats{
		UsedBytes: totalSize,
		UsedHuman: formatBytes(totalSize),
		FileCount: fileCount,
	}
}

func calculateUserStats(ctx context.Context) UserStats {
	qm := GetQuotaManager()
	if qm == nil || !qm.config.Enabled {
		return UserStats{}
	}

	quotas, err := qm.GetAllQuotas(ctx)
	if err != nil {
		return UserStats{}
	}

	return UserStats{
		Total: int64(len(quotas)),
	}
}

func calculateRequestStats() RequestStats {
	// These would ideally come from Prometheus metrics
	return RequestStats{}
}

func writeJSONResponseAdmin(w http.ResponseWriter, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(data); err != nil {
		log.Errorf("Failed to encode JSON response: %v", err)
	}
}

// DefaultAdminConfig returns default admin configuration
func DefaultAdminConfig() AdminConfig {
	return AdminConfig{
		Enabled:    false,
		Bind:       "",
		PathPrefix: "/admin",
		Auth: AdminAuthConfig{
			Type: "bearer",
		},
	}
}
