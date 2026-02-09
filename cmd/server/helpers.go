package main

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"runtime"
	"strings"
	"syscall"
	"time"

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/mem"
	"gopkg.in/natefinch/lumberjack.v2"
)

// WorkerPool represents a pool of workers
type WorkerPool struct {
	workers   int
	taskQueue chan UploadTask
	scanQueue chan ScanTask
	ctx       context.Context
	cancel    context.CancelFunc
}

// NewWorkerPool creates a new worker pool
func NewWorkerPool(workers int, queueSize int) *WorkerPool {
	ctx, cancel := context.WithCancel(context.Background())
	return &WorkerPool{
		workers:   workers,
		taskQueue: make(chan UploadTask, queueSize),
		scanQueue: make(chan ScanTask, queueSize),
		ctx:       ctx,
		cancel:    cancel,
	}
}

// Start starts the worker pool
func (wp *WorkerPool) Start() {
	for i := 0; i < wp.workers; i++ {
		go wp.worker()
	}
}

// Stop stops the worker pool
func (wp *WorkerPool) Stop() {
	wp.cancel()
	close(wp.taskQueue)
	close(wp.scanQueue)
}

// worker is the worker function
func (wp *WorkerPool) worker() {
	for {
		select {
		case <-wp.ctx.Done():
			return
		case task := <-wp.taskQueue:
			if task.Result != nil {
				task.Result <- nil // Simple implementation
			}
		case scanTask := <-wp.scanQueue:
			err := processScan(scanTask)
			if scanTask.Result != nil {
				scanTask.Result <- err
			}
		}
	}
}

// Stub for precacheStoragePath
func precacheStoragePath(storagePath string) error {
	// TODO: Implement actual pre-caching logic
	// This would typically involve walking the storagePath
	// and loading file information into a cache.
	log.Infof("Pre-caching for storage path '%s' is a stub and not yet implemented.", storagePath)
	return nil
}

func checkFreeSpaceWithRetry(path string, retries int, delay time.Duration) error {
	for i := 0; i < retries; i++ {
		minFreeBytes, err := parseSize(conf.Server.MinFreeBytes)
		if err != nil {
			log.Fatalf("Invalid MinFreeBytes: %v", err)
		}
		if err := checkStorageSpace(path, minFreeBytes); err != nil {
			log.Warnf("Free space check failed (attempt %d/%d): %v", i+1, retries, err)
			time.Sleep(delay)
			continue
		}
		return nil
	}
	return fmt.Errorf("insufficient free space after %d attempts", retries)
}

func handleFileCleanup(conf *Config) {
	if !conf.Server.FileTTLEnabled {
		log.Info("File TTL cleanup is disabled.")
		return
	}

	// Determine TTL: prefer FileTTL, fall back to MaxFileAge
	ttlSource := conf.Server.FileTTL
	if ttlSource == "" {
		ttlSource = conf.Server.MaxFileAge
	}
	if ttlSource == "" {
		log.Warn("File TTL enabled but no filettl or max_file_age configured, defaulting to 720h (30 days)")
		ttlSource = "720h"
	}

	ttlDuration, err := parseTTL(ttlSource)
	if err != nil {
		log.Fatalf("Invalid TTL configuration '%s': %v", ttlSource, err)
	}

	// Determine cleanup interval: prefer CleanupInterval, default to 24h
	cleanupIntervalStr := conf.Server.CleanupInterval
	if cleanupIntervalStr == "" {
		cleanupIntervalStr = "24h"
	}
	cleanupInterval, err := parseTTL(cleanupIntervalStr)
	if err != nil {
		log.Warnf("Invalid cleanup_interval '%s', defaulting to 24h: %v", cleanupIntervalStr, err)
		cleanupInterval = 24 * time.Hour
	}

	log.Infof("File cleanup enabled: TTL=%v, interval=%v, storage=%s",
		ttlDuration, cleanupInterval, conf.Server.StoragePath)

	// Run once immediately at startup
	deleteOldFiles(conf, ttlDuration)

	ticker := time.NewTicker(cleanupInterval)
	defer ticker.Stop()

	for range ticker.C {
		deleteOldFiles(conf, ttlDuration)
	}
}

func computeSHA256(ctx context.Context, filePath string) (string, error) {
	if filePath == "" {
		return "", fmt.Errorf("file path is empty")
	}
	file, err := os.Open(filePath)
	if err != nil {
		return "", fmt.Errorf("failed to open file %s: %w", filePath, err)
	}
	defer file.Close()

	hasher := sha256.New()
	if _, err := io.Copy(hasher, file); err != nil {
		return "", fmt.Errorf("failed to hash file: %w", err)
	}
	return hex.EncodeToString(hasher.Sum(nil)), nil
}

func handleDeduplication(ctx context.Context, absFilename string) error {
	// Check if deduplication is enabled
	confMutex.RLock()
	dedupEnabled := conf.Server.DeduplicationEnabled && conf.Deduplication.Enabled
	confMutex.RUnlock()

	if !dedupEnabled {
		log.Debugf("Deduplication disabled, skipping for file: %s", absFilename)
		return nil
	}

	// Check file size and skip deduplication for very large files (performance optimization)
	fileInfo, err := os.Stat(absFilename)
	if err != nil {
		log.Warnf("Failed to get file size for deduplication: %v", err)
		return nil // Don't fail upload, just skip deduplication
	}

	// Parse maxsize from config, default to 500MB if not set
	confMutex.RLock()
	maxDedupSizeStr := conf.Deduplication.MaxSize
	confMutex.RUnlock()

	maxDedupSize := int64(500 * 1024 * 1024) // Default 500MB
	if maxDedupSizeStr != "" {
		if parsedSize, parseErr := parseSize(maxDedupSizeStr); parseErr == nil {
			maxDedupSize = parsedSize
		}
	}

	if fileInfo.Size() > maxDedupSize {
		log.Infof("File %s (%d bytes) exceeds deduplication size limit (%d bytes), skipping deduplication",
			absFilename, fileInfo.Size(), maxDedupSize)
		return nil
	}

	log.Infof("Starting deduplication for file %s (%d bytes)", absFilename, fileInfo.Size())

	checksum, err := computeSHA256(ctx, absFilename)
	if err != nil {
		log.Warnf("Failed to compute hash for deduplication: %v", err)
		return nil // Don't fail upload, just skip deduplication
	}

	dedupDir := conf.Deduplication.Directory
	if dedupDir == "" {
		return fmt.Errorf("deduplication directory not configured")
	}

	dedupPath := filepath.Join(dedupDir, checksum)
	if err := os.MkdirAll(dedupPath, os.ModePerm); err != nil {
		log.Warnf("Failed to create deduplication directory: %v", err)
		return nil // Don't fail upload
	}

	existingPath := filepath.Join(dedupPath, filepath.Base(absFilename))
	if _, err := os.Stat(existingPath); err == nil {
		log.Infof("File %s is a duplicate, creating hard link", absFilename)
		if linkErr := os.Link(existingPath, absFilename); linkErr != nil {
			log.Warnf("Failed to create hard link for duplicate: %v", linkErr)
			return nil // Don't fail upload
		}
		filesDeduplicatedTotal.Inc()
		return nil
	}

	if err := os.Rename(absFilename, existingPath); err != nil {
		log.Warnf("Failed to move file for deduplication: %v", err)
		return nil // Don't fail upload
	}

	if err := os.Link(existingPath, absFilename); err != nil {
		log.Warnf("Failed to create link after deduplication: %v", err)
		// Try to restore original file
		if restoreErr := os.Rename(existingPath, absFilename); restoreErr != nil {
			log.Errorf("Failed to restore file after deduplication error: %v", restoreErr)
		}
		return nil // Don't fail upload
	}

	log.Infof("Successfully deduplicated file %s", absFilename)
	return nil
}

// handleISOContainer handles ISO container operations
// nolint:unused
func handleISOContainer(absFilename string) error {
	isoPath := filepath.Join(conf.ISO.MountPoint, "container.iso")
	if err := CreateISOContainer([]string{absFilename}, isoPath, conf.ISO.Size, conf.ISO.Charset); err != nil {
		return err
	}
	if err := MountISOContainer(isoPath, conf.ISO.MountPoint); err != nil {
		return err
	}
	return UnmountISOContainer(conf.ISO.MountPoint)
}

func sanitizeFilePath(baseDir, filePath string) (string, error) {
	absBaseDir, err := filepath.Abs(baseDir)
	if err != nil {
		return "", err
	}
	absFilePath, err := filepath.Abs(filepath.Join(absBaseDir, filePath))
	if err != nil {
		return "", err
	}
	if !strings.HasPrefix(absFilePath, absBaseDir) {
		return "", fmt.Errorf("invalid file path: %s", filePath)
	}
	return absFilePath, nil
}

// Stub for formatBytes
func formatBytes(bytes int64) string {
	const unit = 1024
	if bytes < unit {
		return fmt.Sprintf("%d B", bytes)
	}
	div, exp := int64(unit), 0
	for n := bytes / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %ciB", float64(bytes)/float64(div), "KMGTPE"[exp])
}

// cleanupFilesDeletedTotal tracks deleted files during cleanup runs
var cleanupFilesDeletedTotal int64

// deleteOldFiles walks the storage directory and removes files older than ttlDuration.
// It also cleans up empty directories left behind after file deletion.
func deleteOldFiles(conf *Config, ttlDuration time.Duration) {
	confMutex.RLock()
	storagePath := conf.Server.StoragePath
	confMutex.RUnlock()

	if storagePath == "" {
		log.Warn("Cleanup: storage path is empty, skipping")
		return
	}

	now := time.Now()
	var deletedFiles int64
	var deletedBytes int64
	var failedFiles int64
	var emptyDirs []string

	log.Infof("Cleanup: starting scan of %s for files older than %v", storagePath, ttlDuration)

	err := filepath.WalkDir(storagePath, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			log.Warnf("Cleanup: error accessing %s: %v", path, err)
			return nil // continue walking
		}

		// Skip the root storage directory itself
		if path == storagePath {
			return nil
		}

		// Track directories for later empty-dir cleanup
		if d.IsDir() {
			return nil
		}

		info, infoErr := d.Info()
		if infoErr != nil {
			log.Warnf("Cleanup: failed to stat %s: %v", path, infoErr)
			return nil
		}

		fileAge := now.Sub(info.ModTime())
		if fileAge > ttlDuration {
			fileSize := info.Size()
			if removeErr := os.Remove(path); removeErr != nil {
				log.Warnf("Cleanup: failed to remove expired file %s (age: %v): %v", path, fileAge.Round(time.Hour), removeErr)
				failedFiles++
			} else {
				log.Infof("Cleanup: removed expired file %s (age: %v, size: %s)",
					path, fileAge.Round(time.Hour), formatBytes(fileSize))
				deletedFiles++
				deletedBytes += fileSize

				// Mark deleted in metadata store
				if ms := GetMetadataStore(); ms != nil {
					_ = ms.RecordDeletion(context.Background(), path)
				}

				// Clean up associated thumbnail
				CleanupThumbnail(path)

				// Track parent dir for potential cleanup
				parentDir := filepath.Dir(path)
				if parentDir != storagePath {
					emptyDirs = append(emptyDirs, parentDir)
				}
			}
		}

		return nil
	})

	if err != nil {
		log.Errorf("Cleanup: walk error: %v", err)
	}

	// Clean up empty directories (deepest first by sorting in reverse)
	seen := make(map[string]bool)
	for _, dir := range emptyDirs {
		if seen[dir] {
			continue
		}
		seen[dir] = true
		removeEmptyParents(dir, storagePath)
	}

	cleanupFilesDeletedTotal += deletedFiles
	log.Infof("Cleanup: completed — deleted %d files (%s freed), %d failures, total lifetime deletions: %d",
		deletedFiles, formatBytes(deletedBytes), failedFiles, cleanupFilesDeletedTotal)
}

// removeEmptyParents removes empty directories up to (but not including) the root storage path.
func removeEmptyParents(dir, storagePath string) {
	for dir != storagePath && dir != "/" {
		entries, err := os.ReadDir(dir)
		if err != nil || len(entries) > 0 {
			break
		}
		if err := os.Remove(dir); err != nil {
			break
		}
		log.Debugf("Cleanup: removed empty directory %s", dir)
		dir = filepath.Dir(dir)
	}
}

// Stub for CreateISOContainer
func CreateISOContainer(files []string, isoPath, size, charset string) error {
	// TODO: Implement actual ISO container creation logic
	log.Infof("CreateISOContainer is a stub and not yet implemented. It would create an ISO at %s.", isoPath)
	return nil
}

// Stub for MountISOContainer
func MountISOContainer(isoPath, mountPoint string) error {
	// TODO: Implement actual ISO container mounting logic
	log.Infof("MountISOContainer is a stub and not yet implemented. It would mount %s to %s.", isoPath, mountPoint)
	return nil
}

// Stub for UnmountISOContainer
func UnmountISOContainer(mountPoint string) error {
	// TODO: Implement actual ISO container unmounting logic
	log.Infof("UnmountISOContainer is a stub and not yet implemented. It would unmount %s.", mountPoint)
	return nil
}

func checkStorageSpace(storagePath string, minFreeBytes int64) error {
	var stat syscall.Statfs_t
	if err := syscall.Statfs(storagePath, &stat); err != nil {
		return err
	}
	availableBytes := stat.Bavail * uint64(stat.Bsize)
	if int64(availableBytes) < minFreeBytes {
		return fmt.Errorf("not enough space: available %d < required %d", availableBytes, minFreeBytes)
	}
	return nil
}

// setupLogging initializes logging configuration
func setupLogging() {
	log.Infof("DEBUG: Starting setupLogging function")
	if conf.Logging.File != "" {
		log.Infof("DEBUG: Setting up file logging to: %s", conf.Logging.File)
		log.SetOutput(&lumberjack.Logger{
			Filename:   conf.Logging.File,
			MaxSize:    conf.Logging.MaxSize,
			MaxBackups: conf.Logging.MaxBackups,
			MaxAge:     conf.Logging.MaxAge,
			Compress:   conf.Logging.Compress,
		})
		log.Infof("Logging configured to file: %s", conf.Logging.File)
	}
	log.Infof("DEBUG: setupLogging function completed")
}

// logSystemInfo logs system information
func logSystemInfo() {
	memStats, err := mem.VirtualMemory()
	if err != nil {
		log.Warnf("Failed to get memory stats: %v", err)
	} else {
		log.Infof("System Memory: Total=%s, Available=%s, Used=%.1f%%",
			formatBytes(int64(memStats.Total)),
			formatBytes(int64(memStats.Available)),
			memStats.UsedPercent)
	}

	cpuStats, err := cpu.Info()
	if err != nil {
		log.Warnf("Failed to get CPU stats: %v", err)
	} else if len(cpuStats) > 0 {
		log.Infof("CPU: %s, Cores=%d", cpuStats[0].ModelName, len(cpuStats))
	}

	log.Infof("Go Runtime: Version=%s, NumCPU=%d, NumGoroutine=%d",
		runtime.Version(), runtime.NumCPU(), runtime.NumGoroutine())
}

// initMetrics initializes Prometheus metrics
func initMetrics() {
	uploadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name: "upload_duration_seconds",
		Help: "Duration of upload operations in seconds",
	})

	uploadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "upload_errors_total",
		Help: "Total number of upload errors",
	})

	uploadsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "uploads_total",
		Help: "Total number of uploads",
	})

	downloadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name: "download_duration_seconds",
		Help: "Duration of download operations in seconds",
	})

	downloadsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "downloads_total",
		Help: "Total number of downloads",
	})

	downloadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "download_errors_total",
		Help: "Total number of download errors",
	})

	memoryUsage = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "memory_usage_percent",
		Help: "Current memory usage percentage",
	})

	cpuUsage = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "cpu_usage_percent",
		Help: "Current CPU usage percentage",
	})

	activeConnections = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "active_connections_total",
		Help: "Number of active connections",
	})

	requestsTotal = prometheus.NewCounterVec(prometheus.CounterOpts{
		Name: "requests_total",
		Help: "Total number of requests",
	}, []string{"method", "status"})

	goroutines = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "goroutines_total",
		Help: "Number of goroutines",
	})

	uploadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name: "upload_size_bytes",
		Help: "Size of uploaded files in bytes",
	})

	downloadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name: "download_size_bytes",
		Help: "Size of downloaded files in bytes",
	})

	filesDeduplicatedTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "files_deduplicated_total",
		Help: "Total number of deduplicated files",
	})

	deduplicationErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "deduplication_errors_total",
		Help: "Total number of deduplication errors",
	})

	isoContainersCreatedTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "iso_containers_created_total",
		Help: "Total number of ISO containers created",
	})

	isoCreationErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "iso_creation_errors_total",
		Help: "Total number of ISO creation errors",
	})

	isoContainersMountedTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "iso_containers_mounted_total",
		Help: "Total number of ISO containers mounted",
	})

	isoMountErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "iso_mount_errors_total",
		Help: "Total number of ISO mount errors",
	})

	workerAdjustmentsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "worker_adjustments_total",
		Help: "Total number of worker adjustments",
	})

	workerReAdjustmentsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "worker_readjustments_total",
		Help: "Total number of worker readjustments",
	})

	// Register all metrics
	prometheus.MustRegister(
		uploadDuration, uploadErrorsTotal, uploadsTotal,
		downloadDuration, downloadsTotal, downloadErrorsTotal,
		memoryUsage, cpuUsage, activeConnections, requestsTotal,
		goroutines, uploadSizeBytes, downloadSizeBytes,
		filesDeduplicatedTotal, deduplicationErrorsTotal,
		isoContainersCreatedTotal, isoCreationErrorsTotal,
		isoContainersMountedTotal, isoMountErrorsTotal,
		workerAdjustmentsTotal, workerReAdjustmentsTotal,
	)

	log.Info("Prometheus metrics initialized successfully")
}

// scanFileWithClamAV scans a file using ClamAV
func scanFileWithClamAV(filename string) error {
	if clamClient == nil {
		return fmt.Errorf("ClamAV client not initialized")
	}

	// Check file size and skip scanning if too large
	fileInfo, err := os.Stat(filename)
	if err != nil {
		return fmt.Errorf("failed to get file size: %w", err)
	}

	// Parse maxscansize from config, default to 200MB if not set
	confMutex.RLock()
	maxScanSizeStr := conf.ClamAV.MaxScanSize
	confMutex.RUnlock()

	maxScanSize := int64(200 * 1024 * 1024) // Default 200MB
	if maxScanSizeStr != "" {
		if parsedSize, parseErr := parseSize(maxScanSizeStr); parseErr == nil {
			maxScanSize = parsedSize
		}
	}

	if fileInfo.Size() > maxScanSize {
		log.Infof("File %s (%d bytes) exceeds ClamAV scan limit (%d bytes), skipping scan",
			filename, fileInfo.Size(), maxScanSize)
		return nil
	}

	// Also check file extension - only scan configured dangerous types
	confMutex.RLock()
	scanExtensions := conf.ClamAV.ScanFileExtensions
	confMutex.RUnlock()

	if len(scanExtensions) > 0 {
		ext := strings.ToLower(filepath.Ext(filename))
		shouldScan := false
		for _, scanExt := range scanExtensions {
			if ext == strings.ToLower(scanExt) {
				shouldScan = true
				break
			}
		}
		if !shouldScan {
			log.Infof("File %s with extension %s not in scan list, skipping ClamAV scan", filename, ext)
			return nil
		}
	}

	log.Infof("Scanning file %s (%d bytes) with ClamAV", filename, fileInfo.Size())

	result, err := clamClient.ScanFile(filename)
	if err != nil {
		return fmt.Errorf("ClamAV scan failed: %w", err)
	}

	// Handle the result channel with timeout based on file size
	timeout := 10 * time.Second         // Base timeout
	if fileInfo.Size() > 10*1024*1024 { // 10MB+
		timeout = 30 * time.Second
	}
	if fileInfo.Size() > 50*1024*1024 { // 50MB+
		timeout = 60 * time.Second
	}

	if result != nil {
		select {
		case scanResult := <-result:
			if scanResult != nil && scanResult.Status != "OK" {
				log.Errorf("Virus detected in %s: %s", filename, scanResult.Status)
				return fmt.Errorf("virus detected in %s: %s", filename, scanResult.Status)
			}
		case <-time.After(timeout):
			log.Warnf("ClamAV scan timeout (%v) for file: %s (%d bytes)", timeout, filename, fileInfo.Size())
			return fmt.Errorf("ClamAV scan timeout for file: %s", filename)
		}
	}

	log.Infof("File %s passed ClamAV scan successfully", filename)
	return nil
}

// initClamAV initializes ClamAV client
func initClamAV(socketPath string) (*clamd.Clamd, error) {
	if socketPath == "" {
		socketPath = "/var/run/clamav/clamd.ctl"
	}

	client := clamd.NewClamd(socketPath)

	// Test connection
	err := client.Ping()
	if err != nil {
		return nil, fmt.Errorf("failed to ping ClamAV daemon: %w", err)
	}

	log.Infof("ClamAV client initialized with socket: %s", socketPath)
	return client, nil
}

// initRedis initializes Redis client
func initRedis() {
	redisClient = redis.NewClient(&redis.Options{
		Addr:     conf.Redis.RedisAddr,
		Password: conf.Redis.RedisPassword,
		DB:       conf.Redis.RedisDBIndex,
	})

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	_, err := redisClient.Ping(ctx).Result()
	if err != nil {
		log.Warnf("Failed to connect to Redis: %v", err)
		redisConnected = false
	} else {
		log.Info("Redis client initialized successfully")
		redisConnected = true
	}
}

// monitorNetwork monitors network events
// nolint:unused
func monitorNetwork(ctx context.Context) {
	log.Info("Starting network monitoring")
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Network monitoring stopped")
			return
		case <-ticker.C:
			// Simple network monitoring - check interface status
			interfaces, err := net.Interfaces()
			if err != nil {
				log.Warnf("Failed to get network interfaces: %v", err)
				continue
			}

			for _, iface := range interfaces {
				if iface.Flags&net.FlagUp != 0 && iface.Flags&net.FlagLoopback == 0 {
					select {
					case networkEvents <- NetworkEvent{
						Timestamp:   time.Now(),
						EventType:   "interface_up",
						ToNetwork:   iface.Name,
						FromNetwork: "unknown",
						ClientIP:    "",
						UserAgent:   "",
					}:
					default:
						// Channel full, skip
					}
				}
			}
		}
	}
}

// handleNetworkEvents handles network events
// nolint:unused
func handleNetworkEvents(ctx context.Context) {
	log.Info("Starting network event handler")

	for {
		select {
		case <-ctx.Done():
			log.Info("Network event handler stopped")
			return
		case event := <-networkEvents:
			log.Debugf("Network event: %s - From: %s To: %s", event.EventType, event.FromNetwork, event.ToNetwork)
		}
	}
}

// updateSystemMetrics updates system metrics
func updateSystemMetrics(ctx context.Context) {
	ticker := time.NewTicker(15 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			// Update memory metrics
			if memStats, err := mem.VirtualMemory(); err == nil {
				memoryUsage.Set(memStats.UsedPercent)
			}

			// Update CPU metrics
			if cpuPercents, err := cpu.Percent(time.Second, false); err == nil && len(cpuPercents) > 0 {
				cpuUsage.Set(cpuPercents[0])
			}

			// Update goroutine count
			goroutines.Set(float64(runtime.NumGoroutine()))
		}
	}
}

// setupRouter sets up HTTP routes
func setupRouter() *http.ServeMux {
	mux := http.NewServeMux()

	// Add CORS middleware wrapper - Enhanced for multi-upload scenarios
	corsWrapper := func(handler http.HandlerFunc) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			// Enhanced CORS headers for Gajim multi-upload support
			w.Header().Set("Access-Control-Allow-Origin", "*")
			w.Header().Set("Access-Control-Allow-Methods", "GET, PUT, POST, DELETE, OPTIONS, HEAD")
			w.Header().Set("Access-Control-Allow-Headers", "Authorization, Content-Type, Content-Length, X-Requested-With, X-Upload-ID, X-Session-Token, X-File-Name, X-File-Size, Range, Content-Range")
			w.Header().Set("Access-Control-Expose-Headers", "Content-Length, Content-Range, X-Upload-Status, X-Session-ID, Location, ETag")
			w.Header().Set("Access-Control-Max-Age", "86400")
			w.Header().Set("Access-Control-Allow-Credentials", "false")

			// Handle OPTIONS preflight for all endpoints
			if r.Method == http.MethodOptions {
				w.WriteHeader(http.StatusOK)
				return
			}

			handler(w, r)
		}
	}

	mux.HandleFunc("/upload", corsWrapper(handleUpload))
	mux.HandleFunc("/download/", corsWrapper(handleDownload))
	mux.HandleFunc("/health", corsWrapper(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte("OK"))
	}))

	if conf.Server.MetricsEnabled {
		mux.Handle("/metrics", promhttp.Handler())
	}

	// Catch-all handler for all upload protocols (v, v2, token, v3)
	// This must be added last as it matches all paths
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {

		// Enhanced CORS headers for all responses - Multi-upload compatible
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, PUT, POST, DELETE, OPTIONS, HEAD")
		w.Header().Set("Access-Control-Allow-Headers", "Authorization, Content-Type, Content-Length, X-Requested-With, X-Upload-ID, X-Session-Token, X-File-Name, X-File-Size, Range, Content-Range")
		w.Header().Set("Access-Control-Expose-Headers", "Content-Length, Content-Range, X-Upload-Status, X-Session-ID, Location, ETag")
		w.Header().Set("Access-Control-Max-Age", "86400")
		w.Header().Set("Access-Control-Allow-Credentials", "false")

		// Handle CORS preflight requests (fix for Gajim "bad gateway" error)
		if r.Method == http.MethodOptions {
			w.WriteHeader(http.StatusOK)
			return
		}

		// Handle PUT requests for all upload protocols
		if r.Method == http.MethodPut {
			query := r.URL.Query()

			// Check if this is a v3 request (mod_http_upload_external)
			if query.Get("v3") != "" && query.Get("expires") != "" {
				handleV3Upload(w, r)
				return
			}

			// Check if this is a legacy protocol request (v, v2, token)
			if query.Get("v") != "" || query.Get("v2") != "" || query.Get("token") != "" {
				handleLegacyUpload(w, r)
				return
			}

			// Handle regular PUT uploads (non-XMPP) - route to general upload handler
			handlePutUpload(w, r)
			return
		}

		// Handle GET/HEAD requests for downloads
		if r.Method == http.MethodGet || r.Method == http.MethodHead {
			// Only handle download requests if the path looks like a file
			path := strings.TrimPrefix(r.URL.Path, "/")
			if path != "" && !strings.HasSuffix(path, "/") {
				handleLegacyDownload(w, r)
				return
			}
		}

		// For all other requests, return 404
		http.NotFound(w, r)
	})

	log.Info("HTTP router configured successfully with full protocol support (v, v2, token, v3)")

	return mux
}

// setupGracefulShutdown sets up graceful shutdown
func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		<-sigChan
		log.Info("Received shutdown signal, initiating graceful shutdown...")

		// Cancel context
		cancel()

		// Shutdown server with timeout
		ctx, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()

		if err := server.Shutdown(ctx); err != nil {
			log.Errorf("Server shutdown error: %v", err)
		} else {
			log.Info("Server shutdown completed")
		}

		// Clean up PID file
		if conf.Server.CleanUponExit {
			removePIDFile(conf.Server.PIDFilePath)
		}

		// Stop worker pool if it exists
		if workerPool != nil {
			workerPool.Stop()
			log.Info("Worker pool stopped")
		}

		os.Exit(0)
	}()
}

// ProgressWriter wraps an io.Writer to provide upload progress reporting
type ProgressWriter struct {
	dst        io.Writer
	total      int64
	written    int64
	filename   string
	onProgress func(written, total int64, filename string)
	lastReport time.Time
}

// NewProgressWriter creates a new ProgressWriter
func NewProgressWriter(dst io.Writer, total int64, filename string) *ProgressWriter {
	return &ProgressWriter{
		dst:      dst,
		total:    total,
		filename: filename,
		onProgress: func(written, total int64, filename string) {
			if total > 0 {
				percentage := float64(written) / float64(total) * 100
				sizeMiB := float64(written) / (1024 * 1024)
				totalMiB := float64(total) / (1024 * 1024)
				log.Infof("Upload progress for %s: %.1f%% (%.1f/%.1f MiB)",
					filepath.Base(filename), percentage, sizeMiB, totalMiB)
			}
		},
		lastReport: time.Now(),
	}
}

// Write implements io.Writer interface with progress reporting
func (pw *ProgressWriter) Write(p []byte) (int, error) {
	n, err := pw.dst.Write(p)
	if err != nil {
		return n, err
	}

	pw.written += int64(n)

	// Report progress every 30 seconds or every 50MB for large files
	now := time.Now()
	shouldReport := false

	if pw.total > 100*1024*1024 { // Files larger than 100MB
		shouldReport = now.Sub(pw.lastReport) > 30*time.Second ||
			(pw.written%(50*1024*1024) == 0 && pw.written > 0)
	} else if pw.total > 10*1024*1024 { // Files larger than 10MB
		shouldReport = now.Sub(pw.lastReport) > 10*time.Second ||
			(pw.written%(10*1024*1024) == 0 && pw.written > 0)
	}

	if shouldReport && pw.onProgress != nil {
		pw.onProgress(pw.written, pw.total, pw.filename)
		pw.lastReport = now
	}

	return n, err
}

// copyWithProgress copies data from src to dst with progress reporting
func copyWithProgress(dst io.Writer, src io.Reader, total int64, filename string) (int64, error) {
	progressWriter := NewProgressWriter(dst, total, filename)

	// Use a pooled buffer for efficient copying
	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)
	buf := *bufPtr

	return io.CopyBuffer(progressWriter, src, buf)
}

// handlePutUpload handles regular PUT uploads (non-XMPP protocol)
func handlePutUpload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Rate limiting check
	if rl := GetRateLimiter(); rl != nil && !rl.Allow(r) {
		log.Warnf("⛔ Rate limit exceeded for PUT upload: ip=%s", getClientIP(r))
		AuditRateLimited(r, "", "put_upload_rate_exceeded")
		w.Header().Set("Retry-After", "60")
		http.Error(w, "Rate limit exceeded", http.StatusTooManyRequests)
		uploadErrorsTotal.Inc()
		return
	}

	// Only allow PUT method
	if r.Method != http.MethodPut {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		uploadErrorsTotal.Inc()
		return
	}

	// Authentication - same as handleUpload
	if conf.Security.EnableJWT {
		_, err := validateJWTFromRequest(r, conf.Security.JWTSecret)
		if err != nil {
			http.Error(w, fmt.Sprintf("JWT Authentication failed: %v", err), http.StatusUnauthorized)
			uploadErrorsTotal.Inc()
			return
		}
		log.Debugf("JWT authentication successful for PUT upload request: %s", r.URL.Path)
	} else {
		err := validateHMAC(r, conf.Security.Secret)
		if err != nil {
			http.Error(w, fmt.Sprintf("HMAC Authentication failed: %v", err), http.StatusUnauthorized)
			uploadErrorsTotal.Inc()
			return
		}
		log.Debugf("HMAC authentication successful for PUT upload request: %s", r.URL.Path)
	}

	// Extract filename from URL path
	originalFilename := strings.TrimPrefix(r.URL.Path, "/")
	if originalFilename == "" {
		http.Error(w, "Filename required in URL path", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	// Validate file size against max_upload_size if configured
	if conf.Server.MaxUploadSize != "" && r.ContentLength > 0 {
		maxSizeBytes, err := parseSize(conf.Server.MaxUploadSize)
		if err != nil {
			log.Errorf("Invalid max_upload_size configuration: %v", err)
			http.Error(w, "Server configuration error", http.StatusInternalServerError)
			uploadErrorsTotal.Inc()
			return
		}
		if r.ContentLength > maxSizeBytes {
			http.Error(w, fmt.Sprintf("File size %s exceeds maximum allowed size %s",
				formatBytes(r.ContentLength), conf.Server.MaxUploadSize), http.StatusRequestEntityTooLarge)
			uploadErrorsTotal.Inc()
			return
		}
	}

	// Validate file extension if configured
	if len(conf.Uploads.AllowedExtensions) > 0 {
		ext := strings.ToLower(filepath.Ext(originalFilename))
		allowed := false
		for _, allowedExt := range conf.Uploads.AllowedExtensions {
			if ext == allowedExt {
				allowed = true
				break
			}
		}
		if !allowed {
			http.Error(w, fmt.Sprintf("File extension %s not allowed", ext), http.StatusBadRequest)
			uploadErrorsTotal.Inc()
			return
		}
	}

	// Generate filename based on configuration
	var filename string
	switch conf.Server.FileNaming {
	case "HMAC":
		// Generate HMAC-based filename
		h := hmac.New(sha256.New, []byte(conf.Security.Secret))
		h.Write([]byte(originalFilename + time.Now().String()))
		filename = hex.EncodeToString(h.Sum(nil)) + filepath.Ext(originalFilename)
	default: // "original" or "None"
		filename = originalFilename
	}

	// Create the file path
	filePath := filepath.Join(conf.Server.StoragePath, filename)

	// Create the directory if it doesn't exist
	if err := os.MkdirAll(filepath.Dir(filePath), 0755); err != nil {
		log.Errorf("Failed to create directory: %v", err)
		http.Error(w, "Failed to create directory", http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}

	// Create the file
	dst, err := os.Create(filePath)
	if err != nil {
		log.Errorf("Failed to create file %s: %v", filePath, err)
		http.Error(w, "Failed to create file", http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}
	defer dst.Close()

	// Copy data from request body to file
	written, err := io.Copy(dst, r.Body)
	if err != nil {
		log.Errorf("Failed to write file %s: %v", filePath, err)
		http.Error(w, "Failed to write file", http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}

	// Create response
	response := map[string]interface{}{
		"message":  "File uploaded successfully",
		"filename": filename,
		"size":     written,
		"url":      fmt.Sprintf("/download/%s", filename),
	}

	// Return success response
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)

	if err := json.NewEncoder(w).Encode(response); err != nil {
		log.Errorf("Failed to encode response: %v", err)
	}

	// Record metrics
	requestDuration := time.Since(startTime)
	uploadDuration.Observe(requestDuration.Seconds())
	uploadsTotal.Inc()

	// Record in metadata store
	if ms := GetMetadataStore(); ms != nil {
		ct := GetContentType(filename)
		_ = ms.RecordUpload(r.Context(), filePath, originalFilename, written, ct, "", getClientIP(r), "")
	}

	log.Infof("PUT upload completed: %s (%d bytes) in %v", filename, written, requestDuration)
}
