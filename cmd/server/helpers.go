package main

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
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
	workers    int
	taskQueue  chan UploadTask
	scanQueue  chan ScanTask
	ctx        context.Context
	cancel     context.CancelFunc
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
		log.Println("File TTL is disabled.")
		return
	}

	ttlDuration, err := parseTTL(conf.Server.FileTTL)
	if err != nil {
		log.Fatalf("Invalid TTL configuration: %v", err)
	}

	log.Printf("TTL cleanup enabled. Files older than %v will be deleted.", ttlDuration)
	ticker := time.NewTicker(24 * time.Hour)
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
	checksum, err := computeSHA256(ctx, absFilename)
	if err != nil {
		return err
	}

	dedupDir := conf.Deduplication.Directory
	if dedupDir == "" {
		return fmt.Errorf("deduplication directory not configured")
	}

	dedupPath := filepath.Join(dedupDir, checksum)
	if err := os.MkdirAll(dedupPath, os.ModePerm); err != nil {
		return err
	}

	existingPath := filepath.Join(dedupPath, filepath.Base(absFilename))
	if _, err := os.Stat(existingPath); err == nil {
		return os.Link(existingPath, absFilename)
	}

	if err := os.Rename(absFilename, existingPath); err != nil {
		return err
	}

	return os.Link(existingPath, absFilename)
}

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

// Stub for deleteOldFiles
func deleteOldFiles(conf *Config, ttlDuration time.Duration) {
	// TODO: Implement actual file deletion logic based on TTL
	log.Infof("deleteOldFiles is a stub and not yet implemented. It would check for files older than %v.", ttlDuration)
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

	result, err := clamClient.ScanFile(filename)
	if err != nil {
		return fmt.Errorf("ClamAV scan failed: %w", err)
	}

	// Handle the result channel
	if result != nil {
		select {
		case scanResult := <-result:
			if scanResult != nil && scanResult.Status != "OK" {
				return fmt.Errorf("virus detected in %s: %s", filename, scanResult.Status)
			}
		case <-time.After(30 * time.Second):
			return fmt.Errorf("ClamAV scan timeout for file: %s", filename)
		}
	}

	log.Debugf("File %s passed ClamAV scan", filename)
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
						Type:    "interface_up",
						Details: fmt.Sprintf("Interface %s is up", iface.Name),
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
func handleNetworkEvents(ctx context.Context) {
	log.Info("Starting network event handler")
	
	for {
		select {
		case <-ctx.Done():
			log.Info("Network event handler stopped")
			return
		case event := <-networkEvents:
			log.Debugf("Network event: %s - %s", event.Type, event.Details)
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
	
	mux.HandleFunc("/upload", handleUpload)
	mux.HandleFunc("/download/", handleDownload)
	mux.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("OK"))
	})

	if conf.Server.MetricsEnabled {
		mux.Handle("/metrics", promhttp.Handler())
	}

	// Catch-all handler for all upload protocols (v, v2, token, v3)
	// This must be added last as it matches all paths
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
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
