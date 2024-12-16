package main

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"flag"
	"fmt"
	"io"
	"mime"
	"net"
	"net/http"
	"net/url"
	"os"
	"os/signal"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"syscall"
	"time"

	"github.com/PlusOne/hmac-file-server/config"
	"github.com/PlusOne/hmac-file-server/metrics"
	"github.com/PlusOne/hmac-file-server/workers"
	"github.com/go-redis/redis/v8"
	"github.com/patrickmn/go-cache"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/disk"
	"github.com/shirou/gopsutil/host"
	"github.com/shirou/gopsutil/mem"
	"github.com/sirupsen/logrus"
)

var (
	conf              config.Config
	log               = logrus.New()
	versionString     = "2.1-dev"
	uploadQueue       chan workers.UploadTask
	scanQueue         chan workers.ScanTask
	networkEvents     chan NetworkEvent
	fileInfoCache     *cache.Cache
	clamClient        *workers.ClamAVClient
	redisClient       *redis.Client
	uploadErrorsTotal = prometheus.NewCounter(
		prometheus.CounterOpts{
			Name: "upload_errors_total",
			Help: "Total number of upload errors",
		},
	)
)

type NetworkEvent struct {
	EventType string
	Timestamp time.Time
}

func init() {
	// Register Prometheus metrics
	// prometheus.MustRegister(uploadErrorsTotal)
}

func initClamAV(socket string) (*workers.ClamAVClient, error) {
	if socket == "" {
		return nil, errors.New("ClamAV socket path is empty")
	}

	logrus.Printf("Initializing ClamAV client with socket: %s", socket)
	return &workers.ClamAVClient{Socket: socket}, nil
}

// corsMiddleware handles CORS by setting appropriate headers
func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}
		next.ServeHTTP(w, r)
	})
}

func setupRouter() *http.ServeMux {
	router := http.NewServeMux()
	router.HandleFunc("/upload", handleUploadWrapper)
	router.HandleFunc("/request", handleRequest) // Add this line to use handleRequest
	// Define additional routes here
	return router
}

func setDefaults() {
	conf.Server = config.ServerConfig{
		StoragePath:    "./storage",
		FileTTL:        "24h",
		MetricsEnabled: true,
		MetricsPort:    "2112",
		UnixSocket:     false,
		LogFile:        "./logs/server.log",
		LogLevel:       "info",
		// ResumableUploads:       true,
	}

	conf.ISO = config.ISOConfig{
		Enabled: false,
	}

	conf.Timeouts = config.TimeoutConfig{
		ReadTimeout:  "10s",
		WriteTimeout: "10s",
		IdleTimeout:  "60s",
	}

	conf.Workers = config.WorkersConfig{
		UploadQueueSize: 10,
		NumWorkers:      4,
		NumScanWorkers:  4,
	}

	conf.ClamAV = config.ClamAVConfig{
		ClamAVEnabled: false,
		ClamAVSocket:  "/var/run/clamav/clamd.ctl",
	}

	conf.Redis = config.RedisConfig{
		RedisEnabled:             false,
		RedisHealthCheckInterval: "30s",
	}

	conf.Security = config.SecurityConfig{
		// SecretKey: "your-secret-key",
	}
}

func readConfig(configFile string, conf *config.Config) error {
	var err error
	confPtr, err := config.LoadConfig(configFile)
	if err != nil {
		return err
	}
	*conf = *confPtr
	return err
}

func getFileInfo(filePath string) (os.FileInfo, error) {
	// Check if file info is cached
	if info, found := fileInfoCache.Get(filePath); found {
		logrus.Infof("Cache hit for file: %s", filePath)
		return info.(os.FileInfo), nil
	}

	// Cache miss, read file info
	info, err := os.Stat(filePath)
	if err != nil {
		return nil, err
	}
	// Store the file info in the cache with a TTL (e.g., 5 minutes)
	fileInfoCache.Set(filePath, info, cache.DefaultExpiration)
	logrus.Infof("Cache miss for file: %s, reading from disk", filePath)

	return info, nil
}

func cleanupOldFiles(storagePath string, ttl time.Duration) {
	files, err := os.ReadDir(storagePath)
	if err != nil {
		logrus.Errorf("Error reading storage directory: %v", err)
		return
	}

	cutoff := time.Now().Add(-ttl)
	for _, file := range files {
		filePath := filepath.Join(storagePath, file.Name())

		// Use the cached file info or read it if not cached
		info, err := getFileInfo(filePath)
		if err != nil {
			logrus.Errorf("Error stating file %s: %v", filePath, err)
			continue
		}

		// Check if the file's modification time is older than the TTL
		if info.ModTime().Before(cutoff) {
			err := os.Remove(filePath)
			if err != nil {
				logrus.Errorf("Error removing file %s: %v", filePath, err)
			} else {
				logrus.Infof("Removed old file: %s", filePath)
			}
		}
	}
}

func checkFreeSpaceWithRetry(path string, retries int, delay time.Duration) error {
	for i := 0; i < retries; i++ {
		if checkFreeSpace(path) {
			return nil
		}
		time.Sleep(delay)
	}
	return fmt.Errorf("insufficient free space after %d retries", retries)
}

func checkFreeSpace(path string) bool {
	var stat syscall.Statfs_t
	if err := syscall.Statfs(path, &stat); err != nil {
		logrus.Errorf("Error getting filesystem stats: %v", err)
		return false
	}
	freeSpace := stat.Bavail * uint64(stat.Bsize)
	requiredSpace := uint64(1 << 30) // 1 GB
	return freeSpace > requiredSpace
}

func parseCustomDuration(durationStr string) (time.Duration, error) {
	re := regexp.MustCompile(`^(\d+)([HhDdMmYySsMm])$`)
	matches := re.FindStringSubmatch(durationStr)
	if len(matches) != 3 {
		return 0, errors.New("invalid duration format")
	}

	value, err := strconv.Atoi(matches[1])
	if err != nil {
		return 0, err
	}

	unit := matches[2]
	switch unit {
	case "H", "h":
		return time.Duration(value) * time.Hour, nil
	case "D", "d":
		return time.Duration(value) * 24 * time.Hour, nil
	case "M", "m":
		return time.Duration(value) * time.Minute, nil
	case "Y", "y":
		return time.Duration(value) * 365 * 24 * time.Hour, nil
	case "S", "s":
		return time.Duration(value) * time.Second, nil
	default:
		return 0, errors.New("unknown duration unit")
	}
}

func logSystemInfo() {
	logrus.Info("========================================")
	logrus.Infof("       HMAC File Server - %s          ", versionString)
	logrus.Info("  Secure File Handling with HMAC Auth   ")
	logrus.Info("========================================")

	logrus.Info("Features: Prometheus Metrics, Chunked Uploads, ClamAV Scanning")
	logrus.Info("Build Date: 2024-10-28")

	logrus.Infof("Operating System: %s", runtime.GOOS)
	logrus.Infof("Architecture: %s", runtime.GOARCH)
	logrus.Infof("Number of CPUs: %d", runtime.NumCPU())
	logrus.Infof("Go Version: %s", runtime.Version())

	v, _ := mem.VirtualMemory()
	logrus.Infof("Total Memory: %v MB", v.Total/1024/1024)
	logrus.Infof("Free Memory: %v MB", v.Free/1024/1024)
	logrus.Infof("Used Memory: %v MB", v.Used/1024/1024)

	cpuInfo, _ := cpu.Info()
	for _, info := range cpuInfo {
		logrus.Infof("CPU Model: %s, Cores: %d, Mhz: %f", info.ModelName, info.Cores, info.Mhz)
	}

	partitions, _ := disk.Partitions(false)
	for _, partition := range partitions {
		usage, _ := disk.Usage(partition.Mountpoint)
		logrus.Infof("Disk Mountpoint: %s, Total: %v GB, Free: %v GB, Used: %v GB",
			partition.Mountpoint, usage.Total/1024/1024/1024, usage.Free/1024/1024/1024, usage.Used/1024/1024/1024)
	}

	hInfo, _ := host.Info()
	logrus.Infof("Hostname: %s", hInfo.Hostname)
	logrus.Infof("Uptime: %s", formatUptime(hInfo.Uptime))
	logrus.Infof("Boot Time: %v", time.Unix(int64(hInfo.BootTime), 0))
	logrus.Infof("Platform: %s", hInfo.Platform)
	logrus.Infof("Platform Family: %s", hInfo.PlatformFamily)
	logrus.Infof("Platform Version: %s", hInfo.PlatformVersion)
	logrus.Infof("Kernel Version: %s", hInfo.KernelVersion)
}

func formatUptime(seconds uint64) string {
	days := seconds / (24 * 3600)
	seconds %= 24 * 3600
	hours := seconds / 3600
	seconds %= 3600
	minutes := seconds / 60
	seconds %= 60
	return fmt.Sprintf("%d days, %d hours, %d minutes, %d seconds", days, hours, minutes, seconds)
}

func setupLogging() {
	// Parse and set log level
	level, err := logrus.ParseLevel(conf.Server.LogLevel)
	if err != nil {
		logrus.Fatalf("Invalid log level: %s", conf.Server.LogLevel)
	}
	logrus.SetLevel(level)

	// Configure log output
	if conf.Server.LogFile != "" {
		// Open the log file
		logFile, err := os.OpenFile(conf.Server.LogFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			logrus.Fatalf("Failed to open log file %s: %v", conf.Server.LogFile, err)
		}
		// Set log output to both stdout and the log file
		logrus.SetOutput(io.MultiWriter(os.Stdout, logFile))
	} else {
		// If no log file is specified, log only to stdout
		logrus.SetOutput(os.Stdout)
	}

	// Use Text formatter for human-readable logs
	logrus.SetFormatter(&logrus.TextFormatter{
		FullTimestamp:   true,
		TimestampFormat: "2006-01-02 15:04:05",
	})
}

func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt)
	go func() {
		<-c
		logrus.Info("Shutting down server...")
		if err := server.Shutdown(context.Background()); err != nil {
			logrus.Fatalf("Server Shutdown Failed:%+v", err)
		}
		cancel()
	}()
}

func verifyAndCreateISOContainer() error {
	// Check if the ISO container exists
	isoPath := "/path/to/iso/container" // Replace with actual ISO path

	if _, err := os.Stat(isoPath); os.IsNotExist(err) {
		logrus.Infof("ISO container not found, creating new ISO container at %s", isoPath)
		// Add logic to create the ISO container
		// For example, you could use an external tool or library to create the ISO
		// Return an error if the creation fails
		return fmt.Errorf("failed to create ISO container at %s", isoPath)
	}

	logrus.Infof("ISO container already exists at %s", isoPath)
	return nil
}

// Monitor network events
func monitorNetwork(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			return
		default:
			networkEvents <- NetworkEvent{
				EventType: "NETWORK_CHANGE",
				Timestamp: time.Now(),
			}
			time.Sleep(10 * time.Second)
		}
	}
}

// Handle network events
func handleNetworkEvents(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			return
		case event := <-networkEvents:
			logrus.Infof("Handling network event: %s at %s", event.EventType, event.Timestamp)
		}
	}
}

// Run file cleaner
func runFileCleaner(ctx context.Context, storagePath string, ttl time.Duration) {
	ticker := time.NewTicker(ttl)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			cleanupOldFiles(storagePath, ttl)
		}
	}
}

func updateSystemMetrics(ctx context.Context) {
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			// Update system metrics here
			logrus.Info("Updating system metrics...")
		}
	}
}

func initMetrics() {
	// Initialize Prometheus metrics here
	metrics.InitMetrics()
	logrus.Info("Prometheus metrics initialized.")
}

func initRedis() {
	// Initialize Redis client here
	logrus.Info("Initializing Redis client...")
	redisClient = redis.NewClient(&redis.Options{
		Addr:     conf.Redis.RedisAddr,
		Password: conf.Redis.RedisPassword,
		DB:       conf.Redis.RedisDBIndex,
	})

	// Optionally, test Redis connection
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := redisClient.Ping(ctx).Err(); err != nil {
		logrus.Errorf("Failed to connect to Redis: %v", err)
		conf.Redis.RedisEnabled = false
	} else {
		logrus.Info("Connected to Redis successfully.")
	}
}

func initializeUploadWorkerPool(ctx context.Context) {
	for i := 0; i < conf.Workers.NumWorkers; i++ {
		go workers.UploadWorker(ctx, uploadQueue)
	}
	logrus.Infof("Initialized %d upload workers", conf.Workers.NumWorkers)
}

func initializeScanWorkerPool(ctx context.Context) {
	for i := 0; i < conf.Workers.NumScanWorkers; i++ {
		go workers.ScanWorker(ctx, scanQueue)
	}
	logrus.Infof("Initialized %d scan workers", conf.Workers.NumScanWorkers)
}

// Monitor Redis health
func MonitorRedisHealth(ctx context.Context, client *redis.Client, interval time.Duration) {
	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			status := client.Ping(ctx)
			if status.Err() != nil {
				logrus.Errorf("Redis health check failed: %v", status.Err())
			} else {
				logrus.Info("Redis health check passed")
			}
		}
	}
}

// Check if the file extension is allowed
func isExtensionAllowed(filePath string) bool {
	allowedExtensions := []string{".txt", ".pdf", ".jpg", ".png"} // Define allowed extensions
	ext := filepath.Ext(filePath)
	for _, allowed := range allowedExtensions {
		if ext == allowed {
			return true
		}
	}
	return false
}

// Wrapper for handleUpload to match handler signature
func handleUploadWrapper(w http.ResponseWriter, r *http.Request) {
	absFilename := r.URL.Query().Get("filename") // Retrieve absolute filename from query parameters
	queryParams := r.URL.Query()
	handleUpload(w, r, absFilename, queryParams)
}

// Handle file uploads with extension restrictions and HMAC validation
func handleUpload(w http.ResponseWriter, r *http.Request, fileStorePath string, a url.Values) {
	if !isExtensionAllowed(fileStorePath) {
		logrus.Warnf("Disallowed file extension for file: %s", fileStorePath)
		http.Error(w, "Disallowed file extension", http.StatusForbidden)
		uploadErrorsTotal.Inc()
		return
	}
	logrus.Debug("handleUpload called")
	logrus.Debugf("Request method: %s", r.Method)
	logrus.Debugf("File store path: %s", fileStorePath)
	logrus.Debugf("Query parameters: %v", a)

	// Log the storage path being used
	logrus.Infof("Using storage path: %s", conf.Server.StoragePath)

	// Determine protocol version based on query parameters
	var protocolVersion string
	if a.Get("v2") != "" {
		protocolVersion = "v2"
	} else if a.Get("token") != "" {
		protocolVersion = "token"
	} else if a.Get("v") != "" {
		protocolVersion = "v"
	} else {
		logrus.Warn("No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC")
		http.Error(w, "No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC", http.StatusForbidden)
		uploadErrorsTotal.Inc()
		return
	}
	logrus.Debugf("Protocol version determined: %s", protocolVersion)

	// Initialize HMAC
	mac := hmac.New(sha256.New, []byte(conf.Security.Secret))
	macString := ""

	// Calculate MAC based on protocolVersion
	if protocolVersion == "v" {
		mac.Write([]byte(fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)))
		macString = hex.EncodeToString(mac.Sum(nil))
	} else if protocolVersion == "v2" || protocolVersion == "token" {
		contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
		if contentType == "" {
			contentType = "application/octet-stream"
		}
		mac.Write([]byte(fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10) + "\x00" + contentType))
		macString = hex.EncodeToString(mac.Sum(nil))
	}

	logrus.Debugf("Calculated MAC: %s", macString)

	// Validate the HMAC
	if hmac.Equal([]byte(macString), []byte(a.Get(protocolVersion))) {
		err := createFile(fileStorePath, w, r)
		if err != nil {
			logrus.Error(err)
		}
		return
	} else {
		logrus.Warn("Invalid MAC.")
		http.Error(w, "Invalid MAC", http.StatusForbidden)
		uploadErrorsTotal.Inc()
		return
	}
}

func createFile(fileStorePath string, w http.ResponseWriter, r *http.Request) error {
	// Make sure the directory path exists
	absDirectory := filepath.Dir(fileStorePath)
	err := os.MkdirAll(absDirectory, os.ModePerm)
	if err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return fmt.Errorf("failed to create directory %s: %s", absDirectory, err)
	}

	// Make sure the target file exists (MUST NOT exist before! -> O_EXCL)
	targetFile, err := os.OpenFile(fileStorePath, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0644)
	if err != nil {
		http.Error(w, "Conflict", http.StatusConflict)
		return fmt.Errorf("failed to create file %s: %s", fileStorePath, err)
	}
	defer targetFile.Close()

	// Copy file contents to file
	_, err = io.Copy(targetFile, r.Body)
	if err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return fmt.Errorf("failed to copy file contents to %s: %s", fileStorePath, err)
	}

	w.WriteHeader(http.StatusCreated)
	return nil
}

func handleRequest(w http.ResponseWriter, r *http.Request) {
	// Existing code...

	queryParams := r.URL.Query()
	switch r.Method {
	case http.MethodPut, http.MethodPost:
		absFilename := queryParams.Get("filename")
		handleUpload(w, r, absFilename, queryParams)
	case http.MethodHead, http.MethodGet:
		absFilename := queryParams.Get("filename")
		handleDownload(w, r, absFilename)
	case http.MethodOptions:
		w.Header().Set("Allow", "OPTIONS, GET, PUT, POST, HEAD")
		return
	default:
		http.Error(w, "Method Not Allowed", http.StatusMethodNotAllowed)
		return
	}
}

func handleDownload(w http.ResponseWriter, r *http.Request, fileStorePath string) {
	// Implement the download logic here
	http.ServeFile(w, r, fileStorePath)
}

func main() {
	logSystemInfo()
	// Set default configuration values
	setDefaults()

	// Flags for configuration file
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	// Load configuration
	err := readConfig(configFile, &conf)
	if err != nil {
		log.Fatalf("Error reading config: %v", err)
	}
	log.Info("Configuration loaded successfully.")

	// Setup logging
	setupLogging()
	log.Info("Logging initialized.")

	// Initialize Prometheus metrics once
	metrics.InitMetrics()

	// Start metrics server if enabled
	if conf.Server.MetricsEnabled {
		go func() {
			metricsMux := http.NewServeMux()
			metricsMux.Handle("/metrics", promhttp.Handler())

			metricsServer := &http.Server{
				Addr:    ":" + conf.Server.MetricsPort,
				Handler: metricsMux,
			}

			log.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
			if err := metricsServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
				log.Fatalf("Metrics server failed: %v", err)
			}
		}()
	}

	// Initialize the conf variable
	conf = config.Config{}

	// Set default configuration values
	setDefaults()

	// Create store directory
	err = os.MkdirAll(conf.Server.StoragePath, os.ModePerm)
	if err != nil {
		logrus.Fatalf("Failed to create storage directory: %v", err)
	}
	logrus.Infof("Storage directory '%s' is ready", conf.Server.StoragePath)

	// Check free space with retry
	err = checkFreeSpaceWithRetry(conf.Server.StoragePath, 3, 5*time.Second)
	if err != nil {
		logrus.Fatalf("Insufficient free space: %v", err)
	}

	// Initialize Prometheus metrics
	initMetrics()
	logrus.Info("Prometheus metrics initialized.")

	// Initialize upload and scan queues
	uploadQueue = make(chan workers.UploadTask, conf.Workers.UploadQueueSize)
	logrus.Infof("Upload queue initialized with size: %d", conf.Workers.UploadQueueSize)
	scanQueue = make(chan workers.ScanTask, conf.Workers.UploadQueueSize)
	networkEvents = make(chan NetworkEvent, 100)
	logrus.Info("Upload, scan, and network event channels initialized.")

	// Context for goroutines
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Start network monitoring if enabled
	if conf.Server.NetworkChangeMonitoring {
		go monitorNetwork(ctx)
		go handleNetworkEvents(ctx)
	}

	// Update system metrics
	go updateSystemMetrics(ctx)

	// Initialize ClamAV client if enabled
	if conf.ClamAV.ClamAVEnabled {
		clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			logrus.WithFields(logrus.Fields{
				"error": err.Error(),
			}).Warn("Failed to initialize ClamAV client. Continuing without ClamAV.")
		} else {
			logrus.Info("ClamAV client initialized successfully.")
		}
	}

	// Verify and create ISO container if needed
	err = verifyAndCreateISOContainer()
	if err != nil {
		logrus.Warnf("Failed to verify or create ISO container: %v", err)
	}

	// Initialize Redis client if enabled
	if conf.Redis.RedisEnabled {
		initRedis()
	}

	// Dynamically adjust workers if AutoAdjustWorkers is enabled
	if conf.Server.AutoAdjustWorkers {
		conf.Workers.NumWorkers = runtime.NumCPU()
		conf.Workers.NumScanWorkers = runtime.NumCPU()
		logrus.Infof("AutoAdjustWorkers enabled: NumWorkers set to %d, NumScanWorkers set to %d", conf.Workers.NumWorkers, conf.Workers.NumScanWorkers)
	}

	// Initialize worker pools
	initializeUploadWorkerPool(ctx)
	if conf.ClamAV.ClamAVEnabled && clamClient != nil {
		initializeScanWorkerPool(ctx)
	}

	// Start Redis health monitor if Redis is enabled
	if conf.Redis.RedisEnabled && redisClient != nil {
		redisHealthCheckInterval, err := parseCustomDuration(conf.Redis.RedisHealthCheckInterval)
		if err != nil {
			logrus.Fatalf("Invalid RedisHealthCheckInterval: %v", err)
		}
		go MonitorRedisHealth(ctx, redisClient, redisHealthCheckInterval)
	}

	// Setup router
	// router := setupRouter()

	// Start file cleaner
	fileTTL, err := parseCustomDuration(conf.Server.FileTTL)
	if err != nil {
		logrus.Warnf("Invalid duration '%s', defaulting to 30s", conf.Server.FileTTL)
		fileTTL = 30 * time.Second
	}
	go runFileCleaner(ctx, conf.Server.StoragePath, fileTTL)

	// Parse timeout durations
	readTimeout, err := parseCustomDuration(conf.Timeouts.ReadTimeout)
	if err != nil {
		logrus.Fatalf("Invalid ReadTimeout: %v", err)
	}

	writeTimeout, err := parseCustomDuration(conf.Timeouts.WriteTimeout)
	if err != nil {
		logrus.Fatalf("Invalid WriteTimeout: %v", err)
	}

	idleTimeout, err := parseCustomDuration(conf.Timeouts.IdleTimeout)
	if err != nil {
		logrus.Fatalf("Invalid IdleTimeout: %v", err)
	}

	// Configure HTTP server
	server := &http.Server{
		Addr:         ":" + conf.Server.ListenPort, // Prepend colon to ListenPort
		Handler:      corsMiddleware(setupRouter()),
		ReadTimeout:  readTimeout,
		WriteTimeout: writeTimeout,
		IdleTimeout:  idleTimeout,
	}

	// Start metrics server if enabled
	if conf.Server.MetricsEnabled {
		go func() {
			metricsMux := http.NewServeMux()
			metricsMux.Handle("/metrics", promhttp.Handler())

			metricsServer := &http.Server{
				Addr:    ":" + conf.Server.MetricsPort,
				Handler: metricsMux,
			}

			logrus.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
			if err := metricsServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
				logrus.Fatalf("Metrics server failed: %v", err)
			}
		}()
	}

	// Setup graceful shutdown
	setupGracefulShutdown(server, cancel)

	// Start server
	logrus.Infof("Starting HMAC file server %s...", versionString)
	if conf.Server.UnixSocket {
		// Listen on Unix socket
		if err := os.RemoveAll(conf.Server.ListenPort); err != nil {
			logrus.Fatalf("Failed to remove existing Unix socket: %v", err)
		}
		listener, err := net.Listen("unix", conf.Server.ListenPort)
		if err != nil {
			logrus.Fatalf("Failed to listen on Unix socket %s: %v", conf.Server.ListenPort, err)
		}
		defer listener.Close()
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			logrus.Fatalf("Server failed: %v", err)
		}
	} else {
		// Listen on TCP port
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			logrus.Fatalf("Server failed: %v", err)
		}
	}
}
