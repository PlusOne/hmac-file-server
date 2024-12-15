package metrics

import (
    "github.com/prometheus/client_golang/prometheus"
    "github.com/prometheus/client_golang/prometheus/promauto"
)

var (
    opsProcessed = promauto.NewCounter(prometheus.CounterOpts{
        Name: "myapp_processed_ops_total",
        Help: "The total number of processed events",
    })
)

func InitMetrics() {
    // Initialize your metrics here
}

func UpdateMetrics() {
    // Update your metrics here
    opsProcessed.Inc()
}
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"syscall"
	"time"

	"github.com/PlusOne/hmac-file-server/config"
	"github.com/PlusOne/hmac-file-server/handlers"
	"github.com/PlusOne/hmac-file-server/workers"
	"github.com/go-redis/redis/v8"
	"github.com/patrickmn/go-cache"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/disk"
	"github.com/shirou/gopsutil/host"
	"github.com/shirou/gopsutil/mem"
	"github.com/sirupsen/logrus"
)

var (
	fileInfoCache *cache.Cache
	uploadQueue   chan workers.UploadTask
	scanQueue     chan workers.ScanTask
	conf          *config.Config
	clamClient    *workers.ClamAVClient
	networkEvents chan NetworkEvent
	versionString = "2.1-dev"
)

type NetworkEvent struct {
	EventType string
	Timestamp time.Time
}

func initClamAV(socket string) (*workers.ClamAVClient, error) {
	if socket == "" {
		return nil, errors.New("ClamAV socket path is empty")
	}

	logrus.Printf("Initializing ClamAV client with socket: %s", socket)
	return &workers.ClamAVClient{Socket: socket}, nil
}

func setupRouter() *http.ServeMux {
	router := http.NewServeMux()
	if conf.Server.ResumeableUploads {
		router.HandleFunc("/upload", handlers.UploadHandler)
	}
	if conf.Server.ResumeableDownloads {
		router.HandleFunc("/download", handlers.DownloadHandler)
	}
	return router
}

func setDefaults() {
	conf.Server = config.ServerConfig{
		StoragePath:             "./storage",
		FileTTL:                 "24h",
		MetricsEnabled:          true,
		MetricsPort:             "2112",
		UnixSocket:              false,
		LogFile:                 "./logs/server.log",
		LogLevel:                "info",
		NetworkChangeMonitoring: true,
		ResumeableUploads:       true,
		ResumeableDownloads:     true,
		AutoAdjustWorkers:       true,
	}

	conf.ISO = config.ISOConfig{
		Enabled: false,
	}

	conf.Timeouts = config.TimeoutsConfig{
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
}

func readConfig(configFile string) error {
	var err error
	conf, err = config.LoadConfig(configFile)
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
	logrus.Info("Initializing Prometheus metrics...")
}

var redisClient *redis.Client

func initRedis() {
	// Initialize Redis client here
	logrus.Info("Initializing Redis client...")
}

func initializeUploadWorkerPool(ctx context.Context) {
	for i := 0; i < conf.Workers.NumWorkers; i++ {
		go workers.UploadWorker(ctx, uploadQueue)
	}
}

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

func initializeScanWorkerPool(ctx context.Context) {
	for i := 0; i < conf.Workers.NumScanWorkers; i++ {
		go workers.ScanWorker(ctx, scanQueue)
	}
}

func main() {
	// Initialize the conf variable
	conf = &config.Config{}

	// Set default configuration values
	setDefaults()

	// Flags for configuration file
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	// Load configuration
	err := readConfig(configFile)
	if err != nil {
		logrus.Fatalf("Error reading configuration file: %v", err)
	}
	logrus.Info("Configuration loaded successfully.")

	// Setup logging
	setupLogging()

	// Log system information
	logSystemInfo()

	// Verify and create ISO container if it doesn't exist
	if conf.ISO.Enabled {
		err = verifyAndCreateISOContainer()
		if err != nil {
			logrus.Fatalf("Failed to verify or create ISO container: %v", err)
		}
	}

	// Initialize file info cache
	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)

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
	router := setupRouter()

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
		Handler:      router,
		ReadTimeout:  readTimeout,
		WriteTimeout: writeTimeout,
		IdleTimeout:  idleTimeout,
	}

	// Start metrics server if enabled
	if conf.Server.MetricsEnabled {
		go func() {
			http.Handle("/metrics", promhttp.Handler())
			logrus.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
			if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
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
package handlers

import (
    "io"
    "net/http"
    "os"
    "strconv"
    "strings"

    "github.com/sirupsen/logrus"
)

func DownloadHandler(w http.ResponseWriter, r *http.Request) {
    filePath := r.URL.Query().Get("file")
    if filePath == "" {
        http.Error(w, "File parameter is required", http.StatusBadRequest)
        return
    }

    file, err := os.Open(filePath)
    if err != nil {
        http.Error(w, "File not found", http.StatusNotFound)
        return
    }
    defer file.Close()

    fileInfo, err := file.Stat()
    if (err != nil) {
        http.Error(w, "Could not get file info", http.StatusInternalServerError)
        return
    }

    fileSize := fileInfo.Size()
    w.Header().Set("Content-Disposition", "attachment; filename="+fileInfo.Name())
    w.Header().Set("Accept-Ranges", "bytes")

    rangeHeader := r.Header.Get("Range")
    if rangeHeader == "" {
        http.ServeContent(w, r, fileInfo.Name(), fileInfo.ModTime(), file)
        return
    }

    rangeParts := strings.Split(rangeHeader, "=")
    if len(rangeParts) != 2 || rangeParts[0] != "bytes" {
        http.Error(w, "Invalid range header", http.StatusRequestedRangeNotSatisfiable)
        return
    }

    rangeSpec := strings.Split(rangeParts[1], "-")
    if len(rangeSpec) != 2 {
        http.Error(w, "Invalid range header", http.StatusRequestedRangeNotSatisfiable)
        return
    }

    start, err := strconv.ParseInt(rangeSpec[0], 10, 64)
    if err != nil {
        http.Error(w, "Invalid range start", http.StatusRequestedRangeNotSatisfiable)
        return
    }

    var end int64
    if rangeSpec[1] == "" {
        end = fileSize - 1
    } else {
        end, err = strconv.ParseInt(rangeSpec[1], 10, 64)
        if err != nil {
            http.Error(w, "Invalid range end", http.StatusRequestedRangeNotSatisfiable)
            return
        }
    }

    if start > end || start < 0 || end >= fileSize {
        http.Error(w, "Invalid range", http.StatusRequestedRangeNotSatisfiable)
        return
    }

    w.Header().Set("Content-Range", "bytes "+strconv.FormatInt(start, 10)+"-"+strconv.FormatInt(end, 10)+"/"+strconv.FormatInt(fileSize, 10))
    w.Header().Set("Content-Length", strconv.FormatInt(end-start+1, 10))
    w.WriteHeader(http.StatusPartialContent)

    _, err = file.Seek(start, 0)
    if err != nil {
        http.Error(w, "Could not seek file", http.StatusInternalServerError)
        return
    }

    _, err = io.CopyN(w, file, end-start+1)
    if err != nil {
        logrus.Errorf("Error serving file: %v", err)
    }
}package handlers

import (
    "io"
    "net/http"
    "os"
    "path/filepath"
    "time"

    "github.com/go-redis/redis/v8"
    "github.com/sirupsen/logrus"
    "github.com/PlusOne/hmac-file-server/config"
    "github.com/PlusOne/hmac-file-server/workers"
    "github.com/shirou/gopsutil/cpu"
    "github.com/dutchcoders/go-clamd"
    "golang.org/x/net/context"
)

var ctx = context.Background()

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

func UploadHandlerV2(w http.ResponseWriter, r *http.Request) {
    // Load configuration
    conf, err := config.LoadConfig("./config.toml")
    if err != nil {
        logrus.Fatalf("Error loading configuration: %v", err)
    }

    // Check configuration values
    if conf.Server.ListenPort == "8080" {
        logrus.Info("ListenPort is set to 8080.")
    } else {
        logrus.Infof("ListenPort is set to %s.", conf.Server.ListenPort)
    }

    // Gather CPU information
    cpuInfo, err := cpu.Info()
    if err != nil {
        logrus.Fatalf("Error gathering CPU information: %v", err)
    }

    // Log CPU information in a cumulative manner
    for _, info := range cpuInfo {
        logrus.Infof("CPU Model: %s, Cores: %d, Mhz: %f", info.ModelName, info.Cores, info.Mhz)
    }

    // Start a goroutine to log system metrics every 60 seconds if LogLimiter is enabled
    if conf.Server.LogLimiter {
        go func() {
            ticker := time.NewTicker(60 * time.Second) // Adjusted to 60 seconds
            defer ticker.Stop()
            for {
                select {
                case <-ticker.C:
                    logrus.Info("Updating system metrics...")
                    // Add your system metrics update logic here
                }
            }
        }()
    }

    // Implement the upload handler logic here
    file, header, err := r.FormFile("file")
    if err != nil {
        http.Error(w, "Failed to get file from request", http.StatusBadRequest)
        return
    }
    defer file.Close()

    filePath := filepath.Join(conf.Server.StoragePath, header.Filename)
    outFile, err := os.Create(filePath)
    if err != nil {
        http.Error(w, "Failed to create file", http.StatusInternalServerError)
        return
    }
    defer outFile.Close()

    _, err = io.Copy(outFile, file)
    if err != nil {
        http.Error(w, "Failed to save file", http.StatusInternalServerError)
        return
    }

    // Check if ClamAV is enabled and scan the file
    if conf.ClamAV.ClamAVEnabled {
        clamClient := clamd.NewClamd("unix:" + conf.ClamAV.ClamAVSocket)
        response, err := clamClient.ScanFile(filePath)
        if err != nil {
            logrus.Errorf("Error scanning file with ClamAV: %v", err)
            http.Error(w, "Failed to scan file", http.StatusInternalServerError)
            return
        }

        for result := range response {
            if result.Status == clamd.RES_FOUND {
                logrus.Warnf("ClamAV found a virus in the file: %s", result.Description)
                http.Error(w, "File contains a virus", http.StatusForbidden)
                return
            }
        }
    }

    // Check if Redis is enabled and perform Redis operations
    if conf.Redis.RedisEnabled {
        rdb := redis.NewClient(&redis.Options{
            Addr:     conf.Redis.RedisAddr,
            Password: conf.Redis.RedisPassword,
            DB:       conf.Redis.RedisDBIndex,
        })

        // Example Redis operation: Set a key with the file path
        err := rdb.Set(ctx, "uploaded_file:"+header.Filename, filePath, 0).Err()
        if err != nil {
            logrus.Errorf("Error setting Redis key: %v", err)
            http.Error(w, "Failed to set Redis key", http.StatusInternalServerError)
            return
        }

        // Example Redis operation: Get the key value
        val, err := rdb.Get(ctx, "uploaded_file:"+header.Filename).Result()
        if err != nil {
            logrus.Errorf("Error getting Redis key: %v", err)
            http.Error(w, "Failed to get Redis key", http.StatusInternalServerError)
            return
        }
        logrus.Infof("Redis key value: %s", val)
    }

    w.WriteHeader(http.StatusCreated)
    w.Write([]byte("Upload successful"))
}

func InitHandlers(uploadQueue chan workers.UploadTask, scanQueue chan workers.ScanTask, conf *config.Config) {
    mux := http.NewServeMux()
    mux.HandleFunc("/upload", UploadHandlerV2)
    handler := corsMiddleware(mux)
    http.ListenAndServe(":"+conf.Server.ListenPort, handler)
}

type Config struct {
    Server struct {
        ListenPort           string
        StoragePath          string
        DeduplicationEnabled bool
        MinFreeBytes         string
        LogLimiter           bool
    }
    Uploads struct {
        ChunkedUploadsEnabled bool
        ChunkSize             string
        AllowedExtensions     []string
    }
    Redis struct {
        RedisEnabled             bool
        RedisDBIndex             int
        RedisAddr                string
        RedisPassword            string
        RedisHealthCheckInterval string
    }
    File struct {
        FileRevision int
    }
    ClamAV struct {
        ClamAVEnabled bool
        ClamAVSocket  string
    }
}

// Example function where you might want to check the ListenPort value
package handlers

import (
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/sirupsen/logrus"
)

type UploadConfig struct {
	Server struct {
		StoragePath          string
		DeduplicationEnabled bool
		MinFreeBytes         string // Ensure this field is present
	}
	Uploads struct {
		ChunkedUploadsEnabled bool
		ChunkSize             string
		AllowedExtensions     []string
	}
	Redis struct {
		RedisEnabled             bool
		RedisDBIndex             int
		RedisAddr                string
		RedisPassword            string
		RedisHealthCheckInterval string
	}
	File struct {
		FileRevision int
	}
}

var conf *UploadConfig

func UploadHandler(w http.ResponseWriter, r *http.Request) {
	if !conf.Uploads.ChunkedUploadsEnabled {
		http.Error(w, "Chunked uploads are not enabled", http.StatusForbidden)
		return
	}

	if conf.Server.DeduplicationEnabled {
		logrus.Info("Deduplication is enabled.")
		// Add your deduplication logic here
	} else {
		logrus.Info("Deduplication is not enabled.")
	}

	if strings.ToUpper(conf.Server.MinFreeBytes) == "100GB" {
		logrus.Info("MinFreeBytes is set to 100GB.")
		// Add your logic here
	} else {
		logrus.Infof("MinFreeBytes is set to %s.", conf.Server.MinFreeBytes)
	}

	if conf.File.FileRevision == 1 {
		logrus.Info("FileRevision is set to 1.")
		// Add your logic here
	} else {
		logrus.Infof("FileRevision is set to %d.", conf.File.FileRevision)
	}

	if !conf.Redis.RedisEnabled {
		logrus.Info("Redis is disabled.")
	} else {
		logrus.Infof("Redis is enabled. Addr: %s, DBIndex: %d, HealthCheckInterval: %s",
			conf.Redis.RedisAddr, conf.Redis.RedisDBIndex, conf.Redis.RedisHealthCheckInterval)
	}

	file, header, err := r.FormFile("file")
	if err != nil {
		http.Error(w, "Failed to get file from request", http.StatusBadRequest)
		return
	}
	defer file.Close()

	// Check if the file extension is allowed
	ext := strings.ToLower(filepath.Ext(header.Filename))
	allowed := false
	for _, allowedExt := range conf.Uploads.AllowedExtensions {
		if ext == allowedExt {
			allowed = true
			break
		}
	}
	if !allowed {
		http.Error(w, "File extension not allowed", http.StatusForbidden)
		return
	}

	chunkSize, err := parseChunkSize(conf.Uploads.ChunkSize)
	if err != nil {
		http.Error(w, "Invalid chunk size", http.StatusInternalServerError)
		return
	}

	filePath := filepath.Join(conf.Server.StoragePath, header.Filename)
	outFile, err := os.Create(filePath)
	if err != nil {
		http.Error(w, "Failed to create file", http.StatusInternalServerError)
		return
	}
	defer outFile.Close()

	buf := make([]byte, chunkSize)
	for {
		n, err := file.Read(buf)
		if err != nil && err != io.EOF {
			http.Error(w, "Failed to read file", http.StatusInternalServerError)
			return
		}
		if n == 0 {
			break
		}

		if _, err := outFile.Write(buf[:n]); err != nil {
			http.Error(w, "Failed to write file", http.StatusInternalServerError)
			return
		}
	}

	logrus.Infof("File %s uploaded successfully", header.Filename)
	w.WriteHeader(http.StatusOK)
}

func parseChunkSize(sizeStr string) (int64, error) {
	sizeStr = sizeStr[:len(sizeStr)-2] // Remove the "MB" suffix
	size, err := strconv.ParseInt(sizeStr, 10, 64)
	if err != nil {
		return 0, err
	}
	return size * 1024 * 1024, nil // Convert MB to bytes
}
package config

import (
	"github.com/spf13/viper"
)

type ServerConfig struct {
	ListenPort              string `mapstructure:"listenport"`
	StoragePath             string `mapstructure:"storagepath"`
	FileTTL                 string `mapstructure:"filettl"`
	MetricsEnabled          bool   `mapstructure:"metricsenabled"`
	MetricsPort             string `mapstructure:"metricsport"`
	UnixSocket              bool   `mapstructure:"unixsocket"`
	LogFile                 string `mapstructure:"logfile"`
	LogLevel                string `mapstructure:"loglevel"`
	NetworkChangeMonitoring bool   `mapstructure:"NetworkChangeMonitoring"`
	ResumeableUploads       bool   `mapstructure:"ResumeableUploads"`
	ResumeableDownloads     bool   `mapstructure:"ResumeableDownloads"`
	AutoAdjustWorkers       bool   `mapstructure:"AutoAdjustWorkers"`
	DeduplicationEnabled    bool   `mapstructure:"DeduplicationEnabled"`
	MinFreeBytes            string `mapstructure:"minfreebytes"`
	LogLimiter              bool   `mapstructure:"LogLimiter"`
}

type ISOConfig struct {
	Enabled bool `mapstructure:"enabled"`
}

type TimeoutsConfig struct {
	ReadTimeout  string `mapstructure:"readtimeout"`
	WriteTimeout string `mapstructure:"writetimeout"`
	IdleTimeout  string `mapstructure:"idletimeout"`
}

type WorkersConfig struct {
	UploadQueueSize int `mapstructure:"UploadQueueSize"`
	NumWorkers      int `mapstructure:"NumWorkers"`
	NumScanWorkers  int `mapstructure:"NumScanWorkers"`
}

type ClamAVConfig struct {
	ClamAVEnabled bool   `mapstructure:"clamavenabled"`
	ClamAVSocket  string `mapstructure:"clamavsocket"`
}

type RedisConfig struct {
	RedisEnabled             bool   `mapstructure:"redisenabled"`
	RedisDBIndex             int    `mapstructure:"redisdbindex"`
	RedisAddr                string `mapstructure:"redisaddr"`
	RedisPassword            string `mapstructure:"redispassword"`
	RedisHealthCheckInterval string `mapstructure:"redishealthcheckinterval"`
}

type Config struct {
	Server   ServerConfig   `mapstructure:"server"`
	ISO      ISOConfig      `mapstructure:"iso"`
	Timeouts TimeoutsConfig `mapstructure:"timeouts"`
	Workers  WorkersConfig  `mapstructure:"workers"`
	ClamAV   ClamAVConfig   `mapstructure:"clamav"`
	Redis    RedisConfig    `mapstructure:"redis"`
	File     struct {
		FileRevision int `mapstructure:"filerevision"`
	} `mapstructure:"file"`
}

func LoadConfig(configFile string) (*Config, error) {
	var conf Config
	viper.SetConfigFile(configFile)
	if err := viper.ReadInConfig(); err != nil {
		return nil, err
	}
	if err := viper.Unmarshal(&conf); err != nil {
		return nil, err
	}
	return &conf, nil
}
package workers

import (
    "context"
    "fmt"
    "os/exec"
    "strings"
    "github.com/sirupsen/logrus"
)

type UploadTask struct {
    // Define the fields for the upload task
}

type ScanTask struct {
    FilePath string
}

type ClamAVClient struct {
    Socket string
}

func (c *ClamAVClient) ScanFile(filePath string) error {
    cmd := exec.Command("clamscan", filePath)
    output, err := cmd.CombinedOutput()
    if err != nil {
        return fmt.Errorf("error executing clamscan: %v, output: %s", err, string(output))
    }

    if string(output) == "" {
        return fmt.Errorf("unexpected clamscan output")
    }

    if !strings.Contains(string(output), "OK") {
        return fmt.Errorf("file %s is infected: %s", filePath, string(output))
    }

    return nil
}

func InitializeUploadWorkerPool(ctx context.Context, uploadQueue chan UploadTask, queueSize int) {
    // Initialize the upload worker pool
}

func InitializeScanWorkerPool(ctx context.Context, clamClient *ClamAVClient, scanQueue chan ScanTask, workerCount int) {
    for i := 0; i < workerCount; i++ {
        go func() {
            for {
                select {
                case <-ctx.Done():
                    return
                case task := <-scanQueue:
                    err := clamClient.ScanFile(task.FilePath)
                    if err != nil {
                        logrus.Errorf("Error scanning file %s: %v", task.FilePath, err)
                    } else {
                        logrus.Infof("File %s scanned successfully", task.FilePath)
                    }
                }
            }
        }()
    }
}

func ScanWorker(ctx context.Context, scanQueue chan ScanTask) {
    for {
        select {
        case <-ctx.Done():
            return
        case task := <-scanQueue:
            // Process the scan task
            logrus.Infof("Processing scan task: %+v", task)
        }
    }
}

func UploadWorker(ctx context.Context, uploadQueue chan UploadTask) {
    for {
        select {
        case <-ctx.Done():
            return
        case task := <-uploadQueue:
            // Process the upload task
            logrus.Infof("Processing upload task: %+v", task)
        }
    }
}
package main

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"mime"
	"net/http"
	"net/url"
	"os"
	"path/filepath" // Added this import for filepath usage
	"strconv"
	"testing"
)

const (
	serverURL    = "http://[::1]:8080"				// Replace with your actual server URL
	secret       = "a-orc-and-a-humans-is-drinking-ale"             // Replace with your HMAC secret key
	uploadPath   = "hmac_icon.png"                 			// Test file to upload
	protocolType = "v2"						// Use v2, v, or token as needed
)

// TestUpload performs a basic HMAC validation and upload test.
func TestUpload(t *testing.T) {
	// File setup for testing
	file, err := os.Open(uploadPath)
	if err != nil {
		t.Fatalf("Error opening file: %v", err)
	}
	defer file.Close()

	fileInfo, _ := file.Stat()
	fileStorePath := uploadPath
	contentLength := fileInfo.Size()

	// Generate HMAC based on protocol type
	hmacValue := generateHMAC(fileStorePath, contentLength, protocolType)

	// Formulate request URL with HMAC in query params
	reqURL := fmt.Sprintf("%s/%s?%s=%s", serverURL, fileStorePath, protocolType, url.QueryEscape(hmacValue))

	// Prepare HTTP PUT request with file data
	req, err := http.NewRequest(http.MethodPut, reqURL, file)
	if err != nil {
		t.Fatalf("Error creating request: %v", err)
	}
	req.Header.Set("Content-Type", "application/octet-stream")
	req.Header.Set("Content-Length", strconv.FormatInt(contentLength, 10))

	// Execute HTTP request
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatalf("Error executing request: %v", err)
	}
	defer resp.Body.Close()

	t.Logf("Response status: %s", resp.Status)
}

// Generates the HMAC based on your protocol version
func generateHMAC(filePath string, contentLength int64, protocol string) string {
	mac := hmac.New(sha256.New, []byte(secret))
	macString := ""

	// Calculate HMAC according to protocol
	if protocol == "v" {
		mac.Write([]byte(filePath + "\x20" + strconv.FormatInt(contentLength, 10)))
		macString = hex.EncodeToString(mac.Sum(nil))
	} else if protocol == "v2" || protocol == "token" {
		contentType := mime.TypeByExtension(filepath.Ext(filePath))
		if contentType == "" {
			contentType = "application/octet-stream"
		}
		mac.Write([]byte(filePath + "\x00" + strconv.FormatInt(contentLength, 10) + "\x00" + contentType))
		macString = hex.EncodeToString(mac.Sum(nil))
	}

	return macString
}
