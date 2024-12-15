package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"regexp"
	"strconv"
	"syscall"
	"time"
	"runtime"

	"github.com/PlusOne/hmac-file-server/config"
	"github.com/PlusOne/hmac-file-server/handlers"
	"github.com/PlusOne/hmac-file-server/workers"
	"github.com/patrickmn/go-cache"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/go-redis/redis/v8"
	"github.com/sirupsen/logrus"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/disk"
	"github.com/shirou/gopsutil/host"
	"github.com/shirou/gopsutil/mem"
)

var (
	fileInfoCache *cache.Cache
	uploadQueue   chan workers.UploadTask
	scanQueue     chan workers.ScanTask
	conf          *config.Config
	clamClient    *workers.ClamAVClient
	networkEvents chan NetworkEvent
	versionString = "1.0.0"
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
	router.HandleFunc("/upload", handlers.UploadHandler)
	return router
}

func setDefaults() {
	conf.Server = config.ServerConfig{
		StoragePath:    "./storage",
		FileTTL:        "24h",
		MetricsEnabled: true,
		MetricsPort:    "2112",
		UnixSocket:     false,
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
	logrus.Infof("Uptime: %v seconds", hInfo.Uptime)
	logrus.Infof("Boot Time: %v", time.Unix(int64(hInfo.BootTime), 0))
	logrus.Infof("Platform: %s", hInfo.Platform)
	logrus.Infof("Platform Family: %s", hInfo.PlatformFamily)
	logrus.Infof("Platform Version: %s", hInfo.PlatformVersion)
	logrus.Infof("Kernel Version: %s", hInfo.KernelVersion)
}

func setupLogging() {
	level, err := logrus.ParseLevel(conf.Server.LogLevel)
	if err != nil {
		logrus.Fatalf("Invalid log level: %s", conf.Server.LogLevel)
	}
	logrus.SetLevel(level)

	if conf.Server.LogFile != "" {
		logFile, err := os.OpenFile(conf.Server.LogFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			log.Fatalf("Failed to open log file: %v", err)
		}
		log.SetOutput(io.MultiWriter(os.Stdout, logFile))
	} else {
		log.SetOutput(os.Stdout)
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
	for i := 0; i < conf.Workers.UploadQueueSize; i++ {
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
	for i := 0; i < conf.Workers.UploadQueueSize; i++ {
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
