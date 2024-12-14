// main.go

package main

import (
	"bufio"
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"flag"
	"fmt"
	"io"
	"mime"
	"net"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/dutchcoders/go-clamd" // ClamAV integration
	"github.com/go-redis/redis/v8"    // Redis integration
	"github.com/patrickmn/go-cache"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/disk"
	"github.com/shirou/gopsutil/host"
	"github.com/shirou/gopsutil/mem"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

// Configuration structure
type ServerConfig struct {
	ListenPort           string `mapstructure:"ListenPort"`
	UnixSocket           bool   `mapstructure:"UnixSocket"`
	StoragePath          string `mapstructure:"StoragePath"`
	LogLevel             string `mapstructure:"LogLevel"`
	LogFile              string `mapstructure:"LogFile"`
	MetricsEnabled       bool   `mapstructure:"MetricsEnabled"`
	MetricsPort          string `mapstructure:"MetricsPort"`
	FileTTL              string `mapstructure:"FileTTL"`
	MinFreeBytes         int64  `mapstructure:"MinFreeBytes"` // Minimum free bytes required
	DeduplicationEnabled bool   `mapstructure:"DeduplicationEnabled"`
}

type TimeoutConfig struct {
	ReadTimeout  string `mapstructure:"ReadTimeout"`
	WriteTimeout string `mapstructure:"WriteTimeout"`
	IdleTimeout  string `mapstructure:"IdleTimeout"`
}

type SecurityConfig struct {
	Secret string `mapstructure:"Secret"`
}

type VersioningConfig struct {
	EnableVersioning bool `mapstructure:"EnableVersioning"`
	MaxVersions      int  `mapstructure:"MaxVersions"`
}

type UploadsConfig struct {
	ResumableUploadsEnabled bool     `mapstructure:"ResumableUploadsEnabled"`
	ChunkedUploadsEnabled   bool     `mapstructure:"ChunkedUploadsEnabled"`
	ChunkSize               int64    `mapstructure:"ChunkSize"`
	AllowedExtensions       []string `mapstructure:"AllowedExtensions"`
}

type ClamAVConfig struct {
	ClamAVEnabled  bool   `mapstructure:"ClamAVEnabled"`
	ClamAVSocket   string `mapstructure:"ClamAVSocket"`
	NumScanWorkers int    `mapstructure:"NumScanWorkers"`
}

type RedisConfig struct {
	RedisEnabled             bool   `mapstructure:"RedisEnabled"`
	RedisDBIndex             int    `mapstructure:"RedisDBIndex"`
	RedisAddr                string `mapstructure:"RedisAddr"`
	RedisPassword            string `mapstructure:"RedisPassword"`
	RedisHealthCheckInterval string `mapstructure:"RedisHealthCheckInterval"`
}

type WorkersConfig struct {
	NumWorkers      int `mapstructure:"NumWorkers"`
	UploadQueueSize int `mapstructure:"UploadQueueSize"`
}

type FileConfig struct {
	FileRevision int `mapstructure:"FileRevision"`
}

// Configuration structure for ISO settings
type ISOConfig struct {
	Enabled    bool   `mapstructure:"enabled"`
	Size       string `mapstructure:"size"`
	MountPoint string `mapstructure:"mountpoint"`
	Charset    string `mapstructure:"charset"` // Add this line
}

// Add ISO configuration to the main configuration structure
type Config struct {
	Server     ServerConfig     `mapstructure:"server"`
	Timeouts   TimeoutConfig    `mapstructure:"timeouts"`
	Security   SecurityConfig   `mapstructure:"security"`
	Versioning VersioningConfig `mapstructure:"versioning"`
	Uploads    UploadsConfig    `mapstructure:"uploads"`
	ClamAV     ClamAVConfig     `mapstructure:"clamav"`
	Redis      RedisConfig      `mapstructure:"redis"`
	Workers    WorkersConfig    `mapstructure:"workers"`
	File       FileConfig       `mapstructure:"file"`
	ISO        ISOConfig        `mapstructure:"iso"`
}

// UploadTask represents a file upload task
type UploadTask struct {
	AbsFilename string
	Request     *http.Request
	Result      chan error
}

// ScanTask represents a file scan task
type ScanTask struct {
	AbsFilename string
	Result      chan error
}

// NetworkEvent represents a network-related event
type NetworkEvent struct {
	Type    string
	Details string
}

var (
	conf           Config
	versionString  string = "v2.0-dev"
	log                   = logrus.New()
	uploadQueue    chan UploadTask
	networkEvents  chan NetworkEvent
	fileInfoCache  *cache.Cache
	clamClient     *clamd.Clamd  // Added for ClamAV integration
	redisClient    *redis.Client // Redis client
	redisConnected bool          // Redis connection status
	mu             sync.RWMutex

	// Prometheus metrics
	uploadDuration      prometheus.Histogram
	uploadErrorsTotal   prometheus.Counter
	uploadsTotal        prometheus.Counter
	downloadDuration    prometheus.Histogram
	downloadsTotal      prometheus.Counter
	downloadErrorsTotal prometheus.Counter
	memoryUsage         prometheus.Gauge
	cpuUsage            prometheus.Gauge
	activeConnections   prometheus.Gauge
	requestsTotal       *prometheus.CounterVec
	goroutines          prometheus.Gauge
	uploadSizeBytes     prometheus.Histogram
	downloadSizeBytes   prometheus.Histogram

	// Channels
	scanQueue   chan ScanTask
	ScanWorkers = 5 // Number of ClamAV scan workers
)

const (
	MinFreeBytes = 1 << 30 // 1 GB
)

func main() {
	// Set default configuration values
	setDefaults()

	// Flags for configuration file
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	// Load configuration
	err := readConfig(configFile, &conf)
	if err != nil {
		log.Fatalf("Error reading config: %v", err) // Fatal: application cannot proceed
	}
	log.Info("Configuration loaded successfully.")

	// Verify and create ISO container if it doesn't exist
	if conf.ISO.Enabled {
		err = verifyAndCreateISOContainer()
		if err != nil {
			log.Fatalf("ISO container verification failed: %v", err)
		}
	}

	// Initialize file info cache
	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)

	// Create store directory
	err = os.MkdirAll(conf.Server.StoragePath, os.ModePerm)
	if err != nil {
		log.Fatalf("Error creating store directory: %v", err)
	}
	log.WithField("directory", conf.Server.StoragePath).Info("Store directory is ready")

	// Check free space with retry
	err = checkFreeSpaceWithRetry(conf.Server.StoragePath, 3, 5*time.Second)
	if err != nil {
		log.Fatalf("Insufficient free space: %v", err)
	}

	// Setup logging
	setupLogging()

	// Log system information
	logSystemInfo()

	// Initialize Prometheus metrics
	initMetrics()
	log.Info("Prometheus metrics initialized.")

	// Initialize upload and scan queues
	uploadQueue = make(chan UploadTask, conf.Workers.UploadQueueSize)
	log.Infof("Upload queue initialized with size: %d", conf.Workers.UploadQueueSize)
	scanQueue = make(chan ScanTask, conf.Workers.UploadQueueSize)
	networkEvents = make(chan NetworkEvent, 100)
	log.Info("Upload, scan, and network event channels initialized.")

	// Context for goroutines
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Start network monitoring
	go monitorNetwork(ctx)
	go handleNetworkEvents(ctx)

	// Update system metrics
	go updateSystemMetrics(ctx)

	// Initialize ClamAV client if enabled
	if conf.ClamAV.ClamAVEnabled {
		clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			log.WithFields(logrus.Fields{
				"error": err.Error(),
			}).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
		} else {
			log.Info("ClamAV client initialized successfully.")
		}
	}

	// Initialize Redis client if enabled
	if conf.Redis.RedisEnabled {
		initRedis()
	}

	// Redis Initialization
	initRedis()
	log.Info("Redis client initialized and connected successfully.")

	// ClamAV Initialization
	if conf.ClamAV.ClamAVEnabled {
		clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			log.WithFields(logrus.Fields{
				"error": err.Error(),
			}).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
		} else {
			log.Info("ClamAV client initialized successfully.")
		}
	}

	// Initialize worker pools
	initializeUploadWorkerPool(ctx)
	if conf.ClamAV.ClamAVEnabled && clamClient != nil {
		initializeScanWorkerPool(ctx)
	}

	// Start Redis health monitor if Redis is enabled
	if conf.Redis.RedisEnabled && redisClient != nil {
		go MonitorRedisHealth(ctx, redisClient, parseDuration(conf.Redis.RedisHealthCheckInterval))
	}

	// Setup router
	router := setupRouter()

	// Start file cleaner
	fileTTL, err := time.ParseDuration(conf.Server.FileTTL)
	if err != nil {
		log.Fatalf("Invalid FileTTL: %v", err)
	}
	go runFileCleaner(ctx, conf.Server.StoragePath, fileTTL)

	// Parse timeout durations
	readTimeout, err := time.ParseDuration(conf.Timeouts.ReadTimeout)
	if err != nil {
		log.Fatalf("Invalid ReadTimeout: %v", err)
	}

	writeTimeout, err := time.ParseDuration(conf.Timeouts.WriteTimeout)
	if err != nil {
		log.Fatalf("Invalid WriteTimeout: %v", err)
	}

	idleTimeout, err := time.ParseDuration(conf.Timeouts.IdleTimeout)
	if err != nil {
		log.Fatalf("Invalid IdleTimeout: %v", err)
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
			log.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
			if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
				log.Fatalf("Metrics server failed: %v", err)
			}
		}()
	}

	// Setup graceful shutdown
	setupGracefulShutdown(server, cancel)

	// Start server
	log.Infof("Starting HMAC file server %s...", versionString)
	if conf.Server.UnixSocket {
		// Listen on Unix socket
		if err := os.RemoveAll(conf.Server.ListenPort); err != nil {
			log.Fatalf("Failed to remove existing Unix socket: %v", err)
		}
		listener, err := net.Listen("unix", conf.Server.ListenPort)
		if err != nil {
			log.Fatalf("Failed to listen on Unix socket %s: %v", conf.Server.ListenPort, err)
		}
		defer listener.Close()
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	} else {
		// Listen on TCP port
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	}

	// Example files to include in the ISO container
	files := []string{"file1.txt", "file2.txt"}
	isoPath := "/path/to/container.iso"

	// Create ISO container
	err = CreateISOContainer(files, isoPath, conf.ISO.Size, conf.ISO.Charset)
	if err != nil {
		fmt.Printf("Failed to create ISO container: %v\n", err)
		return
	}

	// Mount ISO container
	err = MountISOContainer(isoPath, conf.ISO.MountPoint)
	if err != nil {
		fmt.Printf("Failed to mount ISO container: %v\n", err)
		return
	}

	// Unmount ISO container (example)
	err = UnmountISOContainer(conf.ISO.MountPoint)
	if err != nil {
		fmt.Printf("Failed to unmount ISO container: %v\n", err)
		return
	}
}

// Function to load configuration using Viper
func readConfig(configFilename string, conf *Config) error {
	viper.SetConfigFile(configFilename)
	viper.SetConfigType("toml")

	// Read in environment variables that match
	viper.AutomaticEnv()
	viper.SetEnvPrefix("HMAC") // Prefix for environment variables

	// Read the config file
	if err := viper.ReadInConfig(); err != nil {
		return fmt.Errorf("error reading config file: %w", err)
	}

	// Unmarshal the config into the Config struct
	if err := viper.Unmarshal(conf); err != nil {
		return fmt.Errorf("unable to decode into struct: %w", err)
	}

	// Debug log the loaded configuration
	log.Debugf("Loaded Configuration: %+v", conf.Server)

	// Validate the configuration
	if err := validateConfig(conf); err != nil {
		return fmt.Errorf("configuration validation failed: %w", err)
	}

	// Set Deduplication Enabled
	conf.Server.DeduplicationEnabled = viper.GetBool("deduplication.Enabled")

	return nil
}

// Set default configuration values
func setDefaults() {
	// Server defaults
	viper.SetDefault("server.ListenPort", "8080")
	viper.SetDefault("server.UnixSocket", false)
	viper.SetDefault("server.StoragePath", "./uploads")
	viper.SetDefault("server.LogLevel", "info")
	viper.SetDefault("server.LogFile", "")
	viper.SetDefault("server.MetricsEnabled", true)
	viper.SetDefault("server.MetricsPort", "9090")
	viper.SetDefault("server.FileTTL", "8760h")      // 365d -> 8760h
	viper.SetDefault("server.MinFreeBytes", 100<<20) // 100 MB

	// Timeout defaults
	viper.SetDefault("timeouts.ReadTimeout", "4800s") // supports 's'
	viper.SetDefault("timeouts.WriteTimeout", "4800s")
	viper.SetDefault("timeouts.IdleTimeout", "4800s")

	// Security defaults
	viper.SetDefault("security.Secret", "changeme")

	// Versioning defaults
	viper.SetDefault("versioning.EnableVersioning", false)
	viper.SetDefault("versioning.MaxVersions", 1)

	// Uploads defaults
	viper.SetDefault("uploads.ResumableUploadsEnabled", true)
	viper.SetDefault("uploads.ChunkedUploadsEnabled", true)
	viper.SetDefault("uploads.ChunkSize", 8192)
	viper.SetDefault("uploads.AllowedExtensions", []string{
		".txt", ".pdf",
		".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp",
		".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2",
		".mp3", ".ogg",
	})

	// ClamAV defaults
	viper.SetDefault("clamav.ClamAVEnabled", true)
	viper.SetDefault("clamav.ClamAVSocket", "/var/run/clamav/clamd.ctl")
	viper.SetDefault("clamav.NumScanWorkers", 2)

	// Redis defaults
	viper.SetDefault("redis.RedisEnabled", true)
	viper.SetDefault("redis.RedisAddr", "localhost:6379")
	viper.SetDefault("redis.RedisPassword", "")
	viper.SetDefault("redis.RedisDBIndex", 0)
	viper.SetDefault("redis.RedisHealthCheckInterval", "120s")

	// Workers defaults
	viper.SetDefault("workers.NumWorkers", 2)
	viper.SetDefault("workers.UploadQueueSize", 50)

	// Deduplication defaults
	viper.SetDefault("deduplication.Enabled", true)

	// ISO defaults
	viper.SetDefault("iso.Enabled", true)
	viper.SetDefault("iso.Size", "1GB")
	viper.SetDefault("iso.MountPoint", "/mnt/iso")
	viper.SetDefault("iso.Charset", "utf-8") // Add this line
}

// Validate configuration fields
func validateConfig(conf *Config) error {
	if conf.Server.ListenPort == "" {
		return fmt.Errorf("ListenPort must be set")
	}
	if conf.Security.Secret == "" {
		return fmt.Errorf("secret must be set")
	}
	if conf.Server.StoragePath == "" {
		return fmt.Errorf("StoragePath must be set")
	}
	if conf.Server.FileTTL == "" {
		return fmt.Errorf("FileTTL must be set")
	}

	// Validate timeouts
	if _, err := time.ParseDuration(conf.Timeouts.ReadTimeout); err != nil {
		return fmt.Errorf("invalid ReadTimeout: %v", err)
	}
	if _, err := time.ParseDuration(conf.Timeouts.WriteTimeout); err != nil {
		return fmt.Errorf("invalid WriteTimeout: %v", err)
	}
	if _, err := time.ParseDuration(conf.Timeouts.IdleTimeout); err != nil {
		return fmt.Errorf("invalid IdleTimeout: %v", err)
	}

	// Validate Redis configuration if enabled
	if conf.Redis.RedisEnabled {
		if conf.Redis.RedisAddr == "" {
			return fmt.Errorf("RedisAddr must be set when Redis is enabled")
		}
	}

	// Validate ISO configuration
	if conf.ISO.Enabled {
		if conf.ISO.Size == "" {
			return fmt.Errorf("ISO size must be set")
		}
		if conf.ISO.MountPoint == "" {
			return fmt.Errorf("ISO mount point must be set")
		}
		if conf.ISO.Charset == "" {
			return fmt.Errorf("ISO charset must be set")
		}
	}

	// Add more validations as needed

	return nil
}

// Setup logging
func setupLogging() {
	level, err := logrus.ParseLevel(conf.Server.LogLevel)
	if err != nil {
		log.Fatalf("Invalid log level: %s", conf.Server.LogLevel)
	}
	log.SetLevel(level)

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
	log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
		// You can customize the format further if needed
	})
}

// Log system information
func logSystemInfo() {
	log.Info("========================================")
	log.Infof("       HMAC File Server - %s          ", versionString)
	log.Info("  Secure File Handling with HMAC Auth   ")
	log.Info("========================================")

	log.Info("Features: Prometheus Metrics, Chunked Uploads, ClamAV Scanning")
	log.Info("Build Date: 2024-10-28")

	log.Infof("Operating System: %s", runtime.GOOS)
	log.Infof("Architecture: %s", runtime.GOARCH)
	log.Infof("Number of CPUs: %d", runtime.NumCPU())
	log.Infof("Go Version: %s", runtime.Version())

	v, _ := mem.VirtualMemory()
	log.Infof("Total Memory: %v MB", v.Total/1024/1024)
	log.Infof("Free Memory: %v MB", v.Free/1024/1024)
	log.Infof("Used Memory: %v MB", v.Used/1024/1024)

	cpuInfo, _ := cpu.Info()
	for _, info := range cpuInfo {
		log.Infof("CPU Model: %s, Cores: %d, Mhz: %f", info.ModelName, info.Cores, info.Mhz)
	}

	partitions, _ := disk.Partitions(false)
	for _, partition := range partitions {
		usage, _ := disk.Usage(partition.Mountpoint)
		log.Infof("Disk Mountpoint: %s, Total: %v GB, Free: %v GB, Used: %v GB",
			partition.Mountpoint, usage.Total/1024/1024/1024, usage.Free/1024/1024/1024, usage.Used/1024/1024/1024)
	}

	hInfo, _ := host.Info()
	log.Infof("Hostname: %s", hInfo.Hostname)
	log.Infof("Uptime: %v seconds", hInfo.Uptime)
	log.Infof("Boot Time: %v", time.Unix(int64(hInfo.BootTime), 0))
	log.Infof("Platform: %s", hInfo.Platform)
	log.Infof("Platform Family: %s", hInfo.PlatformFamily)
	log.Infof("Platform Version: %s", hInfo.PlatformVersion)
	log.Infof("Kernel Version: %s", hInfo.KernelVersion)
}

// Initialize Prometheus metrics
// Duplicate initMetrics function removed
func initMetrics() {
	uploadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{Namespace: "hmac", Name: "file_server_upload_duration_seconds", Help: "Histogram of file upload duration in seconds.", Buckets: prometheus.DefBuckets})
	uploadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{Namespace: "hmac", Name: "file_server_upload_errors_total", Help: "Total number of file upload errors."})
	uploadsTotal = prometheus.NewCounter(prometheus.CounterOpts{Namespace: "hmac", Name: "file_server_uploads_total", Help: "Total number of successful file uploads."})
	downloadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{Namespace: "hmac", Name: "file_server_download_duration_seconds", Help: "Histogram of file download duration in seconds.", Buckets: prometheus.DefBuckets})
	downloadsTotal = prometheus.NewCounter(prometheus.CounterOpts{Namespace: "hmac", Name: "file_server_downloads_total", Help: "Total number of successful file downloads."})
	downloadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{Namespace: "hmac", Name: "file_server_download_errors_total", Help: "Total number of file download errors."})
	memoryUsage = prometheus.NewGauge(prometheus.GaugeOpts{Namespace: "hmac", Name: "memory_usage_bytes", Help: "Current memory usage in bytes."})
	cpuUsage = prometheus.NewGauge(prometheus.GaugeOpts{Namespace: "hmac", Name: "cpu_usage_percent", Help: "Current CPU usage as a percentage."})
	activeConnections = prometheus.NewGauge(prometheus.GaugeOpts{Namespace: "hmac", Name: "active_connections_total", Help: "Total number of active connections."})
	requestsTotal = prometheus.NewCounterVec(prometheus.CounterOpts{Namespace: "hmac", Name: "http_requests_total", Help: "Total number of HTTP requests received, labeled by method and path."}, []string{"method", "path"})
	goroutines = prometheus.NewGauge(prometheus.GaugeOpts{Namespace: "hmac", Name: "goroutines_count", Help: "Current number of goroutines."})
	uploadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_upload_size_bytes",
		Help:      "Histogram of uploaded file sizes in bytes.",
		Buckets:   prometheus.ExponentialBuckets(100, 10, 8),
	})
	downloadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_download_size_bytes",
		Help:      "Histogram of downloaded file sizes in bytes.",
		Buckets:   prometheus.ExponentialBuckets(100, 10, 8),
	})

	if conf.Server.MetricsEnabled {
		prometheus.MustRegister(uploadDuration, uploadErrorsTotal, uploadsTotal)
		prometheus.MustRegister(downloadDuration, downloadsTotal, downloadErrorsTotal)
		prometheus.MustRegister(memoryUsage, cpuUsage, activeConnections, requestsTotal, goroutines)
		prometheus.MustRegister(uploadSizeBytes, downloadSizeBytes)
	}
}

// Update system metrics
func updateSystemMetrics(ctx context.Context) {
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			v, _ := mem.VirtualMemory()
			memoryUsage.Set(float64(v.Used) / float64(v.Total) * 100)
			c, _ := cpu.Percent(0, false)
			if len(c) > 0 {
				cpuUsage.Set(c[0])
			}
			goroutines.Set(float64(runtime.NumGoroutine()))
		}
	}
}

// Function to check if a file exists and return its size
func fileExists(filePath string) (bool, int64) {
	if cachedInfo, found := fileInfoCache.Get(filePath); found {
		if info, ok := cachedInfo.(os.FileInfo); ok {
			return !info.IsDir(), info.Size()
		}
	}

	fileInfo, err := os.Stat(filePath)
	if os.IsNotExist(err) {
		return false, 0
	} else if err != nil {
		log.Error("Error checking file existence:", err)
		return false, 0
	}

	fileInfoCache.Set(filePath, fileInfo, cache.DefaultExpiration)
	return !fileInfo.IsDir(), fileInfo.Size()
}

// Function to check file extension
func isExtensionAllowed(filename string) bool {
	if len(conf.Uploads.AllowedExtensions) == 0 {
		return true // No restrictions if the list is empty
	}
	ext := strings.ToLower(filepath.Ext(filename))
	for _, allowedExt := range conf.Uploads.AllowedExtensions {
		if strings.ToLower(allowedExt) == ext {
			return true
		}
	}
	return false
}

// Version the file by moving the existing file to a versioned directory
func versionFile(absFilename string) error {
	versionDir := absFilename + "_versions"

	err := os.MkdirAll(versionDir, os.ModePerm)
	if err != nil {
		return fmt.Errorf("failed to create version directory: %v", err)
	}

	timestamp := time.Now().Format("20060102-150405")
	versionedFilename := filepath.Join(versionDir, filepath.Base(absFilename)+"."+timestamp)

	err = os.Rename(absFilename, versionedFilename)
	if err != nil {
		return fmt.Errorf("failed to version the file: %v", err)
	}

	log.WithFields(logrus.Fields{
		"original":     absFilename,
		"versioned_as": versionedFilename,
	}).Info("Versioned old file")
	return cleanupOldVersions(versionDir)
}

// Clean up older versions if they exceed the maximum allowed
func cleanupOldVersions(versionDir string) error {
	files, err := os.ReadDir(versionDir)
	if err != nil {
		return fmt.Errorf("failed to list version files: %v", err)
	}

	if conf.Versioning.MaxVersions > 0 && len(files) > conf.Versioning.MaxVersions {
		excessFiles := len(files) - conf.Versioning.MaxVersions
		for i := 0; i < excessFiles; i++ {
			err := os.Remove(filepath.Join(versionDir, files[i].Name()))
			if err != nil {
				return fmt.Errorf("failed to remove old version: %v", err)
			}
			log.WithField("file", files[i].Name()).Info("Removed old version")
		}
	}

	return nil
}

// Process the upload task
func processUpload(task UploadTask) error {
	absFilename := task.AbsFilename
	tempFilename := absFilename + ".tmp"
	r := task.Request

	log.Infof("Processing upload for file: %s", absFilename)
	startTime := time.Now()

	// Handle uploads and write to a temporary file
	if conf.Uploads.ChunkedUploadsEnabled {
		log.Debugf("Chunked uploads enabled. Handling chunked upload for %s", tempFilename)
		err := handleChunkedUpload(tempFilename, r)
		if err != nil {
			uploadDuration.Observe(time.Since(startTime).Seconds())
			log.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Failed to handle chunked upload")
			return err
		}
	} else {
		log.Debugf("Handling standard upload for %s", tempFilename)
		err := createFile(tempFilename, r)
		if err != nil {
			log.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Error creating file")
			uploadDuration.Observe(time.Since(startTime).Seconds())
			return err
		}
	}

	// Perform ClamAV scan on the temporary file
	if clamClient != nil {
		log.Debugf("Scanning %s with ClamAV", tempFilename)
		err := scanFileWithClamAV(tempFilename)
		if err != nil {
			log.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Warn("ClamAV detected a virus or scan failed")
			os.Remove(tempFilename)
			uploadErrorsTotal.Inc()
			return err
		}
		log.Infof("ClamAV scan passed for file: %s", tempFilename)
	}

	// Handle file versioning if enabled
	if conf.Versioning.EnableVersioning {
		existing, _ := fileExists(absFilename)
		if existing {
			log.Infof("File %s exists. Initiating versioning.", absFilename)
			err := versionFile(absFilename)
			if err != nil {
				log.WithFields(logrus.Fields{
					"file":  absFilename,
					"error": err,
				}).Error("Error versioning file")
				os.Remove(tempFilename)
				return err
			}
			log.Infof("File versioned successfully: %s", absFilename)
		}
	}

	// Rename temporary file to final destination
	err := os.Rename(tempFilename, absFilename)
	if err != nil {
		log.WithFields(logrus.Fields{
			"temp_file":  tempFilename,
			"final_file": absFilename,
			"error":      err,
		}).Error("Failed to move file to final destination")
		os.Remove(tempFilename)
		return err
	}
	log.Infof("File moved to final destination: %s", absFilename)

	// Handle deduplication if enabled
	if conf.Server.DeduplicationEnabled {
		log.Debugf("Deduplication enabled. Checking duplicates for %s", absFilename)
		err = handleDeduplication(context.Background(), absFilename)
		if err != nil {
			log.WithError(err).Error("Deduplication failed")
			uploadErrorsTotal.Inc()
			return err
		}
		log.Infof("Deduplication handled successfully for file: %s", absFilename)
	}

	// Handle ISO container if enabled
	if conf.ISO.Enabled {
		err = handleISOContainer(absFilename)
		if err != nil {
			log.WithError(err).Error("ISO container handling failed")
			uploadErrorsTotal.Inc()
			return err
		}
		log.Infof("ISO container handled successfully for file: %s", absFilename)
	}

	log.WithFields(logrus.Fields{
		"file": absFilename,
	}).Info("File uploaded and processed successfully")

	uploadDuration.Observe(time.Since(startTime).Seconds())
	uploadsTotal.Inc()
	return nil
}

// Improved uploadWorker function with better concurrency handling
func uploadWorker(ctx context.Context, workerID int) {
    log.Infof("Upload worker %d started.", workerID)
    defer log.Infof("Upload worker %d stopped.", workerID)
    for {
        select {
        case <-ctx.Done():
            return
        case task, ok := <-uploadQueue:
            if !ok {
                return
            }
            err := processUpload(task)
            task.Result <- err
        }
    }
}

// Improved initializeUploadWorkerPool function
func initializeUploadWorkerPool(ctx context.Context) {
    for i := 0; i < conf.Workers.NumWorkers; i++ {
        go uploadWorker(ctx, i)
    }
    log.Infof("Initialized %d upload workers", conf.Workers.NumWorkers)
}

// Worker function to process scan tasks
func scanWorker(ctx context.Context, workerID int) {
	log.WithField("worker_id", workerID).Info("Scan worker started")
	for {
		select {
		case <-ctx.Done():
			log.WithField("worker_id", workerID).Info("Scan worker stopping")
			return
		case task, ok := <-scanQueue:
			if !ok {
				log.WithField("worker_id", workerID).Info("Scan queue closed")
				return
			}
			log.WithFields(logrus.Fields{
				"worker_id": workerID,
				"file":      task.AbsFilename,
			}).Info("Processing scan task")
			err := scanFileWithClamAV(task.AbsFilename)
			if err != nil {
				log.WithFields(logrus.Fields{
					"worker_id": workerID,
					"file":      task.AbsFilename,
					"error":     err,
				}).Error("Failed to scan file")
			} else {
				log.WithFields(logrus.Fields{
					"worker_id": workerID,
					"file":      task.AbsFilename,
				}).Info("Successfully scanned file")
			}
			task.Result <- err
			close(task.Result)
		}
	}
}

// Initialize scan worker pool
func initializeScanWorkerPool(ctx context.Context) {
	for i := 0; i < conf.ClamAV.NumScanWorkers; i++ {
		go scanWorker(ctx, i)
	}
	log.Infof("Initialized %d scan workers", conf.ClamAV.NumScanWorkers)
}

// Setup router with middleware
func setupRouter() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRequest)
	if (conf.Server.MetricsEnabled) {
		mux.Handle("/metrics", promhttp.Handler())
	}

	// Apply middleware
	handler := loggingMiddleware(mux)
	handler = recoveryMiddleware(handler)
	handler = corsMiddleware(handler)
	return handler
}

// Middleware for logging
func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requestsTotal.WithLabelValues(r.Method, r.URL.Path).Inc()
		next.ServeHTTP(w, r)
	})
}

// Middleware for panic recovery
func recoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if rec := recover(); rec != nil {
				log.WithFields(logrus.Fields{
					"method": r.Method,
					"url":    r.URL.String(),
					"error":  rec,
				}).Error("Panic recovered in HTTP handler")
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
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

// Handle file uploads and downloads
func handleRequest(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost && strings.Contains(r.Header.Get("Content-Type"), "multipart/form-data") {
		absFilename, err := sanitizeFilePath(conf.Server.StoragePath, strings.TrimPrefix(r.URL.Path, "/"))
		if err != nil {
			log.WithError(err).Error("Invalid file path")
			http.Error(w, "Invalid file path", http.StatusBadRequest)
			return
		}
		err = handleMultipartUpload(w, r, absFilename)
		if err != nil {
			log.WithError(err).Error("Failed to handle multipart upload")
			http.Error(w, "Failed to handle multipart upload", http.StatusInternalServerError)
			return
		}
		w.WriteHeader(http.StatusCreated)
		return
	}

	// Get client IP address
	clientIP := r.Header.Get("X-Real-IP")
	if clientIP == "" {
		clientIP = r.Header.Get("X-Forwarded-For")
	}
	if clientIP == "" {
		// Fallback to RemoteAddr
		host, _, err := net.SplitHostPort(r.RemoteAddr)
		if err != nil {
			log.WithError(err).Warn("Failed to parse RemoteAddr")
			clientIP = r.RemoteAddr
		} else {
			clientIP = host
		}
	}

	// Log the request with the client IP
	log.WithFields(logrus.Fields{
		"method": r.Method,
		"url":    r.URL.String(),
		"remote": clientIP,
	}).Info("Incoming request")

	// Parse URL and query parameters
	p := r.URL.Path
	a, err := url.ParseQuery(r.URL.RawQuery)
	if err != nil {
		log.Warn("Failed to parse query parameters")
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}

	fileStorePath := strings.TrimPrefix(p, "/")
	if fileStorePath == "" || fileStorePath == "/" {
		log.Warn("Access to root directory is forbidden")
		http.Error(w, "Forbidden", http.StatusForbidden)
		return
	} else if fileStorePath[0] == '/' {
		fileStorePath = fileStorePath[1:]
	}

	absFilename, err := sanitizeFilePath(conf.Server.StoragePath, fileStorePath)
	if err != nil {
		log.WithFields(logrus.Fields{
			"file":  fileStorePath,
			"error": err,
		}).Warn("Invalid file path")
		http.Error(w, "Invalid file path", http.StatusBadRequest)
		return
	}

	switch r.Method {
	case http.MethodPut:
		handleUpload(w, r, absFilename, fileStorePath, a)
	case http.MethodHead, http.MethodGet:
		handleDownload(w, r, absFilename, fileStorePath)
	case http.MethodOptions:
		// Handled by NGINX; no action needed
		w.Header().Set("Allow", "OPTIONS, GET, PUT, HEAD")
		return
	default:
		log.WithField("method", r.Method).Warn("Invalid HTTP method for upload directory")
		http.Error(w, "Method Not Allowed", http.StatusMethodNotAllowed)
		return
	}
}

// Handle file uploads with extension restrictions and HMAC validation
func handleUpload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string, a url.Values) {
	// Log the storage path being used
	log.Infof("Using storage path: %s", conf.Server.StoragePath)

	// Determine protocol version based on query parameters
	var protocolVersion string
	if a.Get("v2") != "" {
		protocolVersion = "v2"
	} else if a.Get("token") != "" {
		protocolVersion = "token"
	} else if a.Get("v") != "" {
		protocolVersion = "v"
	} else {
		log.Warn("No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC")
		http.Error(w, "No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC", http.StatusForbidden)
		return
	}
	log.Debugf("Protocol version determined: %s", protocolVersion)

	// Initialize HMAC
	mac := hmac.New(sha256.New, []byte(conf.Security.Secret))

	// Calculate MAC based on protocolVersion
	if protocolVersion == "v" {
		mac.Write([]byte(fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)))
	} else if protocolVersion == "v2" || protocolVersion == "token" {
		contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
		if contentType == "" {
			contentType = "application/octet-stream"
		}
		mac.Write([]byte(fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10) + "\x00" + contentType))
	}

	calculatedMAC := mac.Sum(nil)
	log.Debugf("Calculated MAC: %x", calculatedMAC)

	// Decode provided MAC from hex
	providedMACHex := a.Get(protocolVersion)
	providedMAC, err := hex.DecodeString(providedMACHex)
	if err != nil {
		log.Warn("Invalid MAC encoding")
		http.Error(w, "Invalid MAC encoding", http.StatusForbidden)
		return
	}
	log.Debugf("Provided MAC: %x", providedMAC)

	// Validate the HMAC
	if !hmac.Equal(calculatedMAC, providedMAC) {
		log.Warn("Invalid MAC")
		http.Error(w, "Invalid MAC", http.StatusForbidden)
		return
	}
	log.Debug("HMAC validation successful")

	// Validate file extension
	if !isExtensionAllowed(fileStorePath) {
		log.WithFields(logrus.Fields{
			// No need to sanitize and validate the file path here since absFilename is already sanitized in handleRequest
			"file":  fileStorePath,
			"error": err,
		}).Warn("Invalid file path")
		http.Error(w, "Invalid file path", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}
	// absFilename = sanitizedFilename

	// Check if there is enough free space
	err = checkStorageSpace(conf.Server.StoragePath, conf.Server.MinFreeBytes)
	if err != nil {
		log.WithFields(logrus.Fields{
			"storage_path": conf.Server.StoragePath,
			"error":        err,
		}).Warn("Not enough free space")
		http.Error(w, "Not enough free space", http.StatusInsufficientStorage)
		uploadErrorsTotal.Inc()
		return
	}

	// Create an UploadTask with a result channel
	result := make(chan error)
	task := UploadTask{
		AbsFilename: absFilename,
		Request:     r,
		Result:      result,
	}

	// Submit task to the upload queue
	select {
	case uploadQueue <- task:
		// Successfully added to the queue
		log.Debug("Upload task enqueued successfully")
	default:
		// Queue is full
		log.Warn("Upload queue is full. Rejecting upload")
		http.Error(w, "Server busy. Try again later.", http.StatusServiceUnavailable)
		uploadErrorsTotal.Inc()
		return
	}

	// Wait for the worker to process the upload
	err = <-result
	if err != nil {
		// The worker has already logged the error; send an appropriate HTTP response
		http.Error(w, fmt.Sprintf("Upload failed: %v", err), http.StatusInternalServerError)
		return
	}

	// Upload was successful
	w.WriteHeader(http.StatusCreated)
}

// Handle file downloads
func handleDownload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string) {
	fileInfo, err := getFileInfo(absFilename)
	if err != nil {
		log.WithError(err).Error("Failed to get file information")
		http.Error(w, "Not Found", http.StatusNotFound)
		downloadErrorsTotal.Inc()
		return
	} else if fileInfo.IsDir() {
		log.Warn("Directory listing forbidden")
		http.Error(w, "Forbidden", http.StatusForbidden)
		downloadErrorsTotal.Inc()
		return
	}

	contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
	if contentType == "" {
		contentType = "application/octet-stream"
	}
	w.Header().Set("Content-Type", contentType)

	// Handle resumable downloads
	if conf.Uploads.ResumableUploadsEnabled {
		handleResumableDownload(absFilename, w, r, fileInfo.Size())
		return
	}

	if r.Method == http.MethodHead {
		w.Header().Set("Content-Length", strconv.FormatInt(fileInfo.Size(), 10))
		downloadsTotal.Inc()
		return
	} else {
		// Measure download duration
		startTime := time.Now()
		log.Infof("Initiating download for file: %s", absFilename)
		http.ServeFile(w, r, absFilename)
		downloadDuration.Observe(time.Since(startTime).Seconds())
		downloadSizeBytes.Observe(float64(fileInfo.Size()))
		downloadsTotal.Inc()
		log.Infof("File downloaded successfully: %s", absFilename)
		return
	}
}

// Improved createFile function with proper resource management and larger buffer size
func createFile(tempFilename string, r *http.Request) error {
    absDirectory := filepath.Dir(tempFilename)
    err := os.MkdirAll(absDirectory, os.ModePerm)
    if err != nil {
        return fmt.Errorf("failed to create directory: %v", err)
    }

    // Open the file for writing
    targetFile, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
    if err != nil {
        return fmt.Errorf("failed to open file: %v", err)
    }
    defer targetFile.Close()

    // Use a larger buffer for efficient file writing
    bufferSize := 8 * 1024 * 1024 // 8 MB buffer
    writer := bufio.NewWriterSize(targetFile, bufferSize)
    buffer := make([]byte, bufferSize)

    totalBytes := int64(0)
    for {
        n, err := r.Body.Read(buffer)
        if err != nil && err != io.EOF {
            return fmt.Errorf("failed to read request body: %v", err)
        }
        if n == 0 {
            break
        }

        _, err = writer.Write(buffer[:n])
        if err != nil {
            return fmt.Errorf("failed to write to file: %v", err)
        }
        totalBytes += int64(n)
    }

    err = writer.Flush()
    if err != nil {
        return fmt.Errorf("failed to flush writer: %v", err)
    }

    log.WithFields(logrus.Fields{
        "temp_file":   tempFilename,
        "total_bytes": totalBytes,
    }).Info("File uploaded successfully")

    uploadSizeBytes.Observe(float64(totalBytes))
    return nil
}

// Scan the uploaded file with ClamAV (Optional)
func scanFileWithClamAV(filePath string) error {
	log.WithField("file", filePath).Info("Scanning file with ClamAV")

	scanResultChan, err := clamClient.ScanFile(filePath)
	if err != nil {
		log.WithError(err).Error("Failed to initiate ClamAV scan")
		return fmt.Errorf("failed to initiate ClamAV scan: %w", err)
	}

	// Receive scan result
	scanResult := <-scanResultChan
	if scanResult == nil {
		log.Error("Failed to receive scan result from ClamAV")
		return fmt.Errorf("failed to receive scan result from ClamAV")
	}

	// Handle scan result
	switch scanResult.Status {
	case clamd.RES_OK:
		log.WithField("file", filePath).Info("ClamAV scan passed")
		return nil
	case clamd.RES_FOUND:
		log.WithFields(logrus.Fields{
			"file":        filePath,
			"description": scanResult.Description,
		}).Warn("ClamAV detected a virus")
		return fmt.Errorf("virus detected: %s", scanResult.Description)
	default:
		log.WithFields(logrus.Fields{
			"file":        filePath,
			"status":      scanResult.Status,
			"description": scanResult.Description,
		}).Warn("ClamAV scan returned unexpected status")
		return fmt.Errorf("ClamAV scan returned unexpected status: %s", scanResult.Description)
	}
}

// initClamAV initializes the ClamAV client and logs the status
func initClamAV(socket string) (*clamd.Clamd, error) {
	if socket == "" {
		log.Error("ClamAV socket path is not configured.")
		return nil, fmt.Errorf("ClamAV socket path is not configured")
	}

	clamClient := clamd.NewClamd("unix:" + socket)
	err := clamClient.Ping()
	if err != nil {
		log.Errorf("Failed to connect to ClamAV at %s: %v", socket, err)
		return nil, fmt.Errorf("failed to connect to ClamAV: %w", err)
	}

	log.Info("Connected to ClamAV successfully.")
	return clamClient, nil
}

// Handle resumable downloads
func handleResumableDownload(absFilename string, w http.ResponseWriter, r *http.Request, fileSize int64) {
	rangeHeader := r.Header.Get("Range")
	if rangeHeader == "" {
		// If no Range header, serve the full file
		startTime := time.Now()
		http.ServeFile(w, r, absFilename)
		downloadDuration.Observe(time.Since(startTime).Seconds())
		downloadSizeBytes.Observe(float64(fileSize))
		downloadsTotal.Inc()
		return
	}

	// Parse Range header
	ranges := strings.Split(strings.TrimPrefix(rangeHeader, "bytes="), "-")
	if len(ranges) != 2 {
		http.Error(w, "Invalid Range", http.StatusRequestedRangeNotSatisfiable)
		downloadErrorsTotal.Inc()
		return
	}

	start, err := strconv.ParseInt(ranges[0], 10, 64)
	if err != nil {
		http.Error(w, "Invalid Range", http.StatusRequestedRangeNotSatisfiable)
		downloadErrorsTotal.Inc()
		return
	}

	// Calculate end byte
	end := fileSize - 1
	if ranges[1] != "" {
		end, err = strconv.ParseInt(ranges[1], 10, 64)
		if err != nil || end >= fileSize {
			http.Error(w, "Invalid Range", http.StatusRequestedRangeNotSatisfiable)
			downloadErrorsTotal.Inc()
			return
		}
	}

	// Set response headers for partial content
	w.Header().Set("Content-Range", fmt.Sprintf("bytes %d-%d/%d", start, end, fileSize))
	w.Header().Set("Content-Length", strconv.FormatInt(end-start+1, 10))
	w.Header().Set("Accept-Ranges", "bytes")
	w.WriteHeader(http.StatusPartialContent)

	// Serve the requested byte range
	file, err := os.Open(absFilename)
	if err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}
	defer file.Close()

	// Seek to the start byte
	_, err = file.Seek(start, 0)
	if err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}

	// Create a buffer and copy the specified range to the response writer
	buffer := make([]byte, 32*1024) // 32KB buffer
	remaining := end - start + 1
	startTime := time.Now()
	for remaining > 0 {
		if int64(len(buffer)) > remaining {
			buffer = buffer[:remaining]
		}
		n, err := file.Read(buffer)
		if n > 0 {
			if _, writeErr := w.Write(buffer[:n]); writeErr != nil {
				log.WithError(writeErr).Error("Failed to write to response")
				downloadErrorsTotal.Inc()
				return
			}
			remaining -= int64(n)
		}
		if err != nil {
			if err != io.EOF {
				log.WithError(err).Error("Error reading file during resumable download")
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
				downloadErrorsTotal.Inc()
			}
			break
		}
	}
	downloadDuration.Observe(time.Since(startTime).Seconds())
	downloadSizeBytes.Observe(float64(end - start + 1))
	downloadsTotal.Inc()
}

// Handle chunked uploads with bufio.Writer
func handleChunkedUpload(tempFilename string, r *http.Request) error {
	log.WithField("file", tempFilename).Info("Handling chunked upload to temporary file")

	// Ensure the directory exists
	absDirectory := filepath.Dir(tempFilename)
	err := os.MkdirAll(absDirectory, os.ModePerm)
	if err != nil {
		return fmt.Errorf("failed to create directory: %v", err)
	}

	targetFile, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return fmt.Errorf("failed to open file: %v", err)
	}
	defer targetFile.Close()

	writer := bufio.NewWriterSize(targetFile, int(conf.Uploads.ChunkSize))
	buffer := make([]byte, conf.Uploads.ChunkSize)

	totalBytes := int64(0)
	for {
		n, err := r.Body.Read(buffer)
		if err != nil && err != io.EOF {
			return fmt.Errorf("failed to read request body: %v", err)
		}
		if n == 0 {
			break
		}

		_, err = writer.Write(buffer[:n])
		if err != nil {
			return fmt.Errorf("failed to write to file: %v", err)
		}
		totalBytes += int64(n)
	}

	err = writer.Flush()
	if err != nil {
		return fmt.Errorf("failed to flush writer: %v", err)
	}

	log.WithFields(logrus.Fields{
		"temp_file":   tempFilename,
		"total_bytes": totalBytes,
	}).Info("Chunked upload completed successfully")

	uploadSizeBytes.Observe(float64(totalBytes))
	return nil
}

// Get file information with caching
func getFileInfo(absFilename string) (os.FileInfo, error) {
	if cachedInfo, found := fileInfoCache.Get(absFilename); found {
		if info, ok := cachedInfo.(os.FileInfo); ok {
			return info, nil
		}
	}

	fileInfo, err := os.Stat(absFilename)
	if err != nil {
		return nil, err
	}

	fileInfoCache.Set(absFilename, fileInfo, cache.DefaultExpiration)
	return fileInfo, nil
}

// Monitor network changes
func monitorNetwork(ctx context.Context) {
	currentIP := getCurrentIPAddress() // Placeholder for the current IP address

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping network monitor.")
			return
		case <-time.After(10 * time.Second):
			newIP := getCurrentIPAddress()
			if newIP != currentIP && newIP != "" {
				currentIP = newIP
				select {
				case networkEvents <- NetworkEvent{Type: "IP_CHANGE", Details: currentIP}:
					log.WithField("new_ip", currentIP).Info("Queued IP_CHANGE event")
				default:
					log.Warn("Network event channel is full. Dropping IP_CHANGE event.")
				}
			}
		}
	}
}

// Handle network events
func handleNetworkEvents(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping network event handler.")
			return
		case event, ok := <-networkEvents:
			if !ok {
				log.Info("Network events channel closed.")
				return
			}
			switch event.Type {
			case "IP_CHANGE":
				log.WithField("new_ip", event.Details).Info("Network change detected")
				// Example: Update Prometheus gauge or trigger alerts
				// activeConnections.Set(float64(getActiveConnections()))
			}
			// Additional event types can be handled here
		}
	}
}

// Get current IP address (example)
func getCurrentIPAddress() string {
	interfaces, err := net.Interfaces()
	if err != nil {
		log.WithError(err).Error("Failed to get network interfaces")
		return ""
	}

	for _, iface := range interfaces {
		if iface.Flags&net.FlagUp == 0 || iface.Flags&net.FlagLoopback != 0 {
			continue // Skip interfaces that are down or loopback
		}
		addrs, err := iface.Addrs()
		if err != nil {
			log.WithError(err).Errorf("Failed to get addresses for interface %s", iface.Name)
			continue
		}
		for _, addr := range addrs {
			if ipnet, ok := addr.(*net.IPNet); ok && ipnet.IP.IsGlobalUnicast() && ipnet.IP.To4() != nil {
				return ipnet.IP.String()
			}
		}
	}
	return ""
}

// setupGracefulShutdown sets up handling for graceful server shutdown
func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-quit
		log.Infof("Received signal %s. Initiating shutdown...", sig)

		// Create a deadline to wait for.
		ctxShutdown, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()

		// Attempt graceful shutdown
		if err := server.Shutdown(ctxShutdown); err != nil {
			log.Errorf("Server shutdown failed: %v", err)
		} else {
			log.Info("Server shutdown gracefully.")
		}

		// Signal other goroutines to stop
		cancel()

		// Close the upload, scan, and network event channels
		close(uploadQueue)
		log.Info("Upload queue closed.")
		close(scanQueue)
		log.Info("Scan queue closed.")
		close(networkEvents)
		log.Info("Network events channel closed.")

		log.Info("Shutdown process completed. Exiting application.")
		os.Exit(0)
	}()
}

// Initialize Redis client
func initRedis() {
	if !conf.Redis.RedisEnabled {
		log.Info("Redis is disabled in configuration.")
		return
	}

	redisClient = redis.NewClient(&redis.Options{
		Addr:     conf.Redis.RedisAddr,
		Password: conf.Redis.RedisPassword,
		DB:       conf.Redis.RedisDBIndex,
	})

	// Test the Redis connection
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	_, err := redisClient.Ping(ctx).Result()
	if err != nil {
		log.Fatalf("Failed to connect to Redis: %v", err)
	}
	log.Info("Connected to Redis successfully")

	// Set initial connection status
	mu.Lock()
	redisConnected = true
	mu.Unlock()

	// Start monitoring Redis health
	go MonitorRedisHealth(context.Background(), redisClient, parseDuration(conf.Redis.RedisHealthCheckInterval))
}

// MonitorRedisHealth periodically checks Redis connectivity and updates redisConnected status.
func MonitorRedisHealth(ctx context.Context, client *redis.Client, checkInterval time.Duration) {
	ticker := time.NewTicker(checkInterval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping Redis health monitor.")
			return
		case <-ticker.C:
			err := client.Ping(ctx).Err()
			mu.Lock()
			if err != nil {
				if redisConnected {
					log.Errorf("Redis health check failed: %v", err)
				}
				redisConnected = false
			} else {
				if !redisConnected {
					log.Info("Redis reconnected successfully")
				}
				redisConnected = true
				log.Debug("Redis health check succeeded.")
			}
			mu.Unlock()
		}
	}
}

// Helper function to parse duration strings
func parseDuration(durationStr string) time.Duration {
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		log.WithError(err).Warn("Invalid duration format, using default 30s")
		return 30 * time.Second
	}
	return duration
}

// RunFileCleaner periodically deletes files that exceed the FileTTL duration.
func runFileCleaner(ctx context.Context, storeDir string, ttl time.Duration) {
	ticker := time.NewTicker(1 * time.Hour)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping file cleaner.")
			return
		case <-ticker.C:
			now := time.Now()
			err := filepath.Walk(storeDir, func(path string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if info.IsDir() {
					return nil
				}
				if now.Sub(info.ModTime()) > ttl {
					err := os.Remove(path)
					if err != nil {
						log.WithError(err).Errorf("Failed to remove expired file: %s", path)
					} else {
						log.Infof("Removed expired file: %s", path)
					}
				}
				return nil
			})
			if err != nil {
				log.WithError(err).Error("Error walking store directory for file cleaning")
			}
		}
	}
}

// DeduplicateFiles scans the store directory and removes duplicate files based on SHA256 hash.
// It retains one copy of each unique file and replaces duplicates with hard links.
func DeduplicateFiles(storeDir string) error {
	hashMap := make(map[string]string) // map[hash]filepath
	var mu sync.Mutex
	var wg sync.WaitGroup
	fileChan := make(chan string, 100)

	// Worker to process files
	numWorkers := 10
	for i := 0; i < numWorkers; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for filePath := range fileChan {
				hash, err := computeFileHash(filePath)
				if err != nil {
					logrus.WithError(err).Errorf("Failed to compute hash for %s", filePath)
					continue
				}

				mu.Lock()
				original, exists := hashMap[hash]
				if !exists {
					hashMap[hash] = filePath
					mu.Unlock()
					continue
				}
				mu.Unlock()

				// Duplicate found
				err = os.Remove(filePath)
				if err != nil {
					logrus.WithError(err).Errorf("Failed to remove duplicate file %s", filePath)
					continue
				}

				// Create hard link to the original file
				err = os.Link(original, filePath)
				if err != nil {
					logrus.WithError(err).Errorf("Failed to create hard link from %s to %s", original, filePath)
					continue
				}

				logrus.Infof("Removed duplicate %s and linked to %s", filePath, original)
			}
		}()
	}

	// Walk through the store directory
	err := filepath.Walk(storeDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			logrus.WithError(err).Errorf("Error accessing path %s", path)
			return nil
		}
		if !info.Mode().IsRegular() {
			return nil
		}
		fileChan <- path
		return nil
	})
	if err != nil {
		return fmt.Errorf("error walking the path %s: %w", storeDir, err)
	}

	close(fileChan)
	wg.Wait()
	return nil
}

// computeFileHash computes the SHA256 hash of the given file.
func computeFileHash(filePath string) (string, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return "", fmt.Errorf("unable to open file %s: %w", filePath, err)
	}
	defer file.Close()

	hasher := sha256.New()
	if _, err := io.Copy(hasher, file); err != nil {
		return "", fmt.Errorf("error hashing file %s: %w", filePath, err)
	}

	return hex.EncodeToString(hasher.Sum(nil)), nil
}

// handleMultipartUpload handles multipart uploads
func handleMultipartUpload(w http.ResponseWriter, r *http.Request, absFilename string) error {
	err := r.ParseMultipartForm(32 << 20) // 32MB is the default used by FormFile
	if err != nil {
		log.WithError(err).Error("Failed to parse multipart form")
		http.Error(w, "Failed to parse multipart form", http.StatusBadRequest)
		return err
	}

	file, handler, err := r.FormFile("file")
	if err != nil {
		log.WithError(err).Error("Failed to retrieve file from form data")
		http.Error(w, "Failed to retrieve file from form data", http.StatusBadRequest)
		return err
	}
	defer file.Close()

	// Validate file extension
	if !isExtensionAllowed(handler.Filename) {
		log.WithFields(logrus.Fields{
			"filename":  handler.Filename,
			"extension": filepath.Ext(handler.Filename),
		}).Warn("Attempted upload with disallowed file extension")
		http.Error(w, "Disallowed file extension. Allowed extensions are: "+strings.Join(conf.Uploads.AllowedExtensions, ", "), http.StatusForbidden)
		uploadErrorsTotal.Inc()
		return fmt.Errorf("disallowed file extension")
	}

	// Create a temporary file
	tempFilename := absFilename + ".tmp"
	tempFile, err := os.OpenFile(tempFilename, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		log.WithError(err).Error("Failed to create temporary file")
		http.Error(w, "Failed to create temporary file", http.StatusInternalServerError)
		return err
	}
	defer tempFile.Close()

	// Copy the uploaded file to the temporary file
	_, err = io.Copy(tempFile, file)
	if err != nil {
		log.WithError(err).Error("Failed to copy uploaded file to temporary file")
		http.Error(w, "Failed to copy uploaded file", http.StatusInternalServerError)
		return err
	}

	// Perform ClamAV scan on the temporary file
	if clamClient != nil {
		err := scanFileWithClamAV(tempFilename)
		if err != nil {
			log.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Warn("ClamAV detected a virus or scan failed")
			os.Remove(tempFilename)
			uploadErrorsTotal.Inc()
			return err
		}
	}

	// Handle file versioning if enabled
	if conf.Versioning.EnableVersioning {
		existing, _ := fileExists(absFilename)
		if existing {
			err := versionFile(absFilename)
			if err != nil {
				log.WithFields(logrus.Fields{
					"file":  absFilename,
					"error": err,
				}).Error("Error versioning file")
				os.Remove(tempFilename)
				return err
			}
		}
	}

	// Move the temporary file to the final destination
	err = os.Rename(tempFilename, absFilename)
	if err != nil {
		log.WithFields(logrus.Fields{
			"temp_file":  tempFilename,
			"final_file": absFilename,
			"error":      err,
		}).Error("Failed to move file to final destination")
		os.Remove(tempFilename)
		return err
	}

	log.WithFields(logrus.Fields{
		"file": absFilename,
	}).Info("File uploaded and scanned successfully")

	uploadsTotal.Inc()
	return nil
}

// sanitizeFilePath ensures that the file path is within the designated storage directory
func sanitizeFilePath(baseDir, filePath string) (string, error) {
	// Resolve the absolute path
	absBaseDir, err := filepath.Abs(baseDir)
	if err != nil {
		return "", fmt.Errorf("failed to resolve base directory: %w", err)
	}

	absFilePath, err := filepath.Abs(filepath.Join(absBaseDir, filePath))
	if err != nil {
		return "", fmt.Errorf("failed to resolve file path: %w", err)
	}

	// Check if the resolved file path is within the base directory
	if !strings.HasPrefix(absFilePath, absBaseDir) {
		return "", fmt.Errorf("invalid file path: %s", filePath)
	}

	return absFilePath, nil
}

// checkStorageSpace ensures that there is enough free space in the storage path
func checkStorageSpace(storagePath string, minFreeBytes int64) error {
	var stat syscall.Statfs_t
	err := syscall.Statfs(storagePath, &stat)
	if err != nil {
		return fmt.Errorf("failed to get filesystem stats: %w", err)
	}

	// Calculate available bytes
	availableBytes := stat.Bavail * uint64(stat.Bsize)
	if int64(availableBytes) < minFreeBytes {
		return fmt.Errorf("not enough free space: %d bytes available, %d bytes required", availableBytes, minFreeBytes)
	}

	return nil
}

// Helper function to create formatted errors

// handleDeduplication handles file deduplication using SHA256 checksum and hard links
func handleDeduplication(ctx context.Context, absFilename string) error {
	// Compute checksum of the uploaded file
	checksum, err := computeSHA256(ctx, absFilename)
	if err != nil {
		log.Errorf("Failed to compute SHA256 for %s: %v", absFilename, err)
		return fmt.Errorf("checksum computation failed: %w", err)
	}
	log.Debugf("Computed checksum for %s: %s", absFilename, checksum)

	// Check Redis for existing checksum
	existingPath, err := redisClient.Get(ctx, checksum).Result()
	if err != nil {
		if err == redis.Nil {
			// Checksum does not exist, store it in Redis
			err = redisClient.Set(ctx, checksum, absFilename, 0).Err()
			if err != nil {
				log.Errorf("Redis error while setting checksum %s: %v", checksum, err)
				return fmt.Errorf("redis error: %w", err)
			}
			log.Infof("Stored new checksum %s for file %s", checksum, absFilename)
			return nil
		}
		// Handle other Redis errors
		log.Errorf("Redis error while fetching checksum %s: %v", checksum, err)
		return fmt.Errorf("redis error: %w", err)
	}

	// Checksum exists, create a hard link to the existing file
	if existingPath != absFilename {
		// Verify that existingPath exists before creating a hard link
		if _, err := os.Stat(existingPath); os.IsNotExist(err) {
			log.Errorf("Existing file for checksum %s does not exist at path %s", checksum, existingPath)
			return fmt.Errorf("existing file does not exist: %w", err)
		}

		// Attempt to create a hard link
		err = os.Link(existingPath, absFilename)
		if err != nil {
			log.Errorf("Failed to create hard link from %s to %s: %v", existingPath, absFilename, err)
			return fmt.Errorf("failed to create hard link: %w", err)
		}
		log.Infof("Created hard link for duplicate file %s pointing to %s", absFilename, existingPath)
	} else {
		log.Infof("File %s already exists with the same checksum", absFilename)
	}

	return nil
}

// Usage in checkFreeSpace function

// Function to compute SHA256 with context support
func computeSHA256(ctx context.Context, filePath string) (string, error) {
	if filePath == "" {
		return "", fmt.Errorf("computeSHA256: filePath cannot be empty")
	}

	file, err := os.Open(filePath)
	if err != nil {
		log.Errorf("Failed to open file for checksum: %v", err)
		return "", fmt.Errorf("computeSHA256: failed to open file %s: %w", filePath, err)
	}
	defer file.Close()

	hasher := sha256.New()
	reader := bufio.NewReader(file)

	buffer := make([]byte, 4096)
	for {
		select {
		case <-ctx.Done():
			return "", fmt.Errorf("computeSHA256: operation cancelled")
		default:
			n, err := reader.Read(buffer)
			if n > 0 {
				if _, wErr := hasher.Write(buffer[:n]); wErr != nil {
					return "", fmt.Errorf("computeSHA256: failed to write to hasher: %w", wErr)
				}
			}
			if err != nil {
				if err == io.EOF {
					checksum := hex.EncodeToString(hasher.Sum(nil))
					log.Debugf("Computed SHA256 checksum for file %s: %s", filePath, checksum)
					return checksum, nil
				}
				return "", fmt.Errorf("computeSHA256: read error: %w", err)
			}
		}
	}
}

// Function to check free space with retry mechanism
func checkFreeSpaceWithRetry(path string, retries int, delay time.Duration) error {
	for i := 0; i < retries; i++ {
		if err := checkStorageSpace(path, MinFreeBytes); err != nil {
			log.Warnf("Free space check failed (attempt %d/%d): %v", i+1, retries, err)
			time.Sleep(delay)
			continue
		}
		return nil
	}
	return fmt.Errorf("checkFreeSpace: insufficient free space after %d attempts", retries)
}

// CreateISOContainer creates an ISO container with the specified size
func CreateISOContainer(files []string, isoPath string, size string, charset string) error {
	args := []string{"-o", isoPath, "-V", "ISO_CONTAINER", "-J", "-R", "-input-charset", charset}
	args = append(args, files...)
	cmd := exec.Command("genisoimage", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// Improved logging and error handling
func MountISOContainer(isoPath string, mountPoint string) error {
	// Ensure the mount point directory exists
	if err := os.MkdirAll(mountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point directory: %w", err)
	}

	var output []byte
	var err error
	for i := 0; i < 3; i++ {
		cmd := exec.Command("mount", "-o", "loop,ro", isoPath, mountPoint)
		output, err = cmd.CombinedOutput()
		if err == nil {
			log.Infof("ISO container mounted successfully at %s", mountPoint)
			return nil
		}
		log.Warnf("Failed to mount ISO container (attempt %d/3): %v, output: %s", i+1, err, string(output))
		time.Sleep(2 * time.Second)
	}
	return fmt.Errorf("failed to mount ISO container: %w, output: %s", err, string(output))
}

// UnmountISOContainer unmounts the ISO container from the specified mount point
func UnmountISOContainer(mountPoint string) error {
	cmd := exec.Command("umount", mountPoint)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func handleISOContainer(absFilename string) error {
	isoPath := filepath.Join(conf.ISO.MountPoint, "container.iso")

	// Create ISO container
	err := CreateISOContainer([]string{absFilename}, isoPath, conf.ISO.Size, conf.ISO.Charset)
	if err != nil {
		return fmt.Errorf("failed to create ISO container: %w", err)
	}

	// Ensure the mount point directory exists
	if err := os.MkdirAll(conf.ISO.MountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point directory: %w", err)
	}

	// Mount ISO container
	err = MountISOContainer(isoPath, conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}

	// Unmount ISO container (example)
	err = UnmountISOContainer(conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to unmount ISO container: %w", err)
	}

	return nil
}

// Verify and create ISO container if it doesn't exist
func verifyAndCreateISOContainer() error {
	isoPath := filepath.Join(conf.ISO.MountPoint, "container.iso")

	// Check if ISO file exists
	if exists, _ := fileExists(isoPath); !exists {
		log.Infof("ISO container does not exist. Creating new ISO container at %s", isoPath)

		// Example files to include in the ISO container
		files := []string{conf.Server.StoragePath}

		// Create ISO container
		err := CreateISOContainer(files, isoPath, conf.ISO.Size, conf.ISO.Charset)
		if err != nil {
			return fmt.Errorf("failed to create ISO container: %w", err)
		}
		log.Infof("ISO container created successfully at %s", isoPath)
	}

	// Verify ISO file consistency
	err := verifyISOFile(isoPath)
	if err != nil {
		// Handle corrupted ISO file
		files := []string{conf.Server.StoragePath}
		err = handleCorruptedISOFile(isoPath, files, conf.ISO.Size, conf.ISO.Charset)
		if err != nil {
			return fmt.Errorf("failed to handle corrupted ISO file: %w", err)
		}
	}

	// Ensure the mount point directory exists
	if err := os.MkdirAll(conf.ISO.MountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point directory: %w", err)
	}

	// Mount ISO container
	err = MountISOContainer(isoPath, conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}
	log.Infof("ISO container mounted successfully at %s", conf.ISO.MountPoint)

	return nil
}

// Verify ISO file consistency using a checksum
func verifyISOFile(isoPath string) error {
	cmd := exec.Command("isoinfo", "-i", isoPath, "-d")
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("failed to verify ISO file: %w, output: %s", err, string(output))
	}
	return nil
}

// Handle corrupted ISO file by recreating it
func handleCorruptedISOFile(isoPath string, files []string, size string, charset string) error {
	log.Warnf("ISO file %s is corrupted. Recreating it.", isoPath)
	err := CreateISOContainer(files, isoPath, size, charset)
	if err != nil {
		return fmt.Errorf("failed to recreate ISO container: %w", err)
	}
	return nil
}
