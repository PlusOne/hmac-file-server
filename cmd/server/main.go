// main.go

package main

import (
	"bufio"
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
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
	"github.com/lestrrat-go/file-rotatelogs"
	"github.com/patrickmn/go-cache"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/mem"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

// Configuration structures
type ServerConfig struct {
	ListenPort           string `mapstructure:"ListenPort"`
	UnixSocket           bool   `mapstructure:"UnixSocket"`
	StoragePath          string `mapstructure:"StoragePath"`
	LogLevel             string `mapstructure:"LogLevel"`
	LogFile              string `mapstructure:"LogFile"`
	MetricsEnabled       bool   `mapstructure:"MetricsEnabled"`
	MetricsPort          string `mapstructure:"MetricsPort"`
	FileTTL              string `mapstructure:"FileTTL"`
	MinFreeBytes         string `mapstructure:"MinFreeBytes"`
	DeduplicationEnabled bool   `mapstructure:"DeduplicationEnabled"`
	AutoAdjustWorkers    bool   `mapstructure:"AutoAdjustWorkers"`
	NetworkEvents        bool   `mapstructure:"NetworkEvents"`
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
	ChunkSize               string   `mapstructure:"ChunkSize"`
	AllowedExtensions       []string `mapstructure:"AllowedExtensions"`
}

type ClamAVConfig struct {
	ClamAVEnabled      bool     `mapstructure:"ClamAVEnabled"`
	ClamAVSocket       string   `mapstructure:"ClamAVSocket"`
	NumScanWorkers     int      `mapstructure:"NumScanWorkers"`
	ScanFileExtensions []string `mapstructure:"ScanFileExtensions"`
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

type ISOConfig struct {
	Enabled    bool   `mapstructure:"enabled"`
	Size       string `mapstructure:"size"`
	MountPoint string `mapstructure:"mountpoint"`
	Charset    string `mapstructure:"charset"`
}

type PasteConfig struct {
	Enabled     bool   `mapstructure:"enabled"`
	StoragePath string `mapstructure:"storagePath"`
}

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
	Paste      PasteConfig      `mapstructure:"paste"`
}

type UploadTask struct {
	AbsFilename string
	Request     *http.Request
	Result      chan error
}

type ScanTask struct {
	AbsFilename string
	Result      chan error
}

type NetworkEvent struct {
	Type    string
	Details string
}

var (
	conf           Config
	versionString  string = "v2.0-stable"
	logger         = logrus.New()
	uploadQueue    chan UploadTask
	scanQueue      chan ScanTask
	networkEvents  chan NetworkEvent
	fileInfoCache  *cache.Cache
	clamClient     *clamd.Clamd
	redisClient    *redis.Client
	redisConnected bool
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

	bufferPool = sync.Pool{
		New: func() interface{} {
			buf := make([]byte, 32*1024)
			return &buf
		},
	}

	const (
		MinFreeBytes = 1 << 30 // 1 GB
	)

	// Semaphore to limit concurrent operations
	semaphore = make(chan struct{}, 10)

	// WaitGroup to manage goroutines
	wg sync.WaitGroup
)

func main() {
	setDefaults()

	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	err := readConfig(configFile, &conf)
	if err != nil {
		log.Fatalf("Error reading config: %v", err)
	}
	logrus.Info("Configuration loaded successfully.")

	initializeWorkerSettings(&conf.Server, &conf.Workers, &conf.ClamAV)

	if conf.ISO.Enabled {
		err = verifyAndCreateISOContainer()
		if err != nil {
			logrus.Fatalf("ISO container verification failed: %v", err)
		}
	}

	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)

	err = os.MkdirAll(conf.Server.StoragePath, os.ModePerm)
	if err != nil {
		logrus.Fatalf("Error creating store directory: %v", err)
	}
	logrus.WithField("directory", conf.Server.StoragePath).Info("Store directory is ready")

	err = checkFreeSpaceWithRetry(conf.Server.StoragePath, 3, 5*time.Second)
	if err != nil {
		logrus.Fatalf("Insufficient free space: %v", err)
	}

	setupLogging()
	logSystemInfo()
	initMetrics()
	logrus.Info("Prometheus metrics initialized.")

	uploadQueue = make(chan UploadTask, conf.Workers.UploadQueueSize)
	scanQueue = make(chan ScanTask, conf.Workers.UploadQueueSize)
	networkEvents = make(chan NetworkEvent, 100)
	logrus.Info("Upload, scan, and network event channels initialized.")

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Start monitoring network and handling events
	wg.Add(2)
	go monitorNetwork(ctx)
	go handleNetworkEvents(ctx)
	wg.Wait()

	go updateSystemMetrics(ctx)

	// Initialize ClamAV if enabled
	if conf.ClamAV.ClamAVEnabled {
		clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			logrus.WithError(err).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
		} else {
			logrus.Info("ClamAV client initialized successfully.")
		}
	}

	// Initialize Redis if enabled
	if conf.Redis.RedisEnabled {
		initRedis()
	}

	// Initialize worker pools
	initializeUploadWorkerPool(ctx, &conf.Workers)
	if conf.ClamAV.ClamAVEnabled && clamClient != nil {
		initializeScanWorkerPool(ctx)
	}

	// Start Redis health monitoring
	if conf.Redis.RedisEnabled && redisClient != nil {
		wg.Add(1)
		go MonitorRedisHealth(ctx, redisClient, parseDuration(conf.Redis.RedisHealthCheckInterval))
	}

	router := setupRouter()

	// Start file cleaner
	fileTTL, err := parseTTL(conf.Server.FileTTL)
	if err != nil {
		logrus.Fatalf("Invalid FileTTL: %v", err)
	}
	wg.Add(1)
	go runFileCleaner(ctx, conf.Server.StoragePath, fileTTL)

	// Parse server timeouts
	readTimeout, err := time.ParseDuration(conf.Timeouts.ReadTimeout)
	if err != nil {
		logrus.Fatalf("Invalid ReadTimeout: %v", err)
	}

	writeTimeout, err := time.ParseDuration(conf.Timeouts.WriteTimeout)
	if err != nil {
		logrus.Fatalf("Invalid WriteTimeout: %v", err)
	}

	idleTimeout, err := time.ParseDuration(conf.Timeouts.IdleTimeout)
	if err != nil {
		logrus.Fatalf("Invalid IdleTimeout: %v", err)
	}

	server := &http.Server{
		Addr:           ":" + conf.Server.ListenPort,
		Handler:        router,
		ReadTimeout:    readTimeout,
		WriteTimeout:   writeTimeout,
		IdleTimeout:    idleTimeout,
		MaxHeaderBytes: 1 << 20, // 1 MB
	}

	// Start metrics server if enabled
	if conf.Server.MetricsEnabled {
		wg.Add(1)
		go func() {
			defer wg.Done()
			http.Handle("/metrics", promhttp.Handler())
			logrus.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
			if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
				logrus.Fatalf("Metrics server failed: %v", err)
			}
		}()
	}

	// Setup graceful shutdown
	setupGracefulShutdown(server, cancel)

	// Auto-adjust workers if enabled
	if conf.Server.AutoAdjustWorkers {
		wg.Add(1)
		go monitorWorkerPerformance(ctx, &conf.Workers, &conf.ClamAV)
	}

	logrus.Infof("Starting HMAC file server %s...", versionString)
	if conf.Server.UnixSocket {
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
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			logrus.Fatalf("Server failed: %v", err)
		}
	}

	wg.Wait()
}

// setDefaults sets default configuration values
func setDefaults() {
	viper.SetDefault("server.ListenPort", "8080")
	viper.SetDefault("server.UnixSocket", false)
	viper.SetDefault("server.StoragePath", "./uploads")
	viper.SetDefault("server.LogLevel", "info")
	viper.SetDefault("server.LogFile", "")
	viper.SetDefault("server.MetricsEnabled", true)
	viper.SetDefault("server.MetricsPort", "9090")
	viper.SetDefault("server.FileTTL", "8760h") // 1 year
	viper.SetDefault("server.MinFreeBytes", "100MB")
	viper.SetDefault("server.AutoAdjustWorkers", true)
	viper.SetDefault("server.NetworkEvents", true)

	viper.SetDefault("timeouts.ReadTimeout", "4800s")
	viper.SetDefault("timeouts.WriteTimeout", "4800s")
	viper.SetDefault("timeouts.IdleTimeout", "4800s")

	viper.SetDefault("security.Secret", "")

	viper.SetDefault("versioning.EnableVersioning", false)
	viper.SetDefault("versioning.MaxVersions", 1)

	viper.SetDefault("uploads.ResumableUploadsEnabled", true)
	viper.SetDefault("uploads.ChunkedUploadsEnabled", true)
	viper.SetDefault("uploads.ChunkSize", "8192")
	viper.SetDefault("uploads.AllowedExtensions", []string{
		".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp",
		".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v",
		".3gp", ".3g2", ".mp3", ".ogg",
	})

	viper.SetDefault("clamav.ClamAVEnabled", true)
	viper.SetDefault("clamav.ClamAVSocket", "/var/run/clamav/clamd.ctl")
	viper.SetDefault("clamav.NumScanWorkers", 2)
	viper.SetDefault("clamav.ScanFileExtensions", []string{".exe", ".dll", ".bin"})

	viper.SetDefault("redis.RedisEnabled", true)
	viper.SetDefault("redis.RedisAddr", "localhost:6379")
	viper.SetDefault("redis.RedisPassword", "")
	viper.SetDefault("redis.RedisDBIndex", 0)
	viper.SetDefault("redis.RedisHealthCheckInterval", "120s")

	viper.SetDefault("workers.NumWorkers", 4)
	viper.SetDefault("workers.UploadQueueSize", 50)

	viper.SetDefault("iso.Enabled", true)
	viper.SetDefault("iso.Size", "1GB")
	viper.SetDefault("iso.MountPoint", "/mnt/iso")
	viper.SetDefault("iso.Charset", "utf-8")
}

// readConfig reads the configuration from the specified file and environment variables
func readConfig(configFilename string, conf *Config) error {
	viper.SetConfigFile(configFilename)
	viper.SetConfigType("toml")

	viper.AutomaticEnv()
	viper.SetEnvPrefix("HMAC")

	if err := viper.ReadInConfig(); err != nil {
		return fmt.Errorf("error reading config file: %w", err)
	}

	if err := viper.Unmarshal(conf); err != nil {
		return fmt.Errorf("unable to decode into struct: %w", err)
	}

	// Additional configuration validations
	if err := validateConfig(conf); err != nil {
		return fmt.Errorf("configuration validation failed: %w", err)
	}

	return nil
}

// validateConfig validates the loaded configuration
func validateConfig(conf *Config) error {
	if conf.Server.ListenPort == "" {
		return errors.New("ListenPort must be set")
	}
	if conf.Security.Secret == "" {
		return errors.New("security.Secret must be set (load from environment variable)")
	}
	if conf.Server.StoragePath == "" {
		return errors.New("StoragePath must be set")
	}
	if conf.Server.FileTTL == "" {
		return errors.New("FileTTL must be set")
	}

	if _, err := parseTTL(conf.Server.FileTTL); err != nil {
		return fmt.Errorf("invalid FileTTL: %w", err)
	}
	if _, err := parseSize(conf.Server.MinFreeBytes); err != nil {
		return fmt.Errorf("invalid MinFreeBytes: %w", err)
	}

	if conf.Redis.RedisEnabled {
		if conf.Redis.RedisAddr == "" {
			return errors.New("RedisAddr must be set when Redis is enabled")
		}
	}

	if conf.ISO.Enabled {
		if conf.ISO.Size == "" {
			return errors.New("ISO size must be set")
		}
		if conf.ISO.MountPoint == "" {
			return errors.New("ISO mount point must be set")
		}
		if conf.ISO.Charset == "" {
			return errors.New("ISO charset must be set")
		}
	}

	if conf.Paste.Enabled && conf.Paste.StoragePath == "" {
		return errors.New("paste is enabled but 'storagePath' is not set in '[paste]' section")
	}

	return nil
}

// setupLogging configures logrus with structured logging and log rotation
func setupLogging() {
	// Set log level
	level, err := logrus.ParseLevel(conf.Server.LogLevel)
	if err != nil {
		logrus.Fatalf("Invalid log level: %s", conf.Server.LogLevel)
	}
	logger.SetLevel(level)

	// Set log format to JSON for structured logging
	logger.SetFormatter(&logrus.JSONFormatter{})

	// Set up log rotation if LogFile is specified
	if conf.Server.LogFile != "" {
		writer, err := rotatelogs.New(
			conf.Server.LogFile+".%Y%m%d%H%M",
			rotatelogs.WithLinkName(conf.Server.LogFile),
			rotatelogs.WithMaxAge(7*24*time.Hour),
			rotatelogs.WithRotationTime(24*time.Hour),
		)
		if err != nil {
			logger.Fatalf("Failed to initialize log file rotator: %v", err)
		}
		logger.SetOutput(io.MultiWriter(os.Stdout, writer))
	} else {
		// Default to stdout
		logger.SetOutput(os.Stdout)
	}
}

// logSystemInfo logs detailed system information at startup
func logSystemInfo() {
	cpuInfo, _ := cpu.Info()
	memInfo, _ := mem.VirtualMemory()

	startupDetails := logrus.Fields{
		"version":     versionString,
		"build_date":  time.Now().Format(time.RFC3339),
		"features":    []string{"Prometheus Metrics", "Chunked Uploads", "ClamAV Scanning"},
		"os":          runtime.GOOS,
		"architecture": runtime.GOARCH,
		"cpu": logrus.Fields{
			"model": cpuInfo[0].ModelName,
			"cores": runtime.NumCPU(),
			"mhz":   cpuInfo[0].Mhz,
		},
		"memory": logrus.Fields{
			"total_mb": memInfo.Total / (1024 * 1024),
			"free_mb":  memInfo.Available / (1024 * 1024),
			"used_mb":  memInfo.Used / (1024 * 1024),
		},
	}

	logger.WithFields(startupDetails).Info("System initialized")
}

// initMetrics initializes Prometheus metrics
func initMetrics() {
	uploadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_upload_duration_seconds",
		Help:      "Histogram of file upload durations.",
	})
	uploadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "file_server_upload_errors_total",
		Help:      "Total number of file upload errors.",
	})
	uploadsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "file_server_uploads_total",
		Help:      "Total number of successful file uploads.",
	})
	downloadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_download_duration_seconds",
		Help:      "Histogram of file download durations.",
	})
	downloadsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "file_server_downloads_total",
		Help:      "Total number of successful file downloads.",
	})
	downloadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "file_server_download_errors_total",
		Help:      "Total number of file download errors.",
	})
	memoryUsage = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "memory_usage_bytes",
		Help:      "Current memory usage in bytes.",
	})
	cpuUsage = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "cpu_usage_percent",
		Help:      "CPU usage as a percentage.",
	})
	activeConnections = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "active_connections_total",
		Help:      "Total number of active connections.",
	})
	requestsTotal = prometheus.NewCounterVec(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "http_requests_total",
		Help:      "Total HTTP requests.",
	}, []string{"method", "path"})
	goroutines = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "goroutines_count",
		Help:      "Number of goroutines.",
	})
	uploadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_upload_size_bytes",
		Help:      "Histogram of uploaded file sizes.",
	})
	downloadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_download_size_bytes",
		Help:      "Histogram of downloaded file sizes.",
	})

	// Register metrics
	prometheus.MustRegister(
		uploadDuration,
		uploadErrorsTotal,
		uploadsTotal,
		downloadDuration,
		downloadsTotal,
		downloadErrorsTotal,
		memoryUsage,
		cpuUsage,
		activeConnections,
		requestsTotal,
		goroutines,
		uploadSizeBytes,
		downloadSizeBytes,
	)
}

// updateSystemMetrics periodically updates system metrics
func updateSystemMetrics(ctx context.Context) {
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			v, _ := mem.VirtualMemory()
			memoryUsage.Set(float64(v.Used))
			c, _ := cpu.Percent(0, false)
			if len(c) > 0 {
				cpuUsage.Set(c[0])
			}
			goroutines.Set(float64(runtime.NumGoroutine()))
		}
	}
}

// initializeWorkerSettings initializes worker settings, possibly auto-adjusting based on system resources
func initializeWorkerSettings(server *ServerConfig, workers *WorkersConfig, clamav *ClamAVConfig) {
	if server.AutoAdjustWorkers {
		numWorkers, queueSize := autoAdjustWorkers()
		workers.NumWorkers = numWorkers
		workers.UploadQueueSize = queueSize
		clamav.NumScanWorkers = max(numWorkers/2, 1)

		logger.Infof("AutoAdjustWorkers enabled: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
			workers.NumWorkers, workers.UploadQueueSize, clamav.NumScanWorkers)
	} else {
		logger.Infof("Manual configuration in effect: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
			workers.NumWorkers, workers.UploadQueueSize, clamav.NumScanWorkers)
	}
}

// autoAdjustWorkers determines the number of workers and queue size based on system resources
func autoAdjustWorkers() (int, int) {
	v, _ := mem.VirtualMemory()
	cpuCores, _ := cpu.Counts(true)

	numWorkers := cpuCores * 2
	if v.Available < 2*1024*1024*1024 { // Less than 2GB free
		numWorkers = max(numWorkers/2, 1)
	}
	queueSize := numWorkers * 10

	logger.Infof("Auto-adjusting workers: NumWorkers=%d, UploadQueueSize=%d", numWorkers, queueSize)
	return numWorkers, queueSize
}

// initializeUploadWorkerPool starts upload workers
func initializeUploadWorkerPool(ctx context.Context, w *WorkersConfig) {
	for i := 0; i < w.NumWorkers; i++ {
		wg.Add(1)
		go uploadWorker(ctx, i)
	}
	logger.Infof("Initialized %d upload workers", w.NumWorkers)
}

// uploadWorker processes upload tasks
func uploadWorker(ctx context.Context, workerID int) {
	defer wg.Done()
	logger.Infof("Upload worker %d started.", workerID)
	for {
		select {
		case <-ctx.Done():
			logger.Infof("Upload worker %d shutting down.", workerID)
			return
		case task, ok := <-uploadQueue:
			if !ok {
				logger.Infof("Upload worker %d stopping as uploadQueue is closed.", workerID)
				return
			}
			err := processUpload(task)
			if err != nil {
				logger.WithFields(logrus.Fields{
					"worker_id": workerID,
					"file":      task.AbsFilename,
					"error":     err,
				}).Error("Failed to process upload")
			} else {
				logger.WithFields(logrus.Fields{
					"worker_id": workerID,
					"file":      task.AbsFilename,
				}).Info("Successfully processed upload")
			}
			task.Result <- err
		}
	}
}

// initializeScanWorkerPool starts scan workers
func initializeScanWorkerPool(ctx context.Context) {
	for i := 0; i < conf.ClamAV.NumScanWorkers; i++ {
		wg.Add(1)
		go scanWorker(ctx, i)
	}
	logger.Infof("Initialized %d scan workers", conf.ClamAV.NumScanWorkers)
}

// scanWorker processes scan tasks
func scanWorker(ctx context.Context, workerID int) {
	defer wg.Done()
	logger.Infof("Scan worker %d started.", workerID)
	for {
		select {
		case <-ctx.Done():
			logger.Infof("Scan worker %d shutting down.", workerID)
			return
		case task, ok := <-scanQueue:
			if !ok {
				logger.Infof("Scan worker %d stopping as scanQueue is closed.", workerID)
				return
			}
			err := scanFileWithClamAV(task.AbsFilename)
			if err != nil {
				logger.WithFields(logrus.Fields{
					"worker_id": workerID,
					"file":      task.AbsFilename,
					"error":     err,
				}).Error("Failed to scan file")
			} else {
				logger.WithFields(logrus.Fields{
					"worker_id": workerID,
					"file":      task.AbsFilename,
				}).Info("Successfully scanned file")
			}
			task.Result <- err
			close(task.Result)
		}
	}
}

// setupRouter sets up HTTP routes and middleware
func setupRouter() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRequest)
	if conf.Server.MetricsEnabled {
		mux.Handle("/metrics", promhttp.Handler())
	}
	handler := loggingMiddleware(mux)
	handler = recoveryMiddleware(handler)
	handler = corsMiddleware(handler)
	return handler
}

// loggingMiddleware logs incoming HTTP requests
func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requestsTotal.WithLabelValues(r.Method, r.URL.Path).Inc()
		activeConnections.Inc()
		defer activeConnections.Dec()

		start := time.Now()
		next.ServeHTTP(w, r)
		duration := time.Since(start).Seconds()
		uploadDuration.Observe(duration)
	})
}

// recoveryMiddleware recovers from panics in HTTP handlers
func recoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if rec := recover(); rec != nil {
				logger.WithFields(logrus.Fields{
					"method": r.Method,
					"url":    r.URL.String(),
					"error":  rec,
				}).Error("Panic recovered in handler")
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
}

// corsMiddleware handles CORS headers
func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*") // Adjust as needed for security
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}
		next.ServeHTTP(w, r)
	})
}

// handleRequest handles all incoming HTTP requests
func handleRequest(w http.ResponseWriter, r *http.Request) {
	clientIP := getClientIP(r)
	logger.WithFields(logrus.Fields{
		"method":  r.Method,
		"url":     r.URL.String(),
		"remote":  clientIP,
		"user_agent": r.UserAgent(),
	}).Info("Incoming request")

	p := r.URL.Path
	a, err := url.ParseQuery(r.URL.RawQuery)
	if err != nil {
		logger.WithError(err).Warn("Failed to parse query parameters")
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}

	fileStorePath := strings.TrimPrefix(p, "/")
	if fileStorePath == "" || fileStorePath == "/" {
		logger.Warn("Access to root directory is forbidden")
		http.Error(w, "Forbidden", http.StatusForbidden)
		downloadErrorsTotal.Inc()
		return
	}

	absFilename, err := sanitizeFilePath(conf.Server.StoragePath, fileStorePath)
	if err != nil {
		logger.WithFields(logrus.Fields{
			"file":   fileStorePath,
			"error":  err,
		}).Warn("Invalid file path")
		http.Error(w, "Invalid file path", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	switch r.Method {
	case http.MethodPut:
		handleUpload(w, r, absFilename, fileStorePath, a)
	case http.MethodHead, http.MethodGet:
		handleDownload(w, r, absFilename, fileStorePath)
	case http.MethodOptions:
		w.Header().Set("Allow", "OPTIONS, GET, PUT, HEAD")
		return
	default:
		logger.WithField("method", r.Method).Warn("Invalid HTTP method for upload directory")
		http.Error(w, "Method Not Allowed", http.StatusMethodNotAllowed)
		return
	}
}

// handleUpload handles PUT requests for file uploads
func handleUpload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string, a url.Values) {
	logger.Infof("Handling upload for file: %s", absFilename)

	// HMAC validation
	var protocolVersion string
	if a.Get("v2") != "" {
		protocolVersion = "v2"
	} else if a.Get("token") != "" {
		protocolVersion = "token"
	} else if a.Get("v") != "" {
		protocolVersion = "v"
	} else {
		logger.Warn("No HMAC attached to URL.")
		http.Error(w, "No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC", http.StatusForbidden)
		return
	}

	mac := hmac.New(sha256.New, []byte(conf.Security.Secret))

	if protocolVersion == "v" {
		mac.Write([]byte(fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)))
	} else {
		contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
		if contentType == "" {
			contentType = "application/octet-stream"
		}
		mac.Write([]byte(fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10) + "\x00" + contentType))
	}

	calculatedMAC := mac.Sum(nil)

	providedMACHex := a.Get(protocolVersion)
	providedMAC, err := hex.DecodeString(providedMACHex)
	if err != nil {
		logger.Warn("Invalid MAC encoding")
		http.Error(w, "Invalid MAC encoding", http.StatusForbidden)
		return
	}

	if !hmac.Equal(calculatedMAC, providedMAC) {
		logger.Warn("Invalid MAC")
		http.Error(w, "Invalid MAC", http.StatusForbidden)
		return
	}

	if !isExtensionAllowed(fileStorePath) {
		logger.Warn("Invalid file extension")
		http.Error(w, "Invalid file extension", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	minFreeBytes, err := parseSize(conf.Server.MinFreeBytes)
	if err != nil {
		logger.Fatalf("Invalid MinFreeBytes: %v", err)
	}
	err = checkStorageSpace(conf.Server.StoragePath, minFreeBytes)
	if err != nil {
		logger.Warn("Not enough free space")
		http.Error(w, "Not enough free space", http.StatusInsufficientStorage)
		uploadErrorsTotal.Inc()
		return
	}

	// Create temp file and write the uploaded data
	tempFilename := absFilename + ".tmp"
	err = createFile(tempFilename, r)
	if err != nil {
		logger.WithFields(logrus.Fields{
			"file":  tempFilename,
			"error": err,
		}).Error("Error creating temp file")
		http.Error(w, "Error writing temp file", http.StatusInternalServerError)
		return
	}

	// Enqueue the upload task
	resultChan := make(chan error)
	uploadQueue <- UploadTask{
		AbsFilename: absFilename,
		Request:     r,
		Result:      resultChan,
	}

	// Respond with 201 Created immediately
	w.WriteHeader(http.StatusCreated)
	if flusher, ok := w.(http.Flusher); ok {
		flusher.Flush()
	}
	logger.Infof("Responded with 201 Created for file: %s", absFilename)

	// Handle the result asynchronously
	go func() {
		err := <-resultChan
		if err != nil {
			logger.WithFields(logrus.Fields{
				"file":  absFilename,
				"error": err,
			}).Error("Failed to process upload")
		} else {
			logger.WithField("file", absFilename).Info("File uploaded and processed successfully")
		}
	}()
}

// handleDownload handles GET and HEAD requests for file downloads
func handleDownload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string) {
	fileInfo, err := getFileInfo(absFilename)
	if err != nil {
		logger.WithError(err).Error("Failed to get file information")
		http.Error(w, "Not Found", http.StatusNotFound)
		downloadErrorsTotal.Inc()
		return
	} else if fileInfo.IsDir() {
		logger.Warn("Directory listing forbidden")
		http.Error(w, "Forbidden", http.StatusForbidden)
		downloadErrorsTotal.Inc()
		return
	}

	contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
	if contentType == "" {
		contentType = "application/octet-stream"
	}
	w.Header().Set("Content-Type", contentType)

	if conf.Uploads.ResumableUploadsEnabled {
		handleResumableDownload(absFilename, w, r, fileInfo.Size())
		return
	}

	if r.Method == http.MethodHead {
		w.Header().Set("Content-Length", strconv.FormatInt(fileInfo.Size(), 10))
		downloadsTotal.Inc()
		return
	} else {
		startTime := time.Now()
		logger.Infof("Initiating download for file: %s", absFilename)
		http.ServeFile(w, r, absFilename)
		downloadDuration.Observe(time.Since(startTime).Seconds())
		downloadSizeBytes.Observe(float64(fileInfo.Size()))
		downloadsTotal.Inc()
		logger.Infof("File downloaded successfully: %s", absFilename)
		return
	}
}

// processUpload handles the upload processing logic
func processUpload(task UploadTask) error {
	logger.Infof("Started processing upload for file: %s", task.AbsFilename)
	semaphore <- struct{}{}
	defer func() { <-semaphore }()

	absFilename := task.AbsFilename
	tempFilename := absFilename + ".tmp"
	r := task.Request

	logger.Infof("Processing upload for file: %s", absFilename)
	startTime := time.Now()

	// Handle chunked or direct upload
	if conf.Uploads.ChunkedUploadsEnabled {
		chunkSize, err := parseSize(conf.Uploads.ChunkSize)
		if err != nil {
			logger.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Error parsing chunk size")
			uploadDuration.Observe(time.Since(startTime).Seconds())
			return err
		}
		err = handleChunkedUpload(tempFilename, r, int(chunkSize))
		if err != nil {
			uploadDuration.Observe(time.Since(startTime).Seconds())
			logger.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Failed to handle chunked upload")
			return err
		}
	} else {
		err := createFile(tempFilename, r)
		if err != nil {
			logger.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Error creating file")
			uploadDuration.Observe(time.Since(startTime).Seconds())
			return err
		}
	}

	// ClamAV scanning
	if clamClient != nil && shouldScanFile(absFilename) {
		err := scanFileWithClamAV(tempFilename)
		if err != nil {
			logger.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Warn("ClamAV detected a virus or scan failed")
			os.Remove(tempFilename)
			uploadErrorsTotal.Inc()
			return err
		}
		logger.Infof("ClamAV scan passed for file: %s", tempFilename)
	} else {
		logger.Warn("ClamAV is not available or file extension not in scan list. Proceeding without virus scan.")
	}

	// Versioning
	if conf.Versioning.EnableVersioning {
		existing, _ := fileExists(absFilename)
		if existing {
			logger.Infof("File %s exists. Initiating versioning.", absFilename)
			err := versionFile(absFilename)
			if err != nil {
				logger.WithFields(logrus.Fields{
					"file":  absFilename,
					"error": err,
				}).Error("Error versioning file")
				os.Remove(tempFilename)
				return err
			}
			logger.Infof("File versioned successfully: %s", absFilename)
		}
	}

	// Rename temp file to final destination
	err := os.Rename(tempFilename, absFilename)
	if err != nil {
		logger.Errorf("Rename failed for %s: %v", absFilename, err)
		os.Remove(tempFilename)
		return fmt.Errorf("failed to move file to final destination: %w", err)
	}
	logger.Infof("File moved to final destination: %s", absFilename)

	// Deduplication
	if conf.DeduplicationEnabled && conf.Redis.RedisEnabled {
		err = handleDeduplication(context.Background(), absFilename)
		if err != nil {
			logger.WithError(err).Error("Deduplication failed")
			uploadErrorsTotal.Inc()
			return err
		}
		logger.Infof("Deduplication handled successfully for file: %s", absFilename)
	}

	// ISO Container Handling
	if conf.ISO.Enabled {
		err = handleISOContainer(absFilename)
		if err != nil {
			logger.WithError(err).Error("ISO container handling failed")
			uploadErrorsTotal.Inc()
			return err
		}
		logger.Infof("ISO container handled successfully for file: %s", absFilename)
	}

	// Record metrics
	uploadDuration.Observe(time.Since(startTime).Seconds())
	uploadSizeBytes.Observe(float64(getFileSize(absFilename)))
	uploadsTotal.Inc()

	logger.WithField("file", absFilename).Info("File uploaded and processed successfully")
	return nil
}

// createFile creates a file from the HTTP request body
func createFile(tempFilename string, r *http.Request) error {
	err := os.MkdirAll(filepath.Dir(tempFilename), 0755)
	if err != nil {
		return fmt.Errorf("failed to create directories: %w", err)
	}

	file, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return fmt.Errorf("failed to open temp file: %w", err)
	}
	defer file.Close()

	bufWriter := bufio.NewWriter(file)
	defer bufWriter.Flush()

	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)

	_, err = io.CopyBuffer(bufWriter, r.Body, *bufPtr)
	if err != nil {
		return fmt.Errorf("failed to copy data to temp file: %w", err)
	}

	return nil
}

// shouldScanFile determines if a file should be scanned based on its extension
func shouldScanFile(filename string) bool {
	ext := strings.ToLower(filepath.Ext(filename))
	for _, scanExt := range conf.ClamAV.ScanFileExtensions {
		if strings.ToLower(scanExt) == ext {
			return true
		}
	}
	return false
}

// uploadWorker processes upload tasks from the uploadQueue
func uploadWorker(ctx context.Context, workerID int) {
	defer wg.Done()
	logger.Infof("Upload worker %d started.", workerID)
	for {
		select {
		case <-ctx.Done():
			logger.Infof("Upload worker %d shutting down.", workerID)
			return
		case task, ok := <-uploadQueue:
			if !ok {
				logger.Infof("Upload worker %d stopping as uploadQueue is closed.", workerID)
				return
			}
			err := processUpload(task)
			if err != nil {
				logger.WithFields(logrus.Fields{
					"worker_id": workerID,
					"file":      task.AbsFilename,
					"error":     err,
				}).Error("Failed to process upload")
			} else {
				logger.WithFields(logrus.Fields{
					"worker_id": workerID,
					"file":      task.AbsFilename,
				}).Info("Successfully processed upload")
			}
			task.Result <- err
		}
	}
}

// initializeUploadWorkerPool starts the upload worker pool
func initializeUploadWorkerPool(ctx context.Context, w *WorkersConfig) {
	for i := 0; i < w.NumWorkers; i++ {
		wg.Add(1)
		go uploadWorker(ctx, i)
	}
	logger.Infof("Initialized %d upload workers", w.NumWorkers)
}

// scanWorker processes scan tasks from the scanQueue
func scanWorker(ctx context.Context, workerID int) {
	defer wg.Done()
	logger.Infof("Scan worker %d started.", workerID)
	for {
		select {
		case <-ctx.Done():
			logger.Infof("Scan worker %d shutting down.", workerID)
			return
		case task, ok := <-scanQueue:
			if !ok {
				logger.Infof("Scan worker %d stopping as scanQueue is closed.", workerID)
				return
			}
			err := scanFileWithClamAV(task.AbsFilename)
			if err != nil {
				logger.WithFields(logrus.Fields{
					"worker_id": workerID,
					"file":      task.AbsFilename,
					"error":     err,
				}).Error("Failed to scan file")
			} else {
				logger.WithFields(logrus.Fields{
					"worker_id": workerID,
					"file":      task.AbsFilename,
				}).Info("Successfully scanned file")
			}
			task.Result <- err
			close(task.Result)
		}
	}
}

// initializeScanWorkerPool starts the scan worker pool
func initializeScanWorkerPool(ctx context.Context) {
	for i := 0; i < conf.ClamAV.NumScanWorkers; i++ {
		wg.Add(1)
		go scanWorker(ctx, i)
	}
	logger.Infof("Initialized %d scan workers", conf.ClamAV.NumScanWorkers)
}

// setupRouter sets up HTTP routes and applies middleware
func setupRouter() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRequest)
	if conf.Server.MetricsEnabled {
		mux.Handle("/metrics", promhttp.Handler())
	}
	handler := loggingMiddleware(mux)
	handler = recoveryMiddleware(handler)
	handler = corsMiddleware(handler)
	return handler
}

// loggingMiddleware logs HTTP requests
func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requestsTotal.WithLabelValues(r.Method, r.URL.Path).Inc()
		activeConnections.Inc()
		defer activeConnections.Dec()

		start := time.Now()
		next.ServeHTTP(w, r)
		duration := time.Since(start).Seconds()
		uploadDuration.Observe(duration)
	})
}

// recoveryMiddleware recovers from panics in HTTP handlers
func recoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if rec := recover(); rec != nil {
				logger.WithFields(logrus.Fields{
					"method": r.Method,
					"url":    r.URL.String(),
					"error":  rec,
				}).Error("Panic recovered in handler")
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
}

// corsMiddleware handles CORS headers
func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*") // Adjust for security as needed
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}
		next.ServeHTTP(w, r)
	})
}

// handleRequest handles all incoming HTTP requests
func handleRequest(w http.ResponseWriter, r *http.Request) {
	clientIP := getClientIP(r)
	logger.WithFields(logrus.Fields{
		"method":     r.Method,
		"url":        r.URL.String(),
		"remote":     clientIP,
		"user_agent": r.UserAgent(),
	}).Info("Incoming request")

	p := r.URL.Path
	a, err := url.ParseQuery(r.URL.RawQuery)
	if err != nil {
		logger.WithError(err).Warn("Failed to parse query parameters")
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}

	fileStorePath := strings.TrimPrefix(p, "/")
	if fileStorePath == "" || fileStorePath == "/" {
		logger.Warn("Access to root directory is forbidden")
		http.Error(w, "Forbidden", http.StatusForbidden)
		downloadErrorsTotal.Inc()
		return
	}

	absFilename, err := sanitizeFilePath(conf.Server.StoragePath, fileStorePath)
	if err != nil {
		logger.WithFields(logrus.Fields{
			"file":  fileStorePath,
			"error": err,
		}).Warn("Invalid file path")
		http.Error(w, "Invalid file path", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	switch r.Method {
	case http.MethodPut:
		handleUpload(w, r, absFilename, fileStorePath, a)
	case http.MethodHead, http.MethodGet:
		handleDownload(w, r, absFilename, fileStorePath)
	case http.MethodOptions:
		w.Header().Set("Allow", "OPTIONS, GET, PUT, HEAD")
		return
	default:
		logger.WithField("method", r.Method).Warn("Invalid HTTP method for upload directory")
		http.Error(w, "Method Not Allowed", http.StatusMethodNotAllowed)
		return
	}
}

// handleUpload handles file uploads via PUT requests
func handleUpload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string, a url.Values) {
	logger.Infof("Handling upload for file: %s", absFilename)

	// HMAC validation
	var protocolVersion string
	if a.Get("v2") != "" {
		protocolVersion = "v2"
	} else if a.Get("token") != "" {
		protocolVersion = "token"
	} else if a.Get("v") != "" {
		protocolVersion = "v"
	} else {
		logger.Warn("No HMAC attached to URL.")
		http.Error(w, "No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC", http.StatusForbidden)
		return
	}

	mac := hmac.New(sha256.New, []byte(conf.Security.Secret))

	if protocolVersion == "v" {
		mac.Write([]byte(fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)))
	} else {
		contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
		if contentType == "" {
			contentType = "application/octet-stream"
		}
		mac.Write([]byte(fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10) + "\x00" + contentType))
	}

	calculatedMAC := mac.Sum(nil)

	providedMACHex := a.Get(protocolVersion)
	providedMAC, err := hex.DecodeString(providedMACHex)
	if err != nil {
		logger.Warn("Invalid MAC encoding")
		http.Error(w, "Invalid MAC encoding", http.StatusForbidden)
		return
	}

	if !hmac.Equal(calculatedMAC, providedMAC) {
		logger.Warn("Invalid MAC")
		http.Error(w, "Invalid MAC", http.StatusForbidden)
		return
	}

	if !isExtensionAllowed(fileStorePath) {
		logger.Warn("Invalid file extension")
		http.Error(w, "Invalid file extension", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	minFreeBytes, err := parseSize(conf.Server.MinFreeBytes)
	if err != nil {
		logger.Fatalf("Invalid MinFreeBytes: %v", err)
	}
	err = checkStorageSpace(conf.Server.StoragePath, minFreeBytes)
	if err != nil {
		logger.Warn("Not enough free space")
		http.Error(w, "Not enough free space", http.StatusInsufficientStorage)
		uploadErrorsTotal.Inc()
		return
	}

	// Create temp file and write the uploaded data
	tempFilename := absFilename + ".tmp"
	err = createFile(tempFilename, r)
	if err != nil {
		logger.WithFields(logrus.Fields{
			"file":  tempFilename,
			"error": err,
		}).Error("Error creating temp file")
		http.Error(w, "Error writing temp file", http.StatusInternalServerError)
		return
	}

	// Enqueue the upload task
	resultChan := make(chan error)
	uploadQueue <- UploadTask{
		AbsFilename: absFilename,
		Request:     r,
		Result:      resultChan,
	}

	// Respond with 201 Created immediately
	w.WriteHeader(http.StatusCreated)
	if flusher, ok := w.(http.Flusher); ok {
		flusher.Flush()
	}
	logger.Infof("Responded with 201 Created for file: %s", absFilename)

	// Handle the result asynchronously
	go func() {
		err := <-resultChan
		if err != nil {
			logger.WithFields(logrus.Fields{
				"file":  absFilename,
				"error": err,
			}).Error("Failed to process upload")
		} else {
			logger.WithField("file", absFilename).Info("File uploaded and processed successfully")
		}
	}()
}

// handleDownload handles file downloads via GET and HEAD requests
func handleDownload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string) {
	fileInfo, err := getFileInfo(absFilename)
	if err != nil {
		logger.WithError(err).Error("Failed to get file information")
		http.Error(w, "Not Found", http.StatusNotFound)
		downloadErrorsTotal.Inc()
		return
	} else if fileInfo.IsDir() {
		logger.Warn("Directory listing forbidden")
		http.Error(w, "Forbidden", http.StatusForbidden)
		downloadErrorsTotal.Inc()
		return
	}

	contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
	if contentType == "" {
		contentType = "application/octet-stream"
	}
	w.Header().Set("Content-Type", contentType)

	if conf.Uploads.ResumableUploadsEnabled {
		handleResumableDownload(absFilename, w, r, fileInfo.Size())
		return
	}

	if r.Method == http.MethodHead {
		w.Header().Set("Content-Length", strconv.FormatInt(fileInfo.Size(), 10))
		downloadsTotal.Inc()
		return
	} else {
		startTime := time.Now()
		logger.Infof("Initiating download for file: %s", absFilename)
		http.ServeFile(w, r, absFilename)
		downloadDuration.Observe(time.Since(startTime).Seconds())
		downloadSizeBytes.Observe(float64(fileInfo.Size()))
		downloadsTotal.Inc()
		logger.Infof("File downloaded successfully: %s", absFilename)
		return
	}
}

// processUpload processes the upload task
func processUpload(task UploadTask) error {
	logger.Infof("Started processing upload for file: %s", task.AbsFilename)
	semaphore <- struct{}{}
	defer func() { <-semaphore }()

	absFilename := task.AbsFilename
	tempFilename := absFilename + ".tmp"
	r := task.Request

	logger.Infof("Processing upload for file: %s", absFilename)
	startTime := time.Now()

	// Handle chunked or direct upload
	if conf.Uploads.ChunkedUploadsEnabled {
		chunkSize, err := parseSize(conf.Uploads.ChunkSize)
		if err != nil {
			logger.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Error parsing chunk size")
			uploadDuration.Observe(time.Since(startTime).Seconds())
			return err
		}
		err = handleChunkedUpload(tempFilename, r, int(chunkSize))
		if err != nil {
			uploadDuration.Observe(time.Since(startTime).Seconds())
			logger.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Failed to handle chunked upload")
			return err
		}
	} else {
		err := createFile(tempFilename, r)
		if err != nil {
			logger.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Error creating file")
			uploadDuration.Observe(time.Since(startTime).Seconds())
			return err
		}
	}

	// ClamAV scanning
	if clamClient != nil && shouldScanFile(absFilename) {
		err := scanFileWithClamAV(tempFilename)
		if err != nil {
			logger.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Warn("ClamAV detected a virus or scan failed")
			os.Remove(tempFilename)
			uploadErrorsTotal.Inc()
			return err
		}
		logger.Infof("ClamAV scan passed for file: %s", tempFilename)
	} else {
		logger.Warn("ClamAV is not available or file extension not in scan list. Proceeding without virus scan.")
	}

	// Versioning
	if conf.Versioning.EnableVersioning {
		existing, _ := fileExists(absFilename)
		if existing {
			logger.Infof("File %s exists. Initiating versioning.", absFilename)
			err := versionFile(absFilename)
			if err != nil {
				logger.WithFields(logrus.Fields{
					"file":  absFilename,
					"error": err,
				}).Error("Error versioning file")
				os.Remove(tempFilename)
				return err
			}
			logger.Infof("File versioned successfully: %s", absFilename)
		}
	}

	// Rename temp file to final destination
	err := os.Rename(tempFilename, absFilename)
	if err != nil {
		logger.Errorf("Rename failed for %s: %v", absFilename, err)
		os.Remove(tempFilename)
		return fmt.Errorf("failed to move file to final destination: %w", err)
	}
	logger.Infof("File moved to final destination: %s", absFilename)

	// Deduplication
	if conf.DeduplicationEnabled && conf.Redis.RedisEnabled {
		err = handleDeduplication(context.Background(), absFilename)
		if err != nil {
			logger.WithError(err).Error("Deduplication failed")
			uploadErrorsTotal.Inc()
			return err
		}
		logger.Infof("Deduplication handled successfully for file: %s", absFilename)
	}

	// ISO Container Handling
	if conf.ISO.Enabled {
		err = handleISOContainer(absFilename)
		if err != nil {
			logger.WithError(err).Error("ISO container handling failed")
			uploadErrorsTotal.Inc()
			return err
		}
		logger.Infof("ISO container handled successfully for file: %s", absFilename)
	}

	// Record metrics
	uploadDuration.Observe(time.Since(startTime).Seconds())
	uploadSizeBytes.Observe(float64(getFileSize(absFilename)))
	uploadsTotal.Inc()

	logger.WithField("file", absFilename).Info("File uploaded and processed successfully")
	return nil
}

// createFile creates a file from the HTTP request body
func createFile(tempFilename string, r *http.Request) error {
	err := os.MkdirAll(filepath.Dir(tempFilename), 0755)
	if err != nil {
		return fmt.Errorf("failed to create directories: %w", err)
	}

	file, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return fmt.Errorf("failed to open temp file: %w", err)
	}
	defer file.Close()

	bufWriter := bufio.NewWriter(file)
	defer bufWriter.Flush()

	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)

	_, err = io.CopyBuffer(bufWriter, r.Body, *bufPtr)
	if err != nil {
		return fmt.Errorf("failed to copy data to temp file: %w", err)
	}

	return nil
}

// shouldScanFile determines if a file should be scanned based on its extension
func shouldScanFile(filename string) bool {
	ext := strings.ToLower(filepath.Ext(filename))
	for _, scanExt := range conf.ClamAV.ScanFileExtensions {
		if strings.ToLower(scanExt) == ext {
			return true
		}
	}
	return false
}

// scanFileWithClamAV scans a file using ClamAV
func scanFileWithClamAV(filePath string) error {
	logger.WithField("file", filePath).Info("Scanning file with ClamAV")

	scanResultChan, err := clamClient.ScanFile(filePath)
	if err != nil {
		logger.WithError(err).Error("Failed to initiate ClamAV scan")
		return fmt.Errorf("failed to initiate ClamAV scan: %w", err)
	}

	scanResult := <-scanResultChan
	if scanResult == nil {
		logger.Error("Failed to receive scan result from ClamAV")
		return fmt.Errorf("failed to receive scan result")
	}

	switch scanResult.Status {
	case clamd.RES_OK:
		logger.WithField("file", filePath).Info("ClamAV scan passed")
		return nil
	case clamd.RES_FOUND:
		logger.WithFields(logrus.Fields{
			"file":        filePath,
			"description": scanResult.Description,
		}).Warn("ClamAV detected a virus")
		return fmt.Errorf("virus detected: %s", scanResult.Description)
	default:
		logger.WithFields(logrus.Fields{
			"file":        filePath,
			"status":      scanResult.Status,
			"description": scanResult.Description,
		}).Warn("ClamAV scan returned unexpected status")
		return fmt.Errorf("unexpected ClamAV status: %s", scanResult.Description)
	}
}

// initClamAV initializes the ClamAV client
func initClamAV(socket string) (*clamd.Clamd, error) {
	if socket == "" {
		logger.Error("ClamAV socket path not configured.")
		return nil, errors.New("ClamAV socket path not configured")
	}

	clamClient := clamd.NewClamd("unix:" + socket)
	err := clamClient.Ping()
	if err != nil {
		logger.Errorf("Failed to connect to ClamAV at %s: %v", socket, err)
		return nil, fmt.Errorf("failed to connect to ClamAV: %w", err)
	}

	logger.Info("Connected to ClamAV successfully.")
	return clamClient, nil
}

// initRedis initializes the Redis client
func initRedis() {
	if !conf.Redis.RedisEnabled {
		logger.Info("Redis is disabled in configuration.")
		return
	}

	redisClient = redis.NewClient(&redis.Options{
		Addr:     conf.Redis.RedisAddr,
		Password: conf.Redis.RedisPassword,
		DB:       conf.Redis.RedisDBIndex,
	})

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	_, err := redisClient.Ping(ctx).Result()
	if err != nil {
		logger.Fatalf("Failed to connect to Redis: %v", err)
	}
	logger.Info("Connected to Redis successfully")

	mu.Lock()
	redisConnected = true
	mu.Unlock()
}

// handleDeduplication handles file deduplication using Redis
func handleDeduplication(ctx context.Context, absFilename string) error {
	checksum, err := computeSHA256(ctx, absFilename)
	if err != nil {
		logger.Errorf("Failed to compute SHA256 for %s: %v", absFilename, err)
		return fmt.Errorf("checksum computation failed: %w", err)
	}
	logger.Debugf("Computed checksum for %s: %s", absFilename, checksum)

	existingPath, err := redisClient.Get(ctx, checksum).Result()
	if err != nil {
		if err == redis.Nil {
			err = redisClient.Set(ctx, checksum, absFilename, 0).Err()
			if err != nil {
				logger.Errorf("Redis error setting checksum %s: %v", checksum, err)
				return fmt.Errorf("redis error: %w", err)
			}
			logger.Infof("Stored new checksum %s for file %s", checksum, absFilename)
			return nil
		}
		logger.Errorf("Redis error fetching checksum %s: %v", checksum, err)
		return fmt.Errorf("redis error: %w", err)
	}

	if existingPath != absFilename {
		if _, err := os.Stat(existingPath); os.IsNotExist(err) {
			logger.Errorf("Existing file for checksum %s not found at %s", checksum, existingPath)
			return fmt.Errorf("existing file not found: %w", err)
		}

		err = os.Link(existingPath, absFilename)
		if err != nil {
			logger.Errorf("Failed linking %s to %s: %v", existingPath, absFilename, err)
			return fmt.Errorf("failed link: %w", err)
		}

		logger.Infof("Created hard link for duplicate file %s -> %s", absFilename, existingPath)
	}

	return nil
}

// computeSHA256 computes the SHA256 hash of a file
func computeSHA256(ctx context.Context, filePath string) (string, error) {
	if filePath == "" {
		return "", errors.New("computeSHA256: filePath cannot be empty")
	}

	file, err := os.Open(filePath)
	if err != nil {
		logger.Errorf("Failed to open file for checksum: %v", err)
		return "", fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	hasher := sha256.New()
	reader := bufio.NewReader(file)

	buffer := make([]byte, 4096)
	for {
		select {
		case <-ctx.Done():
			return "", errors.New("operation cancelled")
		default:
			n, err := reader.Read(buffer)
			if n > 0 {
				if _, wErr := hasher.Write(buffer[:n]); wErr != nil {
					return "", fmt.Errorf("hasher write error: %w", wErr)
				}
			}
			if err != nil {
				if err == io.EOF {
					sum := hex.EncodeToString(hasher.Sum(nil))
					logger.Debugf("Checksum computed: %s", sum)
					return sum, nil
				}
				return "", fmt.Errorf("read error: %w", err)
			}
		}
	}
}

// handleResumableDownload handles HTTP Range requests for resumable downloads
func handleResumableDownload(absFilename string, w http.ResponseWriter, r *http.Request, fileSize int64) {
	rangeHeader := r.Header.Get("Range")
	if rangeHeader == "" {
		startTime := time.Now()
		http.ServeFile(w, r, absFilename)
		downloadDuration.Observe(time.Since(startTime).Seconds())
		downloadSizeBytes.Observe(float64(fileSize))
		downloadsTotal.Inc()
		return
	}

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

	end := fileSize - 1
	if ranges[1] != "" {
		end, err = strconv.ParseInt(ranges[1], 10, 64)
		if err != nil || end >= fileSize {
			http.Error(w, "Invalid Range", http.StatusRequestedRangeNotSatisfiable)
			downloadErrorsTotal.Inc()
			return
		}
	}

	w.Header().Set("Content-Range", fmt.Sprintf("bytes %d-%d/%d", start, end, fileSize))
	w.Header().Set("Content-Length", strconv.FormatInt(end-start+1, 10))
	w.Header().Set("Accept-Ranges", "bytes")
	w.WriteHeader(http.StatusPartialContent)

	file, err := os.Open(absFilename)
	if err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}
	defer file.Close()

	_, err = file.Seek(start, 0)
	if err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}

	buffer := make([]byte, 32*1024)
	remaining := end - start + 1
	startTime := time.Now()
	for remaining > 0 {
		if int64(len(buffer)) > remaining {
			buffer = buffer[:remaining]
		}
		n, err := file.Read(buffer)
		if n > 0 {
			if _, writeErr := w.Write(buffer[:n]); writeErr != nil {
				logger.WithError(writeErr).Error("Failed to write to response")
				downloadErrorsTotal.Inc()
				return
			}
			remaining -= int64(n)
		}
		if err != nil {
			if err != io.EOF {
				logger.WithError(err).Error("Error reading file during resumable download")
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

// getFileInfo retrieves file information, utilizing cache
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

// sanitizeFilePath sanitizes and validates the file path to prevent directory traversal
func sanitizeFilePath(baseDir, filePath string) (string, error) {
	absBaseDir, err := filepath.Abs(baseDir)
	if err != nil {
		return "", fmt.Errorf("failed to resolve base directory: %w", err)
	}

	absFilePath, err := filepath.Abs(filepath.Join(absBaseDir, filePath))
	if err != nil {
		return "", fmt.Errorf("failed to resolve file path: %w", err)
	}

	if !strings.HasPrefix(absFilePath, absBaseDir) {
		return "", fmt.Errorf("invalid file path: %s", filePath)
	}

	return absFilePath, nil
}

// checkStorageSpace checks if there's enough free space in the storage path
func checkStorageSpace(storagePath string, minFreeBytes int64) error {
	var stat syscall.Statfs_t
	err := syscall.Statfs(storagePath, &stat)
	if err != nil {
		return fmt.Errorf("failed to get filesystem stats: %w", err)
	}

	availableBytes := stat.Bavail * uint64(stat.Bsize)
	if int64(availableBytes) < minFreeBytes {
		return fmt.Errorf("not enough free space: %d available, %d required", availableBytes, minFreeBytes)
	}

	return nil
}

// handleDeduplication handles file deduplication using Redis
func handleDeduplication(ctx context.Context, absFilename string) error {
	checksum, err := computeSHA256(ctx, absFilename)
	if err != nil {
		logger.Errorf("Failed to compute SHA256 for %s: %v", absFilename, err)
		return fmt.Errorf("checksum computation failed: %w", err)
	}
	logger.Debugf("Computed checksum for %s: %s", absFilename, checksum)

	existingPath, err := redisClient.Get(ctx, checksum).Result()
	if err != nil {
		if err == redis.Nil {
			err = redisClient.Set(ctx, checksum, absFilename, 0).Err()
			if err != nil {
				logger.Errorf("Redis error setting checksum %s: %v", checksum, err)
				return fmt.Errorf("redis error: %w", err)
			}
			logger.Infof("Stored new checksum %s for file %s", checksum, absFilename)
			return nil
		}
		logger.Errorf("Redis error fetching checksum %s: %v", checksum, err)
		return fmt.Errorf("redis error: %w", err)
	}

	if existingPath != absFilename {
		if _, err := os.Stat(existingPath); os.IsNotExist(err) {
			logger.Errorf("Existing file for checksum %s not found at %s", checksum, existingPath)
			return fmt.Errorf("existing file not found: %w", err)
		}

		err = os.Link(existingPath, absFilename)
		if err != nil {
			logger.Errorf("Failed linking %s to %s: %v", existingPath, absFilename, err)
			return fmt.Errorf("failed link: %w", err)
		}

		logger.Infof("Created hard link for duplicate file %s -> %s", absFilename, existingPath)
	}

	return nil
}

// computeSHA256 computes the SHA256 hash of a file
func computeSHA256(ctx context.Context, filePath string) (string, error) {
	if filePath == "" {
		return "", errors.New("computeSHA256: filePath cannot be empty")
	}

	file, err := os.Open(filePath)
	if err != nil {
		logger.Errorf("Failed to open file for checksum: %v", err)
		return "", fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	hasher := sha256.New()
	reader := bufio.NewReader(file)

	buffer := make([]byte, 4096)
	for {
		select {
		case <-ctx.Done():
			return "", errors.New("operation cancelled")
		default:
			n, err := reader.Read(buffer)
			if n > 0 {
				if _, wErr := hasher.Write(buffer[:n]); wErr != nil {
					return "", fmt.Errorf("hasher write error: %w", wErr)
				}
			}
			if err != nil {
				if err == io.EOF {
					sum := hex.EncodeToString(hasher.Sum(nil))
					logger.Debugf("Checksum computed: %s", sum)
					return sum, nil
				}
				return "", fmt.Errorf("read error: %w", err)
			}
		}
	}
}

// handleResumableDownload handles HTTP Range requests for resumable downloads
func handleResumableDownload(absFilename string, w http.ResponseWriter, r *http.Request, fileSize int64) {
	rangeHeader := r.Header.Get("Range")
	if rangeHeader == "" {
		startTime := time.Now()
		http.ServeFile(w, r, absFilename)
		downloadDuration.Observe(time.Since(startTime).Seconds())
		downloadSizeBytes.Observe(float64(fileSize))
		downloadsTotal.Inc()
		return
	}

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

	end := fileSize - 1
	if ranges[1] != "" {
		end, err = strconv.ParseInt(ranges[1], 10, 64)
		if err != nil || end >= fileSize {
			http.Error(w, "Invalid Range", http.StatusRequestedRangeNotSatisfiable)
			downloadErrorsTotal.Inc()
			return
		}
	}

	w.Header().Set("Content-Range", fmt.Sprintf("bytes %d-%d/%d", start, end, fileSize))
	w.Header().Set("Content-Length", strconv.FormatInt(end-start+1, 10))
	w.Header().Set("Accept-Ranges", "bytes")
	w.WriteHeader(http.StatusPartialContent)

	file, err := os.Open(absFilename)
	if err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}
	defer file.Close()

	_, err = file.Seek(start, 0)
	if err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}

	buffer := make([]byte, 32*1024)
	remaining := end - start + 1
	startTime := time.Now()
	for remaining > 0 {
		if int64(len(buffer)) > remaining {
			buffer = buffer[:remaining]
		}
		n, err := file.Read(buffer)
		if n > 0 {
			if _, writeErr := w.Write(buffer[:n]); writeErr != nil {
				logger.WithError(writeErr).Error("Failed to write to response")
				downloadErrorsTotal.Inc()
				return
			}
			remaining -= int64(n)
		}
		if err != nil {
			if err != io.EOF {
				logger.WithError(err).Error("Error reading file during resumable download")
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

// getClientIP retrieves the client's IP address from the HTTP request
func getClientIP(r *http.Request) string {
	clientIP := r.Header.Get("X-Real-IP")
	if clientIP == "" {
		clientIP = r.Header.Get("X-Forwarded-For")
	}
	if clientIP == "" {
		host, _, err := net.SplitHostPort(r.RemoteAddr)
		if err != nil {
			logger.WithError(err).Warn("Failed to parse RemoteAddr")
			clientIP = r.RemoteAddr
		} else {
			clientIP = host
		}
	}
	return clientIP
}

// computeSHA256 computes the SHA256 hash of a file
func computeSHA256(ctx context.Context, filePath string) (string, error) {
	if filePath == "" {
		return "", errors.New("computeSHA256: filePath cannot be empty")
	}

	file, err := os.Open(filePath)
	if err != nil {
		logger.Errorf("Failed to open file for checksum: %v", err)
		return "", fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	hasher := sha256.New()
	reader := bufio.NewReader(file)

	buffer := make([]byte, 4096)
	for {
		select {
		case <-ctx.Done():
			return "", errors.New("operation cancelled")
		default:
			n, err := reader.Read(buffer)
			if n > 0 {
				if _, wErr := hasher.Write(buffer[:n]); wErr != nil {
					return "", fmt.Errorf("hasher write error: %w", wErr)
				}
			}
			if err != nil {
				if err == io.EOF {
					sum := hex.EncodeToString(hasher.Sum(nil))
					logger.Debugf("Checksum computed: %s", sum)
					return sum, nil
				}
				return "", fmt.Errorf("read error: %w", err)
			}
		}
	}
}

// handleDeduplication handles file deduplication using Redis
func handleDeduplication(ctx context.Context, absFilename string) error {
	checksum, err := computeSHA256(ctx, absFilename)
	if err != nil {
		logger.Errorf("Failed to compute SHA256 for %s: %v", absFilename, err)
		return fmt.Errorf("checksum computation failed: %w", err)
	}
	logger.Debugf("Computed checksum for %s: %s", absFilename, checksum)

	existingPath, err := redisClient.Get(ctx, checksum).Result()
	if err != nil {
		if err == redis.Nil {
			err = redisClient.Set(ctx, checksum, absFilename, 0).Err()
			if err != nil {
				logger.Errorf("Redis error setting checksum %s: %v", checksum, err)
				return fmt.Errorf("redis error: %w", err)
			}
			logger.Infof("Stored new checksum %s for file %s", checksum, absFilename)
			return nil
		}
		logger.Errorf("Redis error fetching checksum %s: %v", checksum, err)
		return fmt.Errorf("redis error: %w", err)
	}

	if existingPath != absFilename {
		if _, err := os.Stat(existingPath); os.IsNotExist(err) {
			logger.Errorf("Existing file for checksum %s not found at %s", checksum, existingPath)
			return fmt.Errorf("existing file not found: %w", err)
		}

		err = os.Link(existingPath, absFilename)
		if err != nil {
			logger.Errorf("Failed linking %s to %s: %v", existingPath, absFilename, err)
			return fmt.Errorf("failed link: %w", err)
		}

		logger.Infof("Created hard link for duplicate file %s -> %s", absFilename, existingPath)
	}

	return nil
}

// getFileSize retrieves the size of a file
func getFileSize(absFilename string) int64 {
	fileInfo, err := os.Stat(absFilename)
	if err != nil {
		logger.WithError(err).Errorf("Failed to get file size for %s", absFilename)
		return 0
	}
	return fileInfo.Size()
}

// handleChunkedUpload handles chunked file uploads
func handleChunkedUpload(tempFilename string, r *http.Request, chunkSize int) error {
	logger.WithField("file", tempFilename).Info("Handling chunked upload to temporary file")

	absDirectory := filepath.Dir(tempFilename)
	err := os.MkdirAll(absDirectory, os.ModePerm)
	if err != nil {
		return fmt.Errorf("failed to create directory: %w", err)
	}

	targetFile, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return fmt.Errorf("failed to open file: %w", err)
	}
	defer targetFile.Close()

	writer := bufio.NewWriterSize(targetFile, chunkSize)
	defer writer.Flush()

	buffer := make([]byte, chunkSize)

	totalBytes := int64(0)
	for {
		n, err := r.Body.Read(buffer)
		if err != nil && err != io.EOF {
			return fmt.Errorf("failed to read request body: %w", err)
		}
		if n == 0 {
			break
		}

		_, err = writer.Write(buffer[:n])
		if err != nil {
			return fmt.Errorf("failed to write to file: %w", err)
		}
		totalBytes += int64(n)
	}

	logger.WithFields(logrus.Fields{
		"temp_file":  tempFilename,
		"total_bytes": totalBytes,
	}).Info("Chunked upload completed successfully")
	uploadSizeBytes.Observe(float64(totalBytes))
	return nil
}

// handleISOContainer handles ISO container creation and mounting
func handleISOContainer(absFilename string) error {
	isoPath := filepath.Join(conf.ISO.MountPoint, "container.iso")

	// Create ISO container
	err := CreateISOContainer([]string{absFilename}, isoPath, conf.ISO.Size, conf.ISO.Charset)
	if err != nil {
		return fmt.Errorf("failed to create ISO container: %w", err)
	}

	// Mount ISO container
	err = MountISOContainer(isoPath, conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}

	// Unmount ISO container
	err = UnmountISOContainer(conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to unmount ISO container: %w", err)
	}

	return nil
}

// CreateISOContainer creates an ISO container with specified files
func CreateISOContainer(files []string, isoPath string, size string, charset string) error {
	args := []string{"-o", isoPath, "-V", "ISO_CONTAINER", "-J", "-R", "-input-charset", charset}
	args = append(args, files...)
	cmd := exec.Command("genisoimage", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// MountISOContainer mounts the ISO container
func MountISOContainer(isoPath string, mountPoint string) error {
	if err := os.MkdirAll(mountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	var output []byte
	var err error
	for i := 0; i < 3; i++ {
		cmd := exec.Command("mount", "-o", "loop,ro", isoPath, mountPoint)
		output, err = cmd.CombinedOutput()
		if err == nil {
			logger.Infof("ISO container mounted at %s", mountPoint)
			return nil
		}
		logger.Warnf("Mount attempt %d failed: %v, output: %s", i+1, err, string(output))
		time.Sleep(2 * time.Second)
	}
	return fmt.Errorf("failed to mount ISO: %w, output: %s", err, string(output))
}

// UnmountISOContainer unmounts the ISO container
func UnmountISOContainer(mountPoint string) error {
	cmd := exec.Command("umount", mountPoint)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// verifyAndCreateISOContainer verifies the ISO container and recreates if corrupted
func verifyAndCreateISOContainer() error {
	isoPath := filepath.Join(conf.ISO.MountPoint, "container.iso")

	if exists, _ := fileExists(isoPath); !exists {
		logger.Infof("ISO container does not exist, creating at %s", isoPath)
		files := []string{conf.Server.StoragePath}

		err := CreateISOContainer(files, isoPath, conf.ISO.Size, conf.ISO.Charset)
		if err != nil {
			return fmt.Errorf("failed to create ISO: %w", err)
		}
		logger.Infof("ISO container created at %s", isoPath)
	}

	err := verifyISOFile(isoPath)
	if err != nil {
		files := []string{conf.Server.StoragePath}
		err = handleCorruptedISOFile(isoPath, files, conf.ISO.Size, conf.ISO.Charset)
		if err != nil {
			return fmt.Errorf("failed to handle corrupted ISO: %w", err)
		}
	}

	if err := os.MkdirAll(conf.ISO.MountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	err = MountISOContainer(isoPath, conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to mount ISO: %w", err)
	}
	logger.Infof("ISO container mounted at %s", conf.ISO.MountPoint)

	return nil
}

// verifyISOFile verifies the integrity of the ISO file
func verifyISOFile(isoPath string) error {
	cmd := exec.Command("isoinfo", "-i", isoPath, "-d")
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("failed to verify ISO: %w, output: %s", err, string(output))
	}
	return nil
}

// handleCorruptedISOFile handles corrupted ISO files by recreating them
func handleCorruptedISOFile(isoPath string, files []string, size string, charset string) error {
	logger.Warnf("ISO file %s corrupted. Recreating...", isoPath)
	err := CreateISOContainer(files, isoPath, size, charset)
	if err != nil {
		return fmt.Errorf("failed to recreate ISO: %w", err)
	}
	return nil
}

// handleGracefulShutdown sets up graceful shutdown on receiving termination signals
func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-quit
		logger.Infof("Received signal %s. Initiating shutdown...", sig)

		ctxShutdown, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()

		if err := server.Shutdown(ctxShutdown); err != nil {
			logger.Errorf("Server shutdown failed: %v", err)
		} else {
			logger.Info("Server shutdown gracefully.")
		}

		cancel()

		close(uploadQueue)
		close(scanQueue)
		close(networkEvents)

		logger.Info("Shutdown process completed. Exiting application.")
		os.Exit(0)
	}()
}

// initMetrics initializes Prometheus metrics
func initMetrics() {
	// [Already initialized above]
}

// MonitorRedisHealth periodically checks Redis connectivity
func MonitorRedisHealth(ctx context.Context, client *redis.Client, checkInterval time.Duration) {
	ticker := time.NewTicker(checkInterval)
	defer ticker.Stop()
	defer wg.Done()

	for {
		select {
		case <-ctx.Done():
			logger.Info("Stopping Redis health monitor.")
			return
		case <-ticker.C:
			err := client.Ping(ctx).Err()
			mu.Lock()
			if err != nil {
				if redisConnected {
					logger.Errorf("Redis health check failed: %v", err)
				}
				redisConnected = false
			} else {
				if !redisConnected {
					logger.Info("Redis reconnected successfully")
				}
				redisConnected = true
				logger.Debug("Redis health check succeeded.")
			}
			mu.Unlock()
		}
	}
}

// runFileCleaner periodically cleans up expired files
func runFileCleaner(ctx context.Context, storeDir string, ttl time.Duration) {
	ticker := time.NewTicker(1 * time.Hour)
	defer ticker.Stop()
	defer wg.Done()

	for {
		select {
		case <-ctx.Done():
			logger.Info("Stopping file cleaner.")
			return
		case <-ticker.C:
			now := time.Now()
			err := filepath.Walk(storeDir, func(path string, info os.FileInfo, err error) error {
				if err != nil {
					logger.WithError(err).Errorf("Error accessing path %s", path)
					return nil
				}
				if info.IsDir() {
					return nil
				}
				if now.Sub(info.ModTime()) > ttl {
					err := os.Remove(path)
					if err != nil {
						logger.WithError(err).Errorf("Failed to remove expired file: %s", path)
					} else {
						logger.Infof("Removed expired file: %s", path)
					}
				}
				return nil
			})
			if err != nil {
				logger.WithError(err).Error("Error cleaning files")
			}
		}
	}
}

// getClientIP retrieves the client's IP address from the HTTP request
func getClientIP(r *http.Request) string {
	clientIP := r.Header.Get("X-Real-IP")
	if clientIP == "" {
		clientIP = r.Header.Get("X-Forwarded-For")
	}
	if clientIP == "" {
		host, _, err := net.SplitHostPort(r.RemoteAddr)
		if err != nil {
			logger.WithError(err).Warn("Failed to parse RemoteAddr")
			clientIP = r.RemoteAddr
		} else {
			clientIP = host
		}
	}
	return clientIP
}

// max returns the maximum of two integers
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// handleISOContainer handles ISO container creation and mounting
func handleISOContainer(absFilename string) error {
	isoPath := filepath.Join(conf.ISO.MountPoint, "container.iso")

	// Create ISO container
	err := CreateISOContainer([]string{absFilename}, isoPath, conf.ISO.Size, conf.ISO.Charset)
	if err != nil {
		return fmt.Errorf("failed to create ISO container: %w", err)
	}

	// Mount ISO container
	err = MountISOContainer(isoPath, conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}

	// Unmount ISO container
	err = UnmountISOContainer(conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to unmount ISO container: %w", err)
	}

	return nil
}

// versionFile handles versioning of existing files
func versionFile(absFilename string) error {
	versionDir := absFilename + "_versions"

	err := os.MkdirAll(versionDir, os.ModePerm)
	if err != nil {
		return fmt.Errorf("failed to create version directory: %w", err)
	}

	timestamp := time.Now().Format("20060102-150405")
	versionedFilename := filepath.Join(versionDir, filepath.Base(absFilename)+"."+timestamp)

	err = os.Rename(absFilename, versionedFilename)
	if err != nil {
		return fmt.Errorf("failed to version the file: %w", err)
	}

	logger.WithFields(logrus.Fields{
		"original":      absFilename,
		"versioned_as":  versionedFilename,
		"version_dir":   versionDir,
	}).Info("Versioned old file")

	return cleanupOldVersions(versionDir)
}

// cleanupOldVersions removes old versions exceeding MaxVersions
func cleanupOldVersions(versionDir string) error {
	files, err := os.ReadDir(versionDir)
	if err != nil {
		return fmt.Errorf("failed to list version files: %w", err)
	}

	if conf.Versioning.MaxVersions > 0 && len(files) > conf.Versioning.MaxVersions {
		excessFiles := len(files) - conf.Versioning.MaxVersions
		for i := 0; i < excessFiles; i++ {
			err := os.Remove(filepath.Join(versionDir, files[i].Name()))
			if err != nil {
				return fmt.Errorf("failed to remove old version: %w", err)
			}
			logger.WithField("file", files[i].Name()).Info("Removed old version")
		}
	}

	return nil
}

// handleChunkedUpload handles chunked uploads by writing chunks to a temp file
func handleChunkedUpload(tempFilename string, r *http.Request, chunkSize int) error {
	logger.WithField("file", tempFilename).Info("Handling chunked upload to temporary file")

	absDirectory := filepath.Dir(tempFilename)
	err := os.MkdirAll(absDirectory, 0755)
	if err != nil {
		return fmt.Errorf("failed to create directory: %w", err)
	}

	targetFile, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return fmt.Errorf("failed to open file: %w", err)
	}
	defer targetFile.Close()

	writer := bufio.NewWriterSize(targetFile, chunkSize)
	defer writer.Flush()

	buffer := make([]byte, chunkSize)

	totalBytes := int64(0)
	for {
		n, err := r.Body.Read(buffer)
		if err != nil && err != io.EOF {
			return fmt.Errorf("failed to read request body: %w", err)
		}
		if n == 0 {
			break
		}

		_, err = writer.Write(buffer[:n])
		if err != nil {
			return fmt.Errorf("failed to write to file: %w", err)
		}
		totalBytes += int64(n)
	}

	logger.WithFields(logrus.Fields{
		"temp_file":   tempFilename,
		"total_bytes": totalBytes,
	}).Info("Chunked upload completed successfully")
	uploadSizeBytes.Observe(float64(totalBytes))
	return nil
}

// getFileInfo retrieves file information, utilizing cache
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

// isExtensionAllowed checks if the file extension is allowed
func isExtensionAllowed(filename string) bool {
	if len(conf.Uploads.AllowedExtensions) == 0 {
		return true
	}
	ext := strings.ToLower(filepath.Ext(filename))
	for _, allowedExt := range conf.Uploads.AllowedExtensions {
		if strings.ToLower(allowedExt) == ext {
			return true
		}
	}
	return false
}

// setupGracefulShutdown sets up graceful shutdown on receiving termination signals
func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-quit
		logger.Infof("Received signal %s. Initiating shutdown...", sig)

		ctxShutdown, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()

		if err := server.Shutdown(ctxShutdown); err != nil {
			logger.Errorf("Server shutdown failed: %v", err)
		} else {
			logger.Info("Server shutdown gracefully.")
		}

		cancel()

		close(uploadQueue)
		close(scanQueue)
		close(networkEvents)

		logger.Info("Shutdown process completed. Exiting application.")
		os.Exit(0)
	}()
}

// monitorNetwork monitors network changes (e.g., IP address changes)
func monitorNetwork(ctx context.Context) {
	currentIP := getCurrentIPAddress()

	for {
		select {
		case <-ctx.Done():
			logger.Info("Stopping network monitor.")
			return
		case <-time.After(10 * time.Second):
			newIP := getCurrentIPAddress()
			if newIP != currentIP && newIP != "" {
				currentIP = newIP
				select {
				case networkEvents <- NetworkEvent{Type: "IP_CHANGE", Details: currentIP}:
					logger.WithField("new_ip", currentIP).Info("Queued IP_CHANGE event")
				default:
					logger.Warn("Network event channel full. Dropping IP_CHANGE event.")
				}
			}
		}
	}
}

// handleNetworkEvents processes network events
func handleNetworkEvents(ctx context.Context) {
	defer wg.Done()
	var previousRedisStatus bool

	for {
		select {
		case <-ctx.Done():
			logger.Info("Stopping network event handler.")
			return
		case event := <-networkEvents:
			switch event.Type {
			case "IP_CHANGE":
				logger.WithFields(logrus.Fields{
					"category": "network",
					"message":  fmt.Sprintf("IP address changed to %s", event.Details),
				}).Info("Network Event")
			// Add more event types as needed
			default:
				logger.WithFields(logrus.Fields{
					"category": "network",
					"message":  event.Details,
				}).Debug("Unhandled network event")
			}
		}
	}
}

// computeSHA256 computes the SHA256 hash of a file
func computeSHA256(ctx context.Context, filePath string) (string, error) {
	// [Already implemented above]
}

// handleISOContainer handles ISO container creation and mounting
func handleISOContainer(absFilename string) error {
	// [Already implemented above]
}

// verifyAndCreateISOContainer verifies the ISO container and recreates if corrupted
func verifyAndCreateISOContainer() error {
	// [Already implemented above]
}

// handleCorruptedISOFile handles corrupted ISO files by recreating them
func handleCorruptedISOFile(isoPath string, files []string, size string, charset string) error {
	// [Already implemented above]
}

// getClientIP retrieves the client's IP address from the HTTP request
func getClientIP(r *http.Request) string {
	// [Already implemented above]
}

// getFileSize retrieves the size of a file
func getFileSize(absFilename string) int64 {
	fileInfo, err := os.Stat(absFilename)
	if err != nil {
		logger.WithError(err).Errorf("Failed to get file size for %s", absFilename)
		return 0
	}
	return fileInfo.Size()
}

// handleDeduplication handles file deduplication using Redis
func handleDeduplication(ctx context.Context, absFilename string) error {
	// [Already implemented above]
}

// handleGracefulShutdown sets up graceful shutdown on receiving termination signals
func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
	// [Already implemented above]
}

// createFile creates a file from the HTTP request body
func createFile(tempFilename string, r *http.Request) error {
	// [Already implemented above]
}

// handleChunkedUpload handles chunked uploads by writing chunks to a temp file
func handleChunkedUpload(tempFilename string, r *http.Request, chunkSize int) error {
	// [Already implemented above]
}

// handleISOContainer handles ISO container creation and mounting
func handleISOContainer(absFilename string) error {
	// [Already implemented above]
}
