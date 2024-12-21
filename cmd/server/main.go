package main

import (
	"context"
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"strconv"
	"strings"
	"syscall"
	"time"

	"github.com/dutchcoders/go-clamd" // ClamAV integration
	"github.com/go-redis/redis/v8"    // Redis integration
	"github.com/prometheus/client_golang/prometheus"
	"github.com/renz/hmac-file-server/config"
	"github.com/renz/hmac-file-server/handlers"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/disk"
	"github.com/shirou/gopsutil/host"
	"github.com/shirou/gopsutil/mem"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

// parseTTL converts a human-readable TTL string to a time.Duration
func parseTTL(ttlStr string) (time.Duration, error) {
	ttlStr = strings.TrimSpace(ttlStr)
	if len(ttlStr) < 2 {
		return 0, fmt.Errorf("invalid TTL: %s", ttlStr)
	}

	unit := ttlStr[len(ttlStr)-1:]
	valueStr := ttlStr[:len(ttlStr)-1]
	value, err := strconv.Atoi(valueStr)
	if err != nil {
		return 0, fmt.Errorf("invalid TTL value: %s", valueStr)
	}

	switch strings.ToUpper(unit) {
	case "D":
		return time.Duration(value) * 24 * time.Hour, nil
	case "M":
		return time.Duration(value) * 30 * 24 * time.Hour, nil
	case "Y":
		return time.Duration(value) * 365 * 24 * time.Hour, nil
	default:
		return 0, fmt.Errorf("unknown TTL unit: %s", unit)
	}
}

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
	log                   = logrus.New()
	uploadQueue    chan UploadTask
	clamClient     *clamd.Clamd
	redisClient    *redis.Client

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

	scanQueue   chan ScanTask
	ScanWorkers = 5
)

const (
	MinFreeBytes = 1 << 30
)


// Removed unused constant maxConcurrentOperations

// Removed unused semaphore variable

func main() {
	// Load configuration
	cfg, err := config.LoadConfig("/path/to/config.toml") // Provide the correct config file path
	if err != nil {
		log.Fatalf("Error loading config: %v", err)
	}

	// Set up logging
	setupLogging() // Ensure setupLogging uses cfg from config package

	// Initialize handlers with config
	h := handlers.NewHandler(cfg)

	// Set up routes
	http.HandleFunc("/upload", h.UploadHandler)
	http.HandleFunc("/download", h.DownloadHandler)

	// Start server
	log.Infof("Starting server on port %s...", cfg.Server.ListenPort)
	if err := http.ListenAndServe(":"+cfg.Server.ListenPort, nil); err != nil {
		log.Fatalf("Server failed: %v", err)
	}
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func autoAdjustWorkers() (int, int) {
	v, _ := mem.VirtualMemory()
	cpuCores, _ := cpu.Counts(true)

	numWorkers := cpuCores * 2
	if v.Available < 2*1024*1024*1024 {
		numWorkers = max(numWorkers/2, 1)
	}
	queueSize := numWorkers * 10

	log.Infof("Auto-adjusting workers: NumWorkers=%d, UploadQueueSize=%d", numWorkers, queueSize)
	return numWorkers, queueSize
}

func initializeWorkerSettings(server *ServerConfig, workers *WorkersConfig, clamav *ClamAVConfig) {
	if server.AutoAdjustWorkers {
		numWorkers, queueSize := autoAdjustWorkers()
		workers.NumWorkers = numWorkers
		workers.UploadQueueSize = queueSize
		clamav.NumScanWorkers = max(numWorkers/2, 1)

		log.Infof("AutoAdjustWorkers enabled: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
			workers.NumWorkers, workers.UploadQueueSize, clamav.NumScanWorkers)
	} else {
		log.Infof("Manual configuration in effect: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
			workers.NumWorkers, workers.UploadQueueSize, clamav.NumScanWorkers)
	}
}

func monitorWorkerPerformance(ctx context.Context, server *ServerConfig, w *WorkersConfig, clamav *ClamAVConfig) {
	ticker := time.NewTicker(5 * time.Minute)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping worker performance monitor.")
			return
		case <-ticker.C:
			if server.AutoAdjustWorkers {
				numWorkers, queueSize := autoAdjustWorkers()
				w.NumWorkers = numWorkers
				w.UploadQueueSize = queueSize
				clamav.NumScanWorkers = max(numWorkers/2, 1)

				log.Infof("Re-adjusted workers: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
					w.NumWorkers, w.UploadQueueSize, clamav.NumScanWorkers)
			}
		}
	}
}

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

	if err := validateConfig(conf); err != nil {
		return fmt.Errorf("configuration validation failed: %w", err)
	}

	conf.Server.DeduplicationEnabled = viper.GetBool("deduplication.Enabled")

	return nil
}

func setDefaults() {
	viper.SetDefault("server.ListenPort", "8080")
	viper.SetDefault("server.UnixSocket", false)
	viper.SetDefault("server.StoragePath", "./uploads")
	viper.SetDefault("server.LogLevel", "info")
	viper.SetDefault("server.LogFile", "")
	viper.SetDefault("server.MetricsEnabled", true)
	viper.SetDefault("server.MetricsPort", "9090")
	viper.SetDefault("server.FileTTL", "8760h")
	viper.SetDefault("server.MinFreeBytes", "100MB")
	viper.SetDefault("server.AutoAdjustWorkers", true)
	viper.SetDefault("server.NetworkEvents", true) // Set default
	_, err := parseTTL("1D")
	if err != nil {
		log.Warnf("Failed to parse TTL: %v", err)
	}

	viper.SetDefault("timeouts.ReadTimeout", "4800s")
	viper.SetDefault("timeouts.WriteTimeout", "4800s")
	viper.SetDefault("timeouts.IdleTimeout", "4800s")

	viper.SetDefault("security.Secret", "changeme")

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

	viper.SetDefault("redis.RedisEnabled", true)
	viper.SetDefault("redis.RedisAddr", "localhost:6379")
	viper.SetDefault("redis.RedisPassword", "")
	viper.SetDefault("redis.RedisDBIndex", 0)
	viper.SetDefault("redis.RedisHealthCheckInterval", "120s")

	viper.SetDefault("workers.NumWorkers", 4)
	viper.SetDefault("workers.UploadQueueSize", 50)

	viper.SetDefault("deduplication.Enabled", true)

	viper.SetDefault("iso.Enabled", true)
	viper.SetDefault("iso.Size", "1GB")
	viper.SetDefault("iso.MountPoint", "/mnt/iso")
	viper.SetDefault("iso.Charset", "utf-8")
}

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
	if conf.Server.MinFreeBytes == "" {
		return fmt.Errorf("MinFreeBytes must be set")
	}
	if conf.Timeouts.ReadTimeout == "" {
		return fmt.Errorf("ReadTimeout must be set")
	}
	if conf.Timeouts.WriteTimeout == "" {
		return fmt.Errorf("WriteTimeout must be set")
	}
	if conf.Timeouts.IdleTimeout == "" {
		return fmt.Errorf("IdleTimeout must be set")
	}
	if conf.Uploads.ChunkSize == "" {
		return fmt.Errorf("ChunkSize must be set")
	}
	if conf.ClamAV.ClamAVSocket == "" {
		return fmt.Errorf("ClamAVSocket must be set")
	}
	if conf.Redis.RedisAddr == "" {
		return fmt.Errorf("RedisAddr must be set")
	}
	if conf.Redis.RedisHealthCheckInterval == "" {
		return fmt.Errorf("RedisHealthCheckInterval must be set")
	}
	return nil
}

func setupLogging() {
	log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})
	log.SetOutput(os.Stdout)

	level, err := logrus.ParseLevel(conf.Server.LogLevel)
	if err != nil {
		log.Warnf("Invalid log level '%s', defaulting to 'info'", conf.Server.LogLevel)
		level = logrus.InfoLevel
	}
	log.SetLevel(level)

	if conf.Server.LogFile != "" {
		file, err := os.OpenFile(conf.Server.LogFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			log.Warnf("Failed to open log file '%s', defaulting to stdout", conf.Server.LogFile)
		} else {
			log.SetOutput(file)
		}
	}
}

func logSystemInfo() {
	hostInfo, _ := host.Info()
	cpuInfo, _ := cpu.Info()
	memInfo, _ := mem.VirtualMemory()
	diskInfo, _ := disk.Usage(conf.Server.StoragePath)

	log.Infof("System Information:")
	log.Infof("Hostname: %s", hostInfo.Hostname)
	log.Infof("OS: %s %s", hostInfo.Platform, hostInfo.PlatformVersion)
	log.Infof("CPU: %s (%d cores)", cpuInfo[0].ModelName, cpuInfo[0].Cores)
	log.Infof("Memory: %d MB total, %d MB available", memInfo.Total/1024/1024, memInfo.Available/1024/1024)
	log.Infof("Disk: %d GB total, %d GB available", diskInfo.Total/1024/1024/1024, diskInfo.Free/1024/1024/1024)
}

func initMetrics() {
	uploadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name:    "upload_duration_seconds",
		Help:    "Duration of file uploads in seconds",
		Buckets: prometheus.DefBuckets,
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
		Name:    "download_duration_seconds",
		Help:    "Duration of file downloads in seconds",
		Buckets: prometheus.DefBuckets,
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
		Name: "memory_usage_bytes",
		Help: "Memory usage in bytes",
	})
	cpuUsage = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "cpu_usage_percent",
		Help: "CPU usage in percent",
	})
	activeConnections = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "active_connections",
		Help: "Number of active connections",
	})
	requestsTotal = prometheus.NewCounterVec(prometheus.CounterOpts{
		Name: "requests_total",
		Help: "Total number of HTTP requests",
	}, []string{"method", "endpoint"})
	goroutines = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "goroutines",
		Help: "Number of goroutines",
	})
	uploadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name:    "upload_size_bytes",
		Help:    "Size of uploaded files in bytes",
		Buckets: prometheus.ExponentialBuckets(1024, 2, 10),
	})
	downloadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name:    "download_size_bytes",
		Help:    "Size of downloaded files in bytes",
		Buckets: prometheus.ExponentialBuckets(1024, 2, 10),
	})

	prometheus.MustRegister(uploadDuration, uploadErrorsTotal, uploadsTotal, downloadDuration, downloadsTotal, downloadErrorsTotal, memoryUsage, cpuUsage, activeConnections, requestsTotal, goroutines, uploadSizeBytes, downloadSizeBytes)
}

func initClamAV(socket string) (*clamd.Clamd, error) {
	client := clamd.NewClamd(socket)
	err := client.Ping()
	if err != nil {
		return nil, fmt.Errorf("error pinging ClamAV: %v", err)
	}
	return client, nil
}

func initRedis() {
	redisClient = redis.NewClient(&redis.Options{
		Addr:     conf.Redis.RedisAddr,
		Password: conf.Redis.RedisPassword,
		DB:       conf.Redis.RedisDBIndex,
	})

	_, err := redisClient.Ping(context.Background()).Result()
	if err != nil {
		log.WithError(err).Warn("Redis client initialization failed. Continuing without Redis.")
		redisClient = nil
	} else {
		log.Info("Redis client initialized successfully.")
		// Redis is connected
	}
}

func MonitorRedisHealth(ctx context.Context, client *redis.Client, interval time.Duration) {
	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping Redis health monitor.")
			return
		case <-ticker.C:
			_, err := client.Ping(ctx).Result()
			if err != nil {
				log.WithError(err).Warn("Redis health check failed.")
				// Redis is not connected
			} else {
				// Redis is connected
			}
		}
	}
}

func initializeUploadWorkerPool(ctx context.Context, workers *WorkersConfig) {
	for i := 0; i < workers.NumWorkers; i++ {
		go uploadWorker(ctx)
	}
	log.Infof("Initialized %d upload workers", workers.NumWorkers)
}

func uploadWorker(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping upload worker.")
			return
		case task := <-uploadQueue:
			err := handleUpload()
			task.Result <- err
		}
	}
}

func initializeScanWorkerPool(ctx context.Context) {
	for i := 0; i < conf.ClamAV.NumScanWorkers; i++ {
		go scanWorker(ctx)
	}
	log.Infof("Initialized %d scan workers", conf.ClamAV.NumScanWorkers)
}

func scanWorker(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping scan worker.")
			return
		case task := <-scanQueue:
			err := handleScan()
			task.Result <- err
		}
	}
}

func handleUpload() error {
	// Handle file upload logic here
	return nil
}

func handleScan() error {
	// Handle file scan logic here
	return nil
}

func setupRouter() *http.ServeMux {
	router := http.NewServeMux()

	router.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		handleRequest(w, r)
	})

	return router
}

func handleRequest(w http.ResponseWriter, r *http.Request) {
	// Handle HTTP request logic here
}

func runFileCleaner(ctx context.Context, storagePath string, ttl time.Duration) {
	ticker := time.NewTicker(1 * time.Hour)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping file cleaner.")
			return
		case <-ticker.C:
			cleanExpiredFiles(storagePath, ttl)
		}
	}
}

func cleanExpiredFiles(storagePath string, ttl time.Duration) {
	// Clean expired files logic here
}

func setupGracefulShutdown(server *http.Server) {
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		<-sigChan
		log.Info("Shutting down server...")

		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()

		if err := server.Shutdown(ctx); err != nil {
			log.WithError(err).Error("Server shutdown failed")
		}

		cancel()
	}()
}

func monitorNetwork(ctx context.Context) {
	// Monitor network logic here
}

func handleNetworkEvents(ctx context.Context) {
	// Handle network events logic here
}

func updateSystemMetrics(ctx context.Context) {
	// Update system metrics logic here
}

func verifyAndCreateISOContainer() error {
	// Verify and create ISO container logic here
	return nil
}

func checkFreeSpaceWithRetry(retries int, delay time.Duration) error {
	for i := 0; i < retries; i++ {
		err := checkFreeSpace()
		if err == nil {
			return nil
		}
		log.WithError(err).Warnf("Free space check failed, retrying in %s...", delay)
		time.Sleep(delay)
	}
	return fmt.Errorf("free space check failed after %d retries", retries)
}

func checkFreeSpace() error {
	// Check free space logic here
	return nil
}

func parseDuration(durationStr string) time.Duration {
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		log.WithError(err).Warnf("Invalid duration '%s', defaulting to 1m", durationStr)
		return time.Minute
	}
	return duration
}
