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
	"mime"
	"net"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"regexp"
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

// Other configuration structs...

// Global variables
var (
	conf          Config
	log           = logrus.New()
	uploadQueue   chan UploadTask
	scanQueue     chan ScanTask
	fileInfoCache *cache.Cache
	clamClient    *clamd.Clamd
	redisClient   *redis.Client
)

// UploadTask represents a task for uploading files
type UploadTask struct {
	AbsFilename string
	Request     *http.Request
	Result      chan error
}

var networkEvents = make(chan NetworkEvent)

var (
	versionString = "2.1-dev"
)

type NetworkEvent struct {
	EventType string
	Timestamp time.Time
}

func init() {
	// Register Prometheus metrics
	// prometheus.MustRegister(uploadErrorsTotal)
}

func initClamAV(socket string) (*clamd.Clamd, error) {
	if socket == "" {
		return nil, errors.New("ClamAV socket path is empty")
	}

	logrus.Printf("Initializing ClamAV client with socket: %s", socket)
	return clamd.NewClamd(socket), nil
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
	conf.Server = ServerConfig{
		StoragePath:    "./storage",
		// FileTTL field removed or replaced with a valid field
		MetricsEnabled: true,
		//MetricsPort:    "2112",
		UnixSocket: false,
		LogFile:    "./logs/server.log",
		LogLevel:   "info",
		// ResumableUploads:       true,
	}

	conf.ISO = ISOConfig{
		Enabled: false,
	}

	conf.Timeouts = TimeoutConfig{
		ReadTimeout:  "10s",
		WriteTimeout: "10s",
		IdleTimeout:  "60s",
	}

	conf.Workers = WorkersConfig{
		UploadQueueSize: 10,
		NumWorkers:      4,
		NumScanWorkers:  4,
	}

	conf.ClamAV = ClamAVConfig{
		ClamAVEnabled: false,
		ClamAVSocket:  "/var/run/clamav/clamd.ctl",
	}

	conf.Redis = RedisConfig{
		RedisEnabled:             false,
		RedisHealthCheckInterval: "30s",
	}

	conf.Security = SecurityConfig{
		// SecretKey: "your-secret-key",
	}
}

func readConfig(path string, config *Config) error {
	v := viper.New()

	if path != "" {
		v.SetConfigFile(path)
	} else {
		// Search for config.toml in the current directory
		v.AddConfigPath(".")
		// and in /etc/hmac-file-server/
		v.AddConfigPath("/etc/hmac-file-server/")
		v.SetConfigName("config")
	}

	v.SetConfigType("toml")

	// Set default values
	v.SetDefault("server.metricsenabled", false)
	//v.SetDefault("server.metricsport", "2112") // Default to 2112 if not set

	if err := v.ReadInConfig(); err != nil {
		return fmt.Errorf("fatal error config file: %w", err)
	}

	if err := v.Unmarshal(config); err != nil {
		return fmt.Errorf("unable to decode into struct: %w", err)
	}

	return nil
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
	logrus.SetFormatter(&logrus.TextFormatter{
		FullTimestamp:   true,
		TimestampFormat: time.RFC3339,
	})
	logrus.SetLevel(logrus.InfoLevel)
}

func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-c
		logrus.Info("Shutting down server...")
		cancel()
		// Additional shutdown logic if necessary
		server.Shutdown(context.Background())
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
	clientIP := r.Header.Get("X-Real-IP")
	if clientIP == "" {
		clientIP = r.Header.Get("X-Forwarded-For")
	}
	if clientIP == "" {
		host, _, err := net.SplitHostPort(r.RemoteAddr)
		if err != nil {
			log.WithError(err).Warn("Failed to parse RemoteAddr")
			clientIP = r.RemoteAddr
		} else {
			clientIP = host
		}
	}

	log.WithFields(logrus.Fields{"method": r.Method, "url": r.URL.String(), "remote": clientIP}).Info("Incoming request")

	p := r.URL.Path
	a, err := url.ParseQuery(r.URL.RawQuery)
	if err != nil {
		log.Warn("Failed to parse query parameters")
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}

	fileStorePath := strings.TrimPrefix(p, "/")
	if fileStorePath == "" || fileStorePath == "/" {
		http.Error(w, "Forbidden", http.StatusForbidden)
		return
	}

	absFilename, err := sanitizeFilePath(conf.Server.StoragePath, fileStorePath)
	if err != nil {
		log.WithFields(logrus.Fields{"file": fileStorePath, "error": err}).Warn("Invalid file path")
		http.Error(w, "Invalid file path", http.StatusBadRequest)
		return
	}

	switch r.Method {
	case http.MethodPut, http.MethodPost:
		handleUpload(w, r, absFilename, a)
	case http.MethodHead, http.MethodGet:
		handleDownload(w, r, absFilename)
	case http.MethodOptions:
		w.Header().Set("Allow", "OPTIONS, GET, PUT, POST, HEAD")
		return
	default:
		log.WithField("method", r.Method).Warn("Invalid HTTP method for upload directory")
		http.Error(w, "Method Not Allowed", http.StatusMethodNotAllowed)
		return
	}
}

func handleDownload(w http.ResponseWriter, r *http.Request, fileStorePath string) {
	// Implement the download logic here
	http.ServeFile(w, r, fileStorePath)
}

func initializeWorkers(ctx context.Context) {
	for i := 0; i < conf.Workers.NumWorkers; i++ {
		go workers.UploadWorker(ctx, uploadQueue)
	}
	for i := 0; i < conf.Workers.NumScanWorkers; i++ {
		go workers.ScanWorker(ctx, scanQueue)
	}
	logrus.Infof("Initialized %d upload workers and %d scan workers",
		conf.Workers.NumWorkers, conf.Workers.NumScanWorkers)
}

// Removed unused gracefulShutdown function

func startMetricsServer() {
	http.Handle("/metrics", promhttp.Handler())
	logrus.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
	if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
		logrus.Fatalf("Metrics server failed: %v", err)
	}
}

func DownloadHandler(w http.ResponseWriter, r *http.Request) {
	if conf.Downloads.ResumableDownloadsEnabled {
		// Implement resumable download logic
	}

	if conf.Downloads.ChunkedDownloadsEnabled {
		// Implement chunked download logic
	}

	// Use conf.Downloads.ChunkSize as needed
}

func validateConfig(conf *Config) error {
	// Add validation logic here
	// Example: Check if required fields are set
	if conf.Server.StoragePath == "" {
		return errors.New("storage path is required")
	}
	if conf.Server.ListenPort == "" {
		return errors.New("listen port is required")
	}
	// Add more validation as needed
	return nil
}

func main() {
	logSystemInfo()
	// Set default configuration values
	setDefaults()

	// Flags for configuration file
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	// Log current working directory for debugging
	cwd, err := os.Getwd()
	if err != nil {
		logrus.Fatalf("Error getting current working directory: %v", err)
	}
	logrus.Infof("Current working directory: %s", cwd)

	// Load configuration
	conf, err := config.LoadConfig(configFile)
	if err != nil {
		logrus.Fatalf("Error reading config: %v", err)
	}
	logrus.Info("Configuration loaded successfully.")

	// Set log level based on configuration
	level, err := logrus.ParseLevel(conf.Server.LogLevel)
	if err != nil {
		logrus.Fatalf("Invalid log level: %v", err)
	}
	logrus.SetLevel(level)

	// Validate configuration
	if err := validateConfig(&conf); err != nil {
		logrus.Fatalf("Configuration validation failed: %v", err)
	}

	// Setup logging
	setupLogging()

	// Initialize Prometheus metrics
	metrics.InitMetrics()

	// Start Prometheus metrics server if enabled
	if conf.Server.MetricsEnabled {
		go metrics.StartMetricsServer(conf.Server.MetricsPort)
	}

	// Define context for goroutines
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Initialize worker pools with context
	initializeWorkers(ctx)

	// Handle graceful shutdown
	setupGracefulShutdown(cancel)

	// Additional initialization and server startup...

	// Example: Start HTTP server
	server := &http.Server{
		Addr:    ":" + conf.Server.ListenPort,
		Handler: setupRouter(),
	}

	logrus.Infof("Starting HMAC file server %s...", versionString)
	if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		logrus.Fatalf("Server failed: %v", err)
	}
}
