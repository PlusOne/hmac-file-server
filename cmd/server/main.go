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

	// "image" // Unused import removed
	_ "image/gif"  // Ensure GIF support
	_ "image/jpeg" // Ensure JPEG support
	_ "image/png"  // Ensure PNG support
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

	"github.com/disintegration/imaging"
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
	"gopkg.in/natefinch/lumberjack.v2"
)

// parseSize converts a human-readable size string to bytes
func parseSize(sizeStr string) (int64, error) {
	sizeStr = strings.TrimSpace(sizeStr)
	if len(sizeStr) < 2 {
		return 0, fmt.Errorf("invalid size string: %s", sizeStr)
	}

	unit := strings.ToUpper(sizeStr[len(sizeStr)-2:])
	valueStr := sizeStr[:len(sizeStr)-2]
	value, err := strconv.Atoi(valueStr)
	if err != nil {
		log.WithError(err).Errorf("Failed to parse size from input: %s", sizeStr)
		return 0, err
	}

	switch unit {
	case "KB":
		return int64(value) * 1 << 10, nil
	case "MB":
		return int64(value) * 1 << 20, nil
	case "GB":
		return int64(value) * 1 << 30, nil
	default:
		return 0, fmt.Errorf("unknown size unit: %s", unit)
	}
}

// parseTTL converts a human-readable TTL string to a time.Duration
func parseTTL(ttlStr string) (time.Duration, error) {
	ttlStr = strings.ToLower(strings.TrimSpace(ttlStr))
	if ttlStr == "" {
		return 0, fmt.Errorf("empty TTL string")
	}
	var valueStr string
	var unit rune
	for _, r := range ttlStr {
		if r >= '0' && r <= '9' {
			valueStr += string(r)
		} else {
			unit = r
			break
		}
	}
	val, err := strconv.Atoi(valueStr)
	if err != nil {
		return 0, fmt.Errorf("invalid TTL value: %v", err)
	}
	switch unit {
	case 'h': // hours
		return time.Duration(val) * time.Hour, nil
	case 'd': // days
		return time.Duration(val*24) * time.Hour, nil
	case 'm': // months (approx. 30 days)
		return time.Duration(val*24*30) * time.Hour, nil
	case 'y': // years (approx. 365 days)
		return time.Duration(val*24*365) * time.Hour, nil
	default: // fallback to Go's standard parsing
		return time.ParseDuration(ttlStr)
	}
}

// Configuration structures
type LoggingConfig struct {
	Level      string `mapstructure:"level"`
	File       string `mapstructure:"file"`
	MaxSize    int    `mapstructure:"max_size"`
	MaxBackups int    `mapstructure:"max_backups"`
	MaxAge     int    `mapstructure:"max_age"`
	Compress   bool   `mapstructure:"compress"`
}

type ServerConfig struct {
	ListenPort           string        `mapstructure:"listenport"`
	UnixSocket           bool          `mapstructure:"unixsocket"`
	StoragePath          string        `mapstructure:"storagepath"`
	MetricsEnabled       bool          `mapstructure:"metricsenabled"`
	MetricsPort          string        `mapstructure:"metricsport"`
	FileTTL              string        `mapstructure:"filettl"`
	MinFreeBytes         string        `mapstructure:"minfreebytes"`
	AutoAdjustWorkers    bool          `mapstructure:"autoadjustworkers"`
	NetworkEvents        bool          `mapstructure:"networkevents"`
	TempPath             string        `mapstructure:"temppath"`
	LoggingJSON          bool          `mapstructure:"loggingjson"`
	PIDFilePath          string        `mapstructure:"pidfilepath"`
	CleanUponExit        bool          `mapstructure:"cleanuponexit"`
	PreCaching           bool          `mapstructure:"precaching"`
	FileTTLEnabled       bool          `mapstructure:"filettlenabled"`
	DeduplicationEnabled bool          `mapstructure:"deduplicationenabled"`
	Logging              LoggingConfig `mapstructure:"logging"`
	GlobalExtensions     []string      `mapstructure:"globalextensions"`
}

type DeduplicationConfig struct {
	Enabled   bool   `mapstructure:"enabled"`
	Directory string `mapstructure:"directory"`
}

type ThumbnailsConfig struct {
	Enabled               bool   `mapstructure:"enabled"`
	Directory             string `mapstructure:"directory"`
	Size                  string `mapstructure:"size"`
	ThumbnailIntervalScan string `mapstructure:"thumbnailintervalscan"`
}

type ISOConfig struct {
	Enabled    bool   `mapstructure:"enabled"`
	Size       string `mapstructure:"size"`
	MountPoint string `mapstructure:"mountpoint"`
	Charset    string `mapstructure:"charset"`
	ContainerFile string `mapstructure:"containerfile"`
}

type TimeoutConfig struct {
	ReadTimeout  string `mapstructure:"readtimeout"`
	WriteTimeout string `mapstructure:"writetimeout"`
	IdleTimeout  string `mapstructure:"idletimeout"`
}

type SecurityConfig struct {
	Secret string `mapstructure:"secret"`
}

type VersioningConfig struct {
	EnableVersioning bool `mapstructure:"enableversioning"`
	MaxVersions      int  `mapstructure:"maxversions"`
}

type UploadsConfig struct {
	ResumableUploadsEnabled bool     `mapstructure:"resumableuploadsenabled"`
	ChunkedUploadsEnabled   bool     `mapstructure:"chunkeduploadsenabled"`
	ChunkSize               string   `mapstructure:"chunksize"`
	AllowedExtensions       []string `mapstructure:"allowedextensions"`
}

type DownloadsConfig struct {
	ResumableDownloadsEnabled bool     `mapstructure:"resumabledownloadsenabled"`
	ChunkedDownloadsEnabled   bool     `mapstructure:"chunkeddownloadsenabled"`
	ChunkSize                 string   `mapstructure:"chunksize"`
	AllowedExtensions         []string `mapstructure:"allowedextensions"`
}

type ClamAVConfig struct {
	ClamAVEnabled      bool     `mapstructure:"clamavenabled"`
	ClamAVSocket       string   `mapstructure:"clamavsocket"`
	NumScanWorkers     int      `mapstructure:"numscanworkers"`
	ScanFileExtensions []string `mapstructure:"scanfileextensions"`
}

type RedisConfig struct {
	RedisEnabled             bool   `mapstructure:"redisenabled"`
	RedisDBIndex             int    `mapstructure:"redisdbindex"`
	RedisAddr                string `mapstructure:"redisaddr"`
	RedisPassword            string `mapstructure:"redispassword"`
	RedisHealthCheckInterval string `mapstructure:"redishealthcheckinterval"`
}

type WorkersConfig struct {
	NumWorkers      int `mapstructure:"numworkers"`
	UploadQueueSize int `mapstructure:"uploadqueuesize"`
}

type FileConfig struct {
	FileRevision int `mapstructure:"filerevision"`
}

type BuildConfig struct {
	Version string `mapstructure:"version"`
}

type Config struct {
	Server        ServerConfig        `mapstructure:"server"`
	Logging       LoggingConfig       `mapstructure:"logging"`
	Deduplication DeduplicationConfig `mapstructure:"deduplication"`
	Thumbnails    ThumbnailsConfig    `mapstructure:"thumbnails"`
	ISO           ISOConfig           `mapstructure:"iso"`
	Timeouts      TimeoutConfig       `mapstructure:"timeouts"`
	Security      SecurityConfig      `mapstructure:"security"`
	Versioning    VersioningConfig    `mapstructure:"versioning"`
	Uploads       UploadsConfig       `mapstructure:"uploads"`
	Downloads     DownloadsConfig     `mapstructure:"downloads"`
	ClamAV        ClamAVConfig        `mapstructure:"clamav"`
	Redis         RedisConfig         `mapstructure:"redis"`
	Workers       WorkersConfig       `mapstructure:"workers"`
	File          FileConfig          `mapstructure:"file"`
	Build         BuildConfig         `mapstructure:"build"`
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

// Add a new field to store the creation date of files
type FileMetadata struct {
	CreationDate time.Time `json:"creationDate"`
}

var (
	conf              Config
	versionString     string
	log                      = logrus.New()
	uploadQueue              = make(chan UploadTask, 100)
	networkEvents            = make(chan NetworkEvent, 100)
	fileInfoCache     *cache.Cache
	fileMetadataCache *cache.Cache
	clamClient        *clamd.Clamd
	redisClient       *redis.Client
	redisConnected    bool
	mu                sync.RWMutex

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

	thumbnailProcessedTotal   prometheus.Counter
	thumbnailProcessingErrors prometheus.Counter
	thumbnailProcessingTime   prometheus.Histogram
	filesDeduplicatedTotal    prometheus.Counter
	deduplicationErrorsTotal  prometheus.Counter
	isoContainersCreatedTotal prometheus.Counter
	isoCreationErrorsTotal    prometheus.Counter
	isoContainersMountedTotal prometheus.Counter
	isoMountErrorsTotal       prometheus.Counter
)

var bufferPool = sync.Pool{
	New: func() interface{} {
		buf := make([]byte, 32*1024)
		return &buf
	},
}

const maxConcurrentOperations = 10

var semaphore = make(chan struct{}, maxConcurrentOperations)

var logMessages []string
var logMu sync.Mutex

func flushLogMessages() {
	logMu.Lock()
	defer logMu.Unlock()
	for _, msg := range logMessages {
		log.Info(msg)
	}
	logMessages = []string{}
}

// writePIDFile writes the current process ID to the specified pid file
func writePIDFile(pidPath string) error {
	pid := os.Getpid()
	pidStr := strconv.Itoa(pid)
	err := os.WriteFile(pidPath, []byte(pidStr), 0644)
	if err != nil {
		return fmt.Errorf("failed to write PID file: %v", err)
	}
	log.Infof("PID %d written to %s", pid, pidPath)
	return nil
}

// removePIDFile removes the PID file
func removePIDFile(pidPath string) {
	err := os.Remove(pidPath)
	if err != nil {
		log.Warnf("failed to remove PID file %s: %v", pidPath, err)
	} else {
		log.Infof("PID file %s removed", pidPath)
	}
}

// createAndMountISO creates an ISO container and mounts it to the specified mount point
func createAndMountISO(size, mountpoint, charset string) error {
	isoPath := conf.ISO.ContainerFile

	// Create an empty ISO file
	cmd := exec.Command("dd", "if=/dev/zero", fmt.Sprintf("of=%s", isoPath), fmt.Sprintf("bs=%s", size), "count=1")
	if err := cmd.Run(); err != nil {
		isoCreationErrorsTotal.Inc()
		return fmt.Errorf("failed to create ISO file: %w", err)
	}

	// Format the ISO file with a filesystem
	cmd = exec.Command("mkfs", "-t", "iso9660", "-input-charset", charset, isoPath)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to format ISO file: %w", err)
	}

	// Create the mount point directory if it doesn't exist
	if err := os.MkdirAll(mountpoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	// Mount the ISO file
	cmd = exec.Command("mount", "-o", "loop", isoPath, mountpoint)
	if err := cmd.Run(); err != nil {
		isoMountErrorsTotal.Inc()
		return fmt.Errorf("failed to mount ISO file: %w", err)
	}

	isoContainersCreatedTotal.Inc()
	isoContainersMountedTotal.Inc()
	return nil
}

func main() {
	setDefaults()

	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	// Initialize Viper
	viper.SetConfigType("toml")

	// Set default config path
	defaultConfigPath := "/etc/hmac-file-server/config.toml"

	// Attempt to load the default config
	viper.SetConfigFile(defaultConfigPath)
	if err := viper.ReadInConfig(); err != nil {
		// If default config not found, fallback to parent directory
		parentDirConfig := "../config.toml"
		viper.SetConfigFile(parentDirConfig)
		if err := viper.ReadInConfig(); err != nil {
			// If still not found and -config is provided, use it
			if configFile != "" {
				viper.SetConfigFile(configFile)
				if err := viper.ReadInConfig(); err != nil {
					fmt.Printf("Error loading config file: %v\n", err)
					os.Exit(1)
				}
			} else {
				fmt.Println("No configuration file found.")
				os.Exit(1)
			}
		}
	}

	err := readConfig(configFile, &conf)
	if err != nil {
		log.Fatalf("Error reading config: %v", err)
	}
	log.Info("Configuration loaded successfully.")

	err = validateConfig(&conf)
	if err != nil {
		log.Fatalf("Configuration validation failed: %v", err)
	}
	log.Info("Configuration validated successfully.")

	// Set log level based on configuration
	level, err := logrus.ParseLevel(conf.Logging.Level)
	if err != nil {
		log.Warnf("Invalid log level '%s', defaulting to 'info'", conf.Logging.Level)
		level = logrus.InfoLevel
	}
	log.SetLevel(level)
	log.Infof("Log level set to: %s", level.String())

	// Log configuration settings using [logging] section
	log.Infof("Server ListenPort: %s", conf.Server.ListenPort)
	log.Infof("Server UnixSocket: %v", conf.Server.UnixSocket)
	log.Infof("Server StoragePath: %s", conf.Server.StoragePath)
	log.Infof("Logging Level: %s", conf.Logging.Level)
	log.Infof("Logging File: %s", conf.Logging.File)
	log.Infof("Server MetricsEnabled: %v", conf.Server.MetricsEnabled)
	log.Infof("Server MetricsPort: %s", conf.Server.MetricsPort)
	log.Infof("Server FileTTL: %s", conf.Server.FileTTL)
	log.Infof("Server MinFreeBytes: %s", conf.Server.MinFreeBytes)
	log.Infof("Server AutoAdjustWorkers: %v", conf.Server.AutoAdjustWorkers)
	log.Infof("Server NetworkEvents: %v", conf.Server.NetworkEvents)
	log.Infof("Server TempPath: %s", conf.Server.TempPath)
	log.Infof("Server LoggingJSON: %v", conf.Server.LoggingJSON)
	log.Infof("Server PIDFilePath: %s", conf.Server.PIDFilePath)
	log.Infof("Server CleanUponExit: %v", conf.Server.CleanUponExit)
	log.Infof("Server PreCaching: %v", conf.Server.PreCaching)
	log.Infof("Server FileTTLEnabled: %v", conf.Server.FileTTLEnabled)
	log.Infof("Server DeduplicationEnabled: %v", conf.Server.DeduplicationEnabled)

	err = writePIDFile(conf.Server.PIDFilePath) // Write PID file after config is loaded
	if err != nil {
		log.Fatalf("Error writing PID file: %v", err)
	}

	initializeWorkerSettings(&conf.Server, &conf.Workers, &conf.ClamAV)

	if conf.ISO.Enabled {
		err := createAndMountISO(conf.ISO.Size, conf.ISO.MountPoint, conf.ISO.Charset)
		if err != nil {
			log.Fatalf("Failed to create and mount ISO container: %v", err)
		}
		log.Infof("ISO container mounted at %s", conf.ISO.MountPoint)
	}

	// Set storage path to ISO mount point if ISO is enabled
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)
	fileMetadataCache = cache.New(5*time.Minute, 10*time.Minute)

	if conf.Server.PreCaching { // Conditionally perform pre-caching
		go func() {
			log.Info("Starting pre-caching of storage path...")
			err := precacheStoragePath(storagePath)
			if err != nil {
				log.Warnf("Pre-caching storage path failed: %v", err)
			} else {
				log.Info("Pre-cached all files in the storage path.")
				log.Info("Pre-caching status: complete.")
			}
		}()
	}

	err = os.MkdirAll(storagePath, os.ModePerm)
	if err != nil {
		log.Fatalf("Error creating store directory: %v", err)
	}
	log.WithField("directory", storagePath).Info("Store directory is ready")

	// Ensure thumbnail directory exists if thumbnails are enabled
	if conf.Thumbnails.Enabled {
		if err := os.MkdirAll(conf.Thumbnails.Directory, os.ModePerm); err != nil {
			log.Fatalf("Failed to create thumbnail directory: %v", err)
		}
	}

	err = checkFreeSpaceWithRetry(storagePath, 3, 5*time.Second)
	if err != nil {
		log.Fatalf("Insufficient free space: %v", err)
	}

	setupLogging()
	logSystemInfo()
	initMetrics()
	log.Info("Prometheus metrics initialized.")

	uploadQueue = make(chan UploadTask, conf.Workers.UploadQueueSize)
	scanQueue = make(chan ScanTask, conf.Workers.UploadQueueSize)
	networkEvents = make(chan NetworkEvent, 100)
	log.Info("Upload, scan, and network event channels initialized.")

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if conf.Server.NetworkEvents {
		go monitorNetwork(ctx)
		go handleNetworkEvents(ctx)
	}
	go updateSystemMetrics(ctx)

	if conf.ClamAV.ClamAVEnabled {
		clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			log.WithError(err).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
		} else {
			log.Info("ClamAV client initialized successfully.")
		}
	}

	if conf.Redis.RedisEnabled {
		initRedis()
	}

	initializeUploadWorkerPool(ctx, &conf.Workers)
	if conf.ClamAV.ClamAVEnabled && clamClient != nil {
		initializeScanWorkerPool(ctx)
	}

	if conf.Redis.RedisEnabled && redisClient != nil {
		go MonitorRedisHealth(ctx, redisClient, parseDuration(conf.Redis.RedisHealthCheckInterval))
	}

	router := setupRouter()

	fileTTL, err := parseTTL(conf.Server.FileTTL)
	if err != nil {
		log.Fatalf("Invalid FileTTL: %v", err)
	}
	go runFileCleaner(ctx, storagePath, fileTTL)

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

	server := &http.Server{
		Addr:           ":" + conf.Server.ListenPort,
		Handler:        router,
		ReadTimeout:    readTimeout,
		WriteTimeout:   writeTimeout,
		IdleTimeout:    idleTimeout,
		MaxHeaderBytes: 1 << 20, // 1 MB
	}

	if conf.Server.MetricsEnabled {
		var wg sync.WaitGroup
		go func() {
			http.Handle("/metrics", promhttp.Handler())
			log.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
			if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
				log.Fatalf("Metrics server failed: %v", err)
			}
			wg.Wait()
		}()
	}

	setupGracefulShutdown(server, cancel)

	if conf.Server.AutoAdjustWorkers {
		go monitorWorkerPerformance(ctx, &conf.Server, &conf.Workers, &conf.ClamAV)
	}

	// Schedule periodic thumbnail generation
	scheduleThumbnailGeneration(ctx)

	versionString = conf.Build.Version
	log.Infof("Running version: %s", versionString)

	log.Infof("Starting HMAC file server %s...", versionString)
	if conf.Server.UnixSocket {
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
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	}

	// Start file cleanup in a separate goroutine
	go handleFileCleanup(&conf)
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
	if v.Available < 4*1024*1024*1024 { // Less than 4GB available
		numWorkers = max(numWorkers/2, 1)
	} else if v.Available < 8*1024*1024*1024 { // Less than 8GB available
		numWorkers = max(numWorkers*3/4, 1)
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
	if err := viper.ReadInConfig(); err != nil {
		log.WithError(err).Errorf("Unable to read config from %s", configFilename)
		return err
	}
	if err := viper.Unmarshal(conf); err != nil {
		return fmt.Errorf("unable to decode config into struct: %v", err)
	}
	return nil
}

func setDefaults() {
	viper.SetDefault("server.listenport", "8080")
	viper.SetDefault("server.unixsocket", false)
	viper.SetDefault("server.storagepath", "./uploads")
	viper.SetDefault("server.metricsenabled", true)
	viper.SetDefault("server.metricsport", "9090")
	viper.SetDefault("server.filettl", "8760h")
	viper.SetDefault("server.minfreebytes", "100MB")
	viper.SetDefault("server.autoadjustworkers", true)
	viper.SetDefault("server.networkevents", true)
	viper.SetDefault("server.precaching", true)
	viper.SetDefault("server.pidfilepath", "/var/run/hmacfileserver.pid")
	viper.SetDefault("server.loggingjson", false)
	viper.SetDefault("server.filettlenabled", true)
	viper.SetDefault("server.deduplicationenabled", true)

	viper.SetDefault("timeouts.readtimeout", "4800s")
	viper.SetDefault("timeouts.writetimeout", "4800s")
	viper.SetDefault("timeouts.idletimeout", "4800s")

	viper.SetDefault("security.secret", "changeme")

	viper.SetDefault("versioning.enableversioning", false)
	viper.SetDefault("versioning.maxversions", 1)

	viper.SetDefault("uploads.resumableuploadsenabled", true)
	viper.SetDefault("uploads.chunkeduploadsenabled", true)
	viper.SetDefault("uploads.chunksize", "8192")
	viper.SetDefault("uploads.allowedextensions", []string{
		".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp",
		".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v",
		".3gp", ".3g2", ".mp3", ".ogg",
	})

	viper.SetDefault("clamav.clamavenabled", true)
	viper.SetDefault("clamav.clamavsocket", "/var/run/clamav/clamd.ctl")
	viper.SetDefault("clamav.numscanworkers", 2)

	viper.SetDefault("redis.redisenabled", true)
	viper.SetDefault("redis.redisaddr", "localhost:6379")
	viper.SetDefault("redis.redispassword", "")
	viper.SetDefault("redis.redisdbindex", 0)
	viper.SetDefault("redis.redishealthcheckinterval", "120s")

	viper.SetDefault("workers.numworkers", 4)
	viper.SetDefault("workers.uploadqueuesize", 50)

	viper.SetDefault("deduplication.enabled", true)

	viper.SetDefault("iso.enabled", true)
	viper.SetDefault("iso.size", "1GB")
	viper.SetDefault("iso.mountpoint", "/mnt/iso")
	viper.SetDefault("iso.charset", "utf-8")

	viper.SetDefault("thumbnails.thumbnailintervalscan", "24h")

	// Logging defaults
	viper.SetDefault("logging.level", "info")
	viper.SetDefault("logging.file", "/var/log/hmac-file-server.log")
	viper.SetDefault("logging.max_size", 100)
	viper.SetDefault("logging.max_backups", 7)
	viper.SetDefault("logging.max_age", 30)
	viper.SetDefault("logging.compress", true)
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

	if _, err := time.ParseDuration(conf.Timeouts.ReadTimeout); err != nil {
		return fmt.Errorf("invalid ReadTimeout: %v", err)
	}
	if _, err := time.ParseDuration(conf.Timeouts.WriteTimeout); err != nil {
		return fmt.Errorf("invalid WriteTimeout: %v", err)
	}
	if _, err := time.ParseDuration(conf.Timeouts.IdleTimeout); err != nil {
		return fmt.Errorf("invalid IdleTimeout: %v", err)
	}

	if conf.Redis.RedisEnabled {
		if conf.Redis.RedisAddr == "" {
			return fmt.Errorf("RedisAddr must be set when Redis is enabled")
		}
	}

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

	// Validate Downloads Configuration
	if conf.Downloads.ResumableDownloadsEnabled {
		if conf.Downloads.ChunkSize == "" {
			return fmt.Errorf("downloads.chunkSize must be set when resumable downloads are enabled")
		}
		if _, err := parseSize(conf.Downloads.ChunkSize); err != nil {
			return fmt.Errorf("invalid downloads.chunkSize: %v", err)
		}
	}

	// Validate Uploads Configuration
	if conf.Uploads.ResumableUploadsEnabled {
		if conf.Uploads.ChunkSize == "" {
			return fmt.Errorf("uploads.chunkSize must be set when resumable uploads are enabled")
		}
		if len(conf.Uploads.AllowedExtensions) == 0 {
			return fmt.Errorf("uploads.allowedextensions must have at least one extension")
		}
	}

	// Validate Workers Configuration
	if conf.Workers.NumWorkers <= 0 {
		return fmt.Errorf("workers.numWorkers must be greater than 0")
	}
	if conf.Workers.UploadQueueSize <= 0 {
		return fmt.Errorf("workers.uploadQueueSize must be greater than 0")
	}

	// Validate ClamAV Configuration
	if conf.ClamAV.ClamAVEnabled {
		if conf.ClamAV.ClamAVSocket == "" {
			return fmt.Errorf("clamav.clamAVSocket must be set when ClamAV is enabled")
		}
		if conf.ClamAV.NumScanWorkers <= 0 {
			return fmt.Errorf("clamav.numScanWorkers must be greater than 0")
		}
		if len(conf.ClamAV.ScanFileExtensions) == 0 {
			return fmt.Errorf("clamav.scanfileextensions must have at least one extension when ClamAV is enabled")
		}
	}

	// Validate ISO Configuration
	if conf.ISO.Enabled {
		if conf.ISO.Size == "" {
			return fmt.Errorf("iso.size must be set when ISO is enabled")
		}
		if conf.ISO.MountPoint == "" {
			return fmt.Errorf("iso.mountPoint must be set when ISO is enabled")
		}
		if conf.ISO.Charset == "" {
			return fmt.Errorf("iso.charset must be set when ISO is enabled")
		}
	}

	// Verify that the primary storage path is accessible
	if err := checkStoragePath(conf.Server.StoragePath); err != nil {
		return fmt.Errorf("storage path check failed: %w", err)
	}

	// Verify Thumbnails Configuration
	if conf.Thumbnails.Enabled {
		if conf.Thumbnails.Directory == "" {
			return fmt.Errorf("thumbnails.directory must be set when thumbnails are enabled")
		}
		if conf.Thumbnails.Size == "" {
			return fmt.Errorf("thumbnails.size must be set when thumbnails are enabled")
		}
		if _, err := time.ParseDuration(conf.Thumbnails.ThumbnailIntervalScan); err != nil {
			return fmt.Errorf("invalid thumbnails.thumbnailintervalscan: %v", err)
		}
	}

	// Verify Versioning Configuration
	if conf.Versioning.EnableVersioning {
		if conf.Versioning.MaxVersions <= 0 {
			return fmt.Errorf("versioning.maxversions must be greater than 0 when versioning is enabled")
		}
	}

	// Verify Uploads Configuration
	if conf.Uploads.ChunkedUploadsEnabled {
		if conf.Uploads.ChunkSize == "" {
			return fmt.Errorf("uploads.chunksize must be set when chunked uploads are enabled")
		}
		if len(conf.Uploads.AllowedExtensions) == 0 && len(conf.Server.GlobalExtensions) == 0 {
			return fmt.Errorf("uploads.allowedextensions must have at least one extension when globalextensions is not set")
		}
	}

	// Verify Downloads Configuration
	if conf.Downloads.ChunkedDownloadsEnabled {
		if conf.Downloads.ChunkSize == "" {
			return fmt.Errorf("downloads.chunksize must be set when chunked downloads are enabled")
		}
	}

	// Verify Redis Configuration
	if conf.Redis.RedisEnabled {
		if conf.Redis.RedisAddr == "" {
			return fmt.Errorf("redis.redisaddr must be set when Redis is enabled")
		}
		// Additional Redis validations can be added here
	}

	// Verify ClamAV Configuration
	if conf.ClamAV.ClamAVEnabled {
		if conf.ClamAV.ClamAVSocket == "" {
			return fmt.Errorf("clamav.clamavsocket must be set when ClamAV is enabled")
		}
		if conf.ClamAV.NumScanWorkers <= 0 {
			return fmt.Errorf("clamav.numscanworkers must be greater than 0 when ClamAV is enabled")
		}
		if len(conf.ClamAV.ScanFileExtensions) == 0 {
			return fmt.Errorf("clamav.scanfileextensions must have at least one extension when ClamAV is enabled")
		}
	}

	// Verify ISO Configuration
	if conf.ISO.Enabled {
		if conf.ISO.Size == "" {
			return fmt.Errorf("iso.size must be set when ISO is enabled")
		}
		if conf.ISO.MountPoint == "" {
			return fmt.Errorf("iso.mountpoint must be set when ISO is enabled")
		}
		if conf.ISO.Charset == "" {
			return fmt.Errorf("iso.charset must be set when ISO is enabled")
		}
	}

	// Verify Security Configuration
	if conf.Security.Secret == "" {
		return fmt.Errorf("security.secret must be set")
	}

	// Verify Workers Configuration
	if conf.Workers.NumWorkers <= 0 {
		return fmt.Errorf("workers.numworkers must be greater than 0")
	}
	if conf.Workers.UploadQueueSize <= 0 {
		return fmt.Errorf("workers.uploadqueuesize must be greater than 0")
	}

	// Verify File Configuration
	if conf.File.FileRevision < 0 {
		return fmt.Errorf("file.filerevision cannot be negative")
	}

	// Validate Global Extensions
	if len(conf.Server.GlobalExtensions) == 0 {
		if len(conf.Uploads.AllowedExtensions) == 0 && len(conf.Downloads.AllowedExtensions) == 0 {
			return fmt.Errorf("server.globalextensions, uploads.allowedextensions, or downloads.allowedextensions must have at least one extension")
		}
	}

	// Validate Uploads Configuration
	if conf.Uploads.ResumableUploadsEnabled {
		if conf.Uploads.ChunkSize == "" {
			return fmt.Errorf("uploads.chunksize must be set when resumable uploads are enabled")
		}
		if len(conf.Uploads.AllowedExtensions) == 0 && len(conf.Server.GlobalExtensions) == 0 {
			return fmt.Errorf("uploads.allowedextensions must have at least one extension when globalextensions is not set")
		}
	}

	// Validate Downloads Configuration
	if conf.Downloads.ResumableDownloadsEnabled {
		if conf.Downloads.ChunkSize == "" {
			return fmt.Errorf("downloads.chunksize must be set when resumable downloads are enabled")
		}
		if len(conf.Downloads.AllowedExtensions) == 0 && len(conf.Server.GlobalExtensions) == 0 {
			return fmt.Errorf("downloads.allowedextensions must have at least one extension when globalextensions is not set")
		}
	}

	// Additional configuration validations can be added here

	return nil
}

func checkStoragePath(path string) error {
	info, err := os.Stat(path)
	if os.IsNotExist(err) {
		log.Errorf("Storage path does not exist: %s", path)
		return err
	}
	if err != nil {
		log.Errorf("Error accessing storage path: %v", err)
		return err
	}
	if !info.IsDir() {
		return fmt.Errorf("not a directory: %s", path)
	}
	log.Infof("Verified storage path is accessible: %s", path)
	return nil
}

func setupLogging() {
	level, err := logrus.ParseLevel(conf.Logging.Level)
	if err != nil {
		log.Fatalf("Invalid log level: %s", conf.Logging.Level)
	}
	log.SetLevel(level)

	if conf.Logging.File != "" {
		log.SetOutput(&lumberjack.Logger{
			Filename:   conf.Logging.File,
			MaxSize:    100, // megabytes
			MaxBackups: 3,
			MaxAge:     28,   // days
			Compress:   true, // compress old log files
		})
	} else {
		log.SetOutput(os.Stdout)
	}

	log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})

	// Initialize lumberjack for log rotation using [logging] config
	lj := &lumberjack.Logger{
		Filename:   conf.Logging.File,
		MaxSize:    conf.Logging.MaxSize, // megabytes
		MaxBackups: conf.Logging.MaxBackups,
		MaxAge:     conf.Logging.MaxAge,   //days
		Compress:   conf.Logging.Compress, // disabled by default
	}

	log.SetOutput(lj)
	log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})

	log.Infof("Logging initialized with level: %s, file: %s", conf.Logging.Level, conf.Logging.File)
}

func logSystemInfo() {
	log.Info("========================================")
	log.Infof("       HMAC File Server - %s          ", versionString)
	log.Info("  Secure File Handling with HMAC Auth   ")
	log.Info("========================================")

	log.Info("Features: Prometheus Metrics, Chunked Uploads, ClamAV Scanning")
	log.Info("Build Date: 2024-12-27")

	log.Infof("Operating System: %s", runtime.GOOS)
	log.Infof("Architecture: %s", runtime.GOARCH)
	log.Infof("Number of CPUs: %d", runtime.NumCPU())
	log.Infof("Go Version: %s", runtime.Version())

	v, _ := mem.VirtualMemory()
	log.Infof("Total Memory: %v MB", v.Total/1024/1024)
	log.Infof("Free Memory: %v MB", v.Free/1024/1024)
	log.Infof("Used Memory: %v MB", v.Used/1024/1024)

	cpuInfo, _ := cpu.Info()
	uniqueCPUModels := make(map[string]bool)
	for _, info := range cpuInfo {
		if !uniqueCPUModels[info.ModelName] {
			log.Infof("CPU Model: %s, Cores: %d, Mhz: %f", info.ModelName, info.Cores, info.Mhz)
			uniqueCPUModels[info.ModelName] = true
		}
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

func initMetrics() {
	uploadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{Namespace: "hmac", Name: "file_server_upload_duration_seconds", Help: "Histogram of file upload duration."})
	uploadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{Namespace: "hmac", Name: "file_server_upload_errors_total", Help: "Total number of file upload errors."})
	uploadsTotal = prometheus.NewCounter(prometheus.CounterOpts{Namespace: "hmac", Name: "file_server_uploads_total", Help: "Total number of successful file uploads."})
	downloadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{Namespace: "hmac", Name: "file_server_download_duration_seconds", Help: "Histogram of file download duration."})
	downloadsTotal = prometheus.NewCounter(prometheus.CounterOpts{Namespace: "hmac", Name: "file_server_downloads_total", Help: "Total number of successful file downloads."})
	downloadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{Namespace: "hmac", Name: "file_server_download_errors_total", Help: "Total number of file download errors."})
	memoryUsage = prometheus.NewGauge(prometheus.GaugeOpts{Namespace: "hmac", Name: "memory_usage_bytes", Help: "Current memory usage in bytes."})
	cpuUsage = prometheus.NewGauge(prometheus.GaugeOpts{Namespace: "hmac", Name: "cpu_usage_percent", Help: "CPU usage as a percentage."})
	activeConnections = prometheus.NewGauge(prometheus.GaugeOpts{Namespace: "hmac", Name: "active_connections_total", Help: "Total number of active connections."})
	requestsTotal = prometheus.NewCounterVec(prometheus.CounterOpts{Namespace: "hmac", Name: "http_requests_total", Help: "Total HTTP requests."}, []string{"method", "path"})
	goroutines = prometheus.NewGauge(prometheus.GaugeOpts{Namespace: "hmac", Name: "goroutines_count", Help: "Number of goroutines."})
	uploadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{Namespace: "hmac", Name: "file_server_upload_size_bytes", Help: "Histogram of uploaded file sizes."})
	downloadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{Namespace: "hmac", Name: "file_server_download_size_bytes", Help: "Histogram of downloaded file sizes."})

	thumbnailProcessedTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "thumbnails_processed_total",
		Help:      "Total number of thumbnails processed.",
	})
	thumbnailProcessingErrors = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "thumbnail_processing_errors_total",
		Help:      "Total number of thumbnail processing errors.",
	})
	thumbnailProcessingTime = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "thumbnail_processing_time_seconds",
		Help:      "Histogram of thumbnail processing time.",
	})

	filesDeduplicatedTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "files_deduplicated_total",
		Help:      "Total number of files deduplicated.",
	})
	deduplicationErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "deduplication_errors_total",
		Help:      "Total number of deduplication errors.",
	})
	isoContainersCreatedTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "iso_containers_created_total",
		Help:      "Total number of ISO containers created.",
	})
	isoCreationErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "iso_creation_errors_total",
		Help:      "Total number of ISO creation errors.",
	})
	isoContainersMountedTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "iso_containers_mounted_total",
		Help:      "Total number of ISO containers mounted.",
	})
	isoMountErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "iso_mount_errors_total",
		Help:      "Total number of ISO mount errors.",
	})

	if conf.Server.MetricsEnabled {
		prometheus.MustRegister(uploadDuration, uploadErrorsTotal, uploadsTotal)
		prometheus.MustRegister(downloadDuration, downloadsTotal, downloadErrorsTotal)
		prometheus.MustRegister(memoryUsage, cpuUsage, activeConnections, requestsTotal, goroutines, uploadSizeBytes, downloadSizeBytes)
		prometheus.MustRegister(thumbnailProcessedTotal, thumbnailProcessingErrors, thumbnailProcessingTime)
		prometheus.MustRegister(filesDeduplicatedTotal, deduplicationErrorsTotal)
		prometheus.MustRegister(isoContainersCreatedTotal, isoCreationErrorsTotal)
		prometheus.MustRegister(isoContainersMountedTotal, isoMountErrorsTotal)
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

func fileExists(filePath string) (bool, int64) {
	log.Debugf("Checking file existence: %s", filePath)
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

func isExtensionAllowed(filename string) bool {
	var allowedExtensions []string
	if len(conf.Server.GlobalExtensions) > 0 {
		allowedExtensions = conf.Server.GlobalExtensions
	} else {
		allowedExtensions = append(conf.Uploads.AllowedExtensions, conf.Downloads.AllowedExtensions...)
	}

	if len(allowedExtensions) == 0 {
		return true
	}

	ext := strings.ToLower(filepath.Ext(filename))
	for _, allowedExt := range allowedExtensions {
		if strings.ToLower(allowedExt) == ext {
			return true
		}
	}
	return false
}

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

func processUpload(task UploadTask) error {
	log.Infof("Started processing upload for file: %s", task.AbsFilename)
	semaphore <- struct{}{}
	defer func() { <-semaphore }()

	absFilename := task.AbsFilename
	tempFilename := absFilename + ".tmp"
	r := task.Request

	log.Infof("Processing upload for file: %s", absFilename)
	startTime := time.Now()

	if conf.Uploads.ChunkedUploadsEnabled {
		chunkSize, err := parseSize(conf.Uploads.ChunkSize)
		if err != nil {
			log.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Error("Error parsing chunk size")
			uploadDuration.Observe(time.Since(startTime).Seconds())
			return err
		}
		err = handleChunkedUpload(tempFilename, r, int(chunkSize))
		if err != nil {
			uploadDuration.Observe(time.Since(startTime).Seconds())
			log.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Error("Failed to handle chunked upload")
			return err
		}
	} else {
		err := createFile(tempFilename, r)
		if err != nil {
			log.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Error("Error creating file")
			uploadDuration.Observe(time.Since(startTime).Seconds())
			return err
		}
	}

	if clamClient != nil && shouldScanFile(absFilename) {
		err := scanFileWithClamAV(tempFilename)
		if err != nil {
			log.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Warn("ClamAV detected a virus or scan failed")
			os.Remove(tempFilename)
			uploadErrorsTotal.Inc()
			return err
		}
		log.Infof("ClamAV scan passed for file: %s", tempFilename)
	} else {
		log.Warn("ClamAV is not available or file extension not in scan list. Proceeding without virus scan.")
	}

	if conf.Versioning.EnableVersioning {
		existing, _ := fileExists(absFilename)
		if existing {
			log.Infof("File %s exists. Initiating versioning.", absFilename)
			err := versionFile(absFilename)
			if err != nil {
				log.WithFields(logrus.Fields{"file": absFilename, "error": err}).Error("Error versioning file")
				os.Remove(tempFilename)
				return err
			}
			log.Infof("File versioned successfully: %s", absFilename)
		}
	}

	// Compute file hash first:
	hashVal, err := computeSHA256(context.Background(), tempFilename)
	if err != nil {
		log.Errorf("Could not compute hash: %v", err)
		return err
	}
	log.Infof("Computed hash for %s: %s", absFilename, hashVal)

	// Check Redis for existing entry:
	existingPath, redisErr := redisClient.Get(context.Background(), hashVal).Result()
	if redisErr == nil && existingPath != "" {
		log.Warnf("Duplicate upload detected. Using existing file at: %s", existingPath)
		return nil
	}

	log.Debugf("Renaming temp file %s -> %s", tempFilename, absFilename)
	err = os.Rename(tempFilename, absFilename)
	defer func() {
		if err != nil {
			os.Remove(tempFilename)
		}
	}()
	if err != nil {
		os.Remove(tempFilename)
		return fmt.Errorf("failed to move file to final destination: %w", err)
	}
	log.Infof("File moved to final destination: %s", absFilename)

	// Store file creation date in metadata cache
	fileMetadataCache.Set(absFilename, FileMetadata{CreationDate: time.Now()}, cache.DefaultExpiration)

	log.Debugf("Verifying existence immediately after rename: %s", absFilename)
	exists, size := fileExists(absFilename)
	log.Debugf("Exists? %v, Size: %d", exists, size)

	// Gajim and Dino do not require a callback or acknowledgement beyond HTTP success.
	callbackURL := r.Header.Get("Callback-URL")
	if callbackURL != "" {
		log.Warnf("Callback-URL provided (%s) but not needed. Ignoring.", callbackURL)
		// We do not block or wait, just ignore.
	}

	if conf.Server.DeduplicationEnabled {
		log.Debugf("Performing deduplication check for %s", task.AbsFilename)
		log.Debugf("Dedup check: Using hash %s to find existing path", hashVal)
		log.Debugf("Existing path found in Redis: %s", existingPath)
		err = handleDeduplication(context.Background(), absFilename)
		if err != nil {
			log.WithError(err).Error("Deduplication failed")
			uploadErrorsTotal.Inc()
			return err
		}
		log.Infof("Deduplication handled successfully for file: %s", absFilename)
	}

	if conf.ISO.Enabled {
		err = handleISOContainer(absFilename)
		if err != nil {
			log.WithError(err).Error("ISO container handling failed")
			uploadErrorsTotal.Inc()
			return err
		}
		log.Infof("ISO container handled successfully for file: %s", absFilename)
	}

	if redisClient != nil {
		errSet := redisClient.Set(context.Background(), hashVal, absFilename, 0).Err()
		if errSet != nil {
			log.Warnf("Failed storing hash reference: %v", errSet)
		} else {
			log.Infof("Hash reference stored: %s -> %s", hashVal, absFilename)
		}
	}

	log.WithFields(logrus.Fields{"file": absFilename}).Info("File uploaded and processed successfully")

	uploadDuration.Observe(time.Since(startTime).Seconds())
	uploadsTotal.Inc()
	log.Infof("Finished processing upload for file: %s", task.AbsFilename)
	return nil
}

func createFile(tempFilename string, r *http.Request) error {
	err := os.MkdirAll(filepath.Dir(tempFilename), 0755)
	if err != nil {
		return err
	}

	file, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return err
	}
	defer file.Close()

	bufWriter := bufio.NewWriter(file)
	defer bufWriter.Flush()

	bufPtr := bufferPool.Get().(*[]byte) // Correct type assertion
	defer bufferPool.Put(bufPtr)

	_, err = io.CopyBuffer(bufWriter, r.Body, *bufPtr)
	if err != nil {
		return err
	}

	return nil
}

func shouldScanFile(filename string) bool {
	ext := strings.ToLower(filepath.Ext(filename))
	for _, scanExt := range conf.ClamAV.ScanFileExtensions {
		if strings.ToLower(scanExt) == ext {
			return true
		}
	}
	return false
}

func uploadWorker(ctx context.Context, workerID int) {
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
			if err != nil {
				log.Errorf("Worker %d failed to process file %s: %v", workerID, task.AbsFilename, err)
			} else {
				log.Infof("Worker %d successfully processed file: %s", workerID, task.AbsFilename)
			}
			task.Result <- err
		}
	}
}

func initializeUploadWorkerPool(ctx context.Context, w *WorkersConfig) {
	var workerIDs []int
	for i := 0; i < w.NumWorkers; i++ {
		go uploadWorker(ctx, i)
		workerIDs = append(workerIDs, i)
	}
	log.Infof("Initialized %d upload workers: %v", w.NumWorkers, workerIDs)
}

func scanWorker(ctx context.Context, workerID int) {
	defer log.WithField("worker_id", workerID).Info("Scan worker stopping")
	for {
		select {
		case <-ctx.Done():
			return
		case task, ok := <-scanQueue:
			if !ok {
				return
			}
			err := scanFileWithClamAV(task.AbsFilename)
			if err != nil {
				log.WithFields(logrus.Fields{"worker_id": workerID, "file": task.AbsFilename, "error": err}).Error("Failed to scan file")
			} else {
				log.WithFields(logrus.Fields{"worker_id": workerID, "file": task.AbsFilename}).Info("Successfully scanned file")
			}
			task.Result <- err
			close(task.Result)
		}
	}
}

func initializeScanWorkerPool(ctx context.Context) {
	var workerIDs []int
	for i := 0; i < conf.ClamAV.NumScanWorkers; i++ {
		go scanWorker(ctx, i)
		workerIDs = append(workerIDs, i)
	}
	log.Infof("Initialized %d scan workers: %v", conf.ClamAV.NumScanWorkers, workerIDs)
}

func setupRouter() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRequest)
	if (conf.Server.MetricsEnabled) {
		mux.Handle("/metrics", promhttp.Handler())
	}
	handler := loggingMiddleware(mux)
	handler = recoveryMiddleware(handler)
	handler = corsMiddleware(handler)
	return handler
}

func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requestsTotal.WithLabelValues(r.Method, r.URL.Path).Inc()
		next.ServeHTTP(w, r)
	})
}

func recoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if rec := recover(); rec != nil {
				log.WithFields(logrus.Fields{"method": r.Method, "url": r.URL.String(), "error": rec}).Error("Panic recovered in handler")
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
}

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
		log.WithFields(logrus.Fields{
			"event":    "AccessAttempt",
			"severity": "warning",
		}).Warn("Access to root directory is forbidden")
		http.Error(w, "Forbidden", http.StatusForbidden)
		flushLogMessages()
		return
	} else if fileStorePath[0] == '/' {
		fileStorePath = fileStorePath[1:]
	}

	absFilename, err := sanitizeFilePath(conf.Server.StoragePath, fileStorePath)
	if err != nil {
		log.WithFields(logrus.Fields{"file": fileStorePath, "error": err}).Warn("Invalid file path")
		http.Error(w, "Invalid file path", http.StatusBadRequest)
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
		log.WithField("method", r.Method).Warn("Invalid HTTP method for upload directory")
		http.Error(w, "Method Not Allowed", http.StatusMethodNotAllowed)
		return
	}
}

// handleUpload handles PUT requests for file uploads
func handleUpload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string, a url.Values) {
	log.Infof("Using storage path: %s", conf.Server.StoragePath)

	// HMAC validation
	var protocolVersion string
	if a.Get("v2") != "" {
		protocolVersion = "v2"
	} else if a.Get("token") != "" {
		protocolVersion = "token"
	} else if a.Get("v") != "" {
		protocolVersion = "v"
	} else {
		log.Warn("No HMAC attached to URL.")
		http.Error(w, "No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC", http.StatusForbidden)
		return
	}

	mac := hmac.New(sha256.New, []byte(conf.Security.Secret))

	if protocolVersion == "v" {
		mac.Write([]byte(fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)))
	} else {
		contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
		if (contentType == "") {
			contentType = "application/octet-stream"
		}
		mac.Write([]byte(fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10) + "\x00" + contentType))
	}

	calculatedMAC := mac.Sum(nil)

	providedMACHex := a.Get(protocolVersion)
	providedMAC, err := hex.DecodeString(providedMACHex)
	if err != nil {
		log.Warn("Invalid MAC encoding")
		http.Error(w, "Invalid MAC encoding", http.StatusForbidden)
		return
	}

	if !hmac.Equal(calculatedMAC, providedMAC) {
		log.Warn("Invalid MAC")
		http.Error(w, "Invalid MAC", http.StatusForbidden)
		return
	}

	if !isExtensionAllowed(fileStorePath) {
		log.Warn("Invalid file extension")
		http.Error(w, "Invalid file extension", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	minFreeBytes, err := parseSize(conf.Server.MinFreeBytes)
	if err != nil {
		log.Fatalf("Invalid MinFreeBytes: %v", err)
	}
	err = checkStorageSpace(conf.Server.StoragePath, minFreeBytes)
	if err != nil {
		log.Warn("Not enough free space")
		http.Error(w, "Not enough free space", http.StatusInsufficientStorage)
		uploadErrorsTotal.Inc()
		return
	}

	// Create temp file and write the uploaded data
	tempFilename := absFilename + ".tmp"
	err = createFile(tempFilename, r)
	if err != nil {
		log.WithFields(logrus.Fields{
			"filename": absFilename,
		}).WithError(err).Error("Error creating temp file")
		http.Error(w, "Error writing temp file", http.StatusInternalServerError)
		return
	}

	if conf.ClamAV.ClamAVEnabled && shouldScanFile(absFilename) {
		scanErr := scanFileWithClamAV(tempFilename)
		if scanErr != nil {
			os.Remove(tempFilename)
			http.Error(w, "File failed virus scan", http.StatusForbidden)
			return
		}
	}

	// Optional: verify it's a valid image if needed
	if isImageFile(absFilename) {
		// Attempt to open temp file with imaging
		_, imgErr := imaging.Open(tempFilename)
		if imgErr != nil {
			os.Remove(tempFilename)
			http.Error(w, "Corrupted or invalid image", http.StatusBadRequest)
			return
		}
	}

	// Finalize only if all checks passed
	err = os.Rename(tempFilename, absFilename)
	if err != nil {
		os.Remove(tempFilename)
		http.Error(w, "Could not finalize upload", http.StatusInternalServerError)
		return
	}

	w.WriteHeader(http.StatusCreated)
	// Respond with 201 Created immediately
	w.WriteHeader(http.StatusCreated)
	if f, ok := w.(http.Flusher); ok {
		f.Flush()
	}
	log.Infof("Responded with 201 Created for file: %s", absFilename)

	// Asynchronous processing in the background
	go func() {
		var logMessages []string

		// ClamAV scanning
		if conf.ClamAV.ClamAVEnabled && shouldScanFile(absFilename) {
			err := scanFileWithClamAV(absFilename)
			if err != nil {
				logMessages = append(logMessages, fmt.Sprintf("ClamAV failed for %s: %v", absFilename, err))
				for _, msg := range logMessages {
					log.Info(msg)
				}
				return
			} else {
				logMessages = append(logMessages, fmt.Sprintf("ClamAV scan passed for file: %s", absFilename))
			}
		}

		// Deduplication
		if conf.Redis.RedisEnabled && conf.Server.DeduplicationEnabled {
			err := handleDeduplication(context.Background(), absFilename)
			if err != nil {
				log.Errorf("Deduplication failed for %s: %v", absFilename, err)
				os.Remove(absFilename)
				uploadErrorsTotal.Inc()
				return
			} else {
				logMessages = append(logMessages, fmt.Sprintf("Deduplication handled successfully for file: %s", absFilename))
			}
		}

		// Versioning
		if conf.Versioning.EnableVersioning {
			if exists, _ := fileExists(absFilename); exists {
				err := versionFile(absFilename)
				if err != nil {
					log.Errorf("Versioning failed for %s: %v", absFilename, err)
					os.Remove(absFilename)
					uploadErrorsTotal.Inc()
					return
				} else {
					logMessages = append(logMessages, fmt.Sprintf("File versioned successfully: %s", absFilename))
				}
			}
		}

		logMessages = append(logMessages, fmt.Sprintf("Processing completed successfully for %s", absFilename))
		uploadsTotal.Inc()

		// Log all messages at once
		for _, msg := range logMessages {
			log.Info(msg)
		}
	}()
}

func handleDownload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string) {
	log.Debugf("Attempting to download file from path: %s", absFilename)

	fileInfo, err := getFileInfo(absFilename)
	if err != nil {
		log.Errorf("Failed to get file information for %s: %v", absFilename, err)
		// If file doesn't exist, list directory contents for debug
		if os.IsNotExist(err) {
			dir := filepath.Dir(absFilename)
			items, dirErr := os.ReadDir(dir)
			if dirErr == nil {
				for _, it := range items {
					log.Debugf("Dir item: %s", it.Name())
				}
			} else {
				log.Warnf("Could not read directory %s: %v", dir, dirErr)
			}
		}
		http.NotFound(w, r)
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
		log.Infof("Initiating download for file: %s", absFilename)
		http.ServeFile(w, r, absFilename)
		downloadDuration.Observe(time.Since(startTime).Seconds())
		downloadSizeBytes.Observe(float64(fileInfo.Size()))
		downloadsTotal.Inc()
		log.Infof("File downloaded successfully: %s", absFilename)
		return
	}
}

func scanFileWithClamAV(filePath string) error {
	log.WithField("file", filePath).Info("Scanning file with ClamAV")

	scanResultChan, err := clamClient.ScanFile(filePath)
	if err != nil {
		log.WithError(err).Error("Failed to initiate ClamAV scan")
		return fmt.Errorf("failed to initiate ClamAV scan: %w", err)
	}

	scanResult := <-scanResultChan
	if scanResult == nil {
		log.Error("Failed to receive scan result from ClamAV")
		return fmt.Errorf("failed to receive scan result")
	}

	switch scanResult.Status {
	case clamd.RES_OK:
		log.WithField("file", filePath).Info("ClamAV scan passed")
		return nil
	case clamd.RES_FOUND:
		log.WithFields(logrus.Fields{"file": filePath, "description": scanResult.Description}).Warn("ClamAV detected a virus")
		return fmt.Errorf("virus detected: %s", scanResult.Description)
	default:
		log.WithFields(logrus.Fields{"file": filePath, "status": scanResult.Status, "description": scanResult.Description}).Warn("ClamAV scan returned unexpected status")
		return fmt.Errorf("unexpected ClamAV status: %s", scanResult.Description)
	}
}

func initClamAV(socket string) (*clamd.Clamd, error) {
	if socket == "" {
		log.Error("ClamAV socket path not configured.")
		return nil, fmt.Errorf("ClamAV socket path not configured")
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

func handleChunkedUpload(tempFilename string, r *http.Request, chunkSize int) error {
	log.WithField("file", tempFilename).Info("Handling chunked upload to temporary file")

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

	writer := bufio.NewWriterSize(targetFile, chunkSize)
	buffer := make([]byte, chunkSize)

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

	log.WithFields(logrus.Fields{"temp_file": tempFilename, "total_bytes": totalBytes}).Info("Chunked upload completed successfully")
	uploadSizeBytes.Observe(float64(totalBytes))
	return nil
}

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

func monitorNetwork(ctx context.Context) {
	currentIP := getCurrentIPAddress()

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
					log.Warn("Network event channel full. Dropping IP_CHANGE event.")
				}
			}
		}
	}
}

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
			}
		}
	}
}

func getCurrentIPAddress() string {
	interfaces, err := net.Interfaces()
	if err != nil {
		log.WithError(err).Error("Failed to get network interfaces")
		return ""
	}

	for _, iface := range interfaces {
		if iface.Flags&net.FlagUp == 0 || iface.Flags&net.FlagLoopback != 0 {
			continue
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

func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-quit
		log.Infof("Received signal %s. Initiating graceful shutdown...", sig)
		removePIDFile(conf.Server.PIDFilePath) // Ensure PID file is removed
		cancel()
		ctx, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()
		if err := server.Shutdown(ctx); err != nil {
			log.Errorf("Graceful shutdown failed: %v", err)
		} else {
			log.Info("Server gracefully stopped")
		}
	}()
}

func initRedis() {
	if (!conf.Redis.RedisEnabled) {
		log.Info("Redis is disabled in configuration.")
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
		log.Fatalf("Failed to connect to Redis: %v", err)
	}
	log.Info("Connected to Redis successfully")

	mu.Lock()
	redisConnected = true
	mu.Unlock()
}

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
				if (!redisConnected) {
					log.Info("Redis reconnected successfully")
				}
				redisConnected = true
				log.Debug("Redis health check succeeded.")
			}
			mu.Unlock()
		}
	}
}

func parseDuration(durationStr string) time.Duration {
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		log.WithError(err).Warn("Invalid duration format, using default 30s")
		return 30 * time.Second
	}
	return duration
}

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
				if !info.IsDir() {
					// Check if file metadata is cached
					if metadata, found := fileMetadataCache.Get(path); found {
						if fileMetadata, ok := metadata.(FileMetadata); ok {
							if now.Sub(fileMetadata.CreationDate) > ttl {
								err := os.Remove(path)
								if err != nil {
									log.WithError(err).Errorf("Failed to remove expired file: %s", path)
								} else {
									log.Infof("Removed expired file: %s", path)
								}
							}
						}
					} else {
						// If metadata is not cached, use file modification time
						if now.Sub(info.ModTime()) > ttl {
							err := os.Remove(path)
							if err != nil {
								log.WithError(err).Errorf("Failed to remove expired file: %s", path)
							} else {
								log.Infof("Removed expired file: %s", path)
							}
						}
					}
				}
				return nil
			})
			if err != nil {
				log.WithError(err).Error("Error cleaning files")
			}
		}
	}
}

func DeduplicateFiles(storeDir string) error {
	hashMap := make(map[string]string)
	var mu sync.Mutex
	var wg sync.WaitGroup
	fileChan := make(chan string, 100)

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

				err = os.Remove(filePath)
				if err != nil {
					logrus.WithError(err).Errorf("Failed to remove duplicate file %s", filePath)
					continue
				}

				err = os.Link(original, filePath)
				if err != nil {
					logrus.WithError(err).Errorf("Failed to create hard link from %s to %s", original, filePath)
					continue
				}

				logrus.Infof("Removed duplicate %s and linked to %s", filePath, original)
			}
		}()
	}

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
		return fmt.Errorf("error walking path %s: %w", storeDir, err)
	}

	close(fileChan)
	wg.Wait()
	return nil
}

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

func handleMultipartUpload(w http.ResponseWriter, r *http.Request, absFilename string) error {
	err := r.ParseMultipartForm(32 << 20)
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

	if !isExtensionAllowed(handler.Filename) {
		log.WithFields(logrus.Fields{"filename": handler.Filename, "extension": filepath.Ext(handler.Filename)}).Warn("Attempted upload with disallowed file extension")
		http.Error(w, "Disallowed file extension. Allowed: "+strings.Join(conf.Uploads.AllowedExtensions, ", "), http.StatusForbidden)
		uploadErrorsTotal.Inc()
		return fmt.Errorf("disallowed file extension")
	}

	tempFilename := absFilename + ".tmp"
	tempFile, err := os.OpenFile(tempFilename, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		log.WithError(err).Error("Failed to create temporary file")
		http.Error(w, "Failed to create temporary file", http.StatusInternalServerError)
		return err
	}
	defer tempFile.Close()

	_, err = io.Copy(tempFile, file)
	if err != nil {
		log.WithError(err).Error("Failed to copy uploaded file to temporary file")
		http.Error(w, "Failed to copy uploaded file", http.StatusInternalServerError)
		return err
	}

	if clamClient != nil {
		err := scanFileWithClamAV(tempFilename)
		if err != nil {
			log.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Warn("ClamAV detected a virus")
			os.Remove(tempFilename)
			uploadErrorsTotal.Inc()
			return err
		}
	}

	if conf.Versioning.EnableVersioning {
		existing, _ := fileExists(absFilename)
		if existing {
			err := versionFile(absFilename)
			if err != nil {
				log.WithFields(logrus.Fields{"file": absFilename, "error": err}).Error("Error versioning file")
				os.Remove(tempFilename)
				return err
			}
		}
	}

	err = os.Rename(tempFilename, absFilename)
	if err != nil {
		os.Remove(tempFilename)
		return fmt.Errorf("failed to move file to final destination: %w", err)
	}

	log.WithField("file", absFilename).Info("File uploaded and scanned successfully")
	uploadsTotal.Inc()
	return nil
}

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

func handleDeduplication(ctx context.Context, absFilename string) error {
	log.Debugf("Starting deduplication for: %s", absFilename)
	checksum, err := computeSHA256(ctx, absFilename)
	if err != nil {
		deduplicationErrorsTotal.Inc()
		log.Errorf("Failed to compute checksum for %s: %v", absFilename, err)
		return err
	}
	log.Debugf("Computed checksum for %s: %s", absFilename, checksum)

	// Use directory names instead of hash values for deduplication
	dedupDir := conf.Deduplication.Directory
	if dedupDir == "" {
		return fmt.Errorf("deduplication directory is not configured")
	}

	// Create a directory for the checksum if it doesn't exist
	dedupPath := filepath.Join(dedupDir, checksum)
	if err := os.MkdirAll(dedupPath, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create deduplication directory: %w", err)
	}

	// Check if a file with the same name already exists in the deduplication directory
	existingPath := filepath.Join(dedupPath, filepath.Base(absFilename))
	if _, err := os.Stat(existingPath); err == nil {
		log.Infof("Found existing file for checksum %s at %s", checksum, existingPath)
		err = os.Link(existingPath, absFilename)
		if err != nil {
			log.Errorf("Failed to create hard link: %v", err)
			return err
		}
		log.Infof("Created hard link from %s to %s", absFilename, existingPath)
		filesDeduplicatedTotal.Inc()
		return nil
	} else if !os.IsNotExist(err) {
		return fmt.Errorf("error checking existing file: %w", err)
	}

	// Move the file to the deduplication directory
	err = os.Rename(absFilename, existingPath)
	if err != nil {
		deduplicationErrorsTotal.Inc()
		return fmt.Errorf("failed to move file to deduplication directory: %w", err)
	}
	log.Infof("Moved file to deduplication directory: %s", existingPath)

	// Create a hard link from the deduplication directory to the original location
	err = os.Link(existingPath, absFilename)
	if err != nil {
		log.Errorf("Failed to create hard link: %v", err)
		return err
	}
	log.Infof("Created hard link from %s to %s", existingPath, absFilename)

	filesDeduplicatedTotal.Inc()
	return nil
}

func computeSHA256(ctx context.Context, filePath string) (string, error) {
	if filePath == "" {
		return "", fmt.Errorf("computeSHA256: filePath cannot be empty")
	}

	file, err := os.Open(filePath)
	if err != nil {
		log.Errorf("Failed to open file for checksum: %v", err)
		return "", fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	hasher := sha256.New()
	reader := bufio.NewReader(file)

	buffer := make([]byte, 4096)
	for {
		select {
		case <-ctx.Done():
			return "", fmt.Errorf("operation cancelled")
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
					log.Debugf("Checksum computed: %s", sum)
					return sum, nil
				}
				return "", fmt.Errorf("read error: %w", err)
			}
		}
	}
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

func CreateISOContainer(files []string, isoPath string, size string, charset string) error {
	args := []string{"-o", isoPath, "-V", "ISO_CONTAINER", "-J", "-R", "-input-charset", charset}
	args = append(args, files...)
	cmd := exec.Command("genisoimage", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

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
			log.Infof("ISO container mounted at %s", mountPoint)
			return nil
		}
		log.Warnf("Mount attempt %d failed: %v, output: %s", i+1, err, string(output))
		time.Sleep(2 * time.Second)
	}
	return fmt.Errorf("failed to mount ISO: %w, output: %s", err, string(output))
}

func UnmountISOContainer(mountPoint string) error {
	cmd := exec.Command("umount", mountPoint)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func handleISOContainer(absFilename string) error {
	isoPath := filepath.Join(conf.ISO.MountPoint, "container.iso")

	err := CreateISOContainer([]string{absFilename}, isoPath, conf.ISO.Size, conf.ISO.Charset)
	if err != nil {
		isoCreationErrorsTotal.Inc()
		return fmt.Errorf("failed to create ISO container: %w", err)
	}

	if err := os.MkdirAll(conf.ISO.MountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	err = MountISOContainer(isoPath, conf.ISO.MountPoint)
	if err != nil {
		isoMountErrorsTotal.Inc()
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}

	err = UnmountISOContainer(conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to unmount ISO: %w", err)
	}

	return nil
}

func precacheStoragePath(dir string) error {
	return filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Warnf("Error accessing path %s: %v", path, err)
			return nil // Continue walking
		}
		if !info.IsDir() {
			fileInfoCache.Set(path, info, cache.DefaultExpiration)
			fileMetadataCache.Set(path, FileMetadata{CreationDate: info.ModTime()}, cache.DefaultExpiration)
			log.Debugf("Cached file info and metadata for %s", path)
		}
		return nil
	})
}

// generateThumbnail exclusively uses imaging and logs additional details
func generateThumbnail(originalPath, thumbnailDir, size string) error {
	log.Infof("Starting thumbnail generation for: %s; target directory: %s; requested size: %s",
		originalPath, thumbnailDir, size)

	// Parse the size (e.g., "200x200")
	dimensions := strings.Split(size, "x")
	if len(dimensions) != 2 {
		return fmt.Errorf("invalid size format: %s", size)
	}
	width, err := strconv.Atoi(dimensions[0])
	if err != nil {
		return fmt.Errorf("invalid width: %v", err)
	}
	height, err := strconv.Atoi(dimensions[1])
	if err != nil {
		return fmt.Errorf("invalid height: %v", err)
	}

	thumbnailPath := filepath.Join(thumbnailDir, filepath.Base(originalPath))
	log.Infof("Thumbnail will be saved at: %s", thumbnailPath)

	err = generateThumbnailWithImaging(originalPath, thumbnailPath, width, height)
	if err != nil {
		log.Errorf("Thumbnail generation failed for %s -> %s: %v", originalPath, thumbnailPath, err)
		return err
	}
	log.Infof("Thumbnail generation completed successfully for: %s -> %s", originalPath, thumbnailPath)
	return nil
}

// enhance logging in generateThumbnailWithImaging
func generateThumbnailWithImaging(originalPath, thumbnailPath string, width, height int) error {
	log.Infof("Using imaging to generate thumbnail: original=%s, thumbnail=%s, size=%dx%d",
		originalPath, thumbnailPath, width, height)
	// Open the original image
	img, err := imaging.Open(originalPath)
	if err != nil {
		return fmt.Errorf("failed to open image: %v", err)
	}

	// Resize the image using Lanczos filter
	thumb := imaging.Resize(img, width, height, imaging.Lanczos)

	// Save the thumbnail
	err = imaging.Save(thumb, thumbnailPath)
	if err != nil {
		return fmt.Errorf("failed to save thumbnail: %v", err)
	}
	log.Infof("Thumbnail saved successfully at: %s", thumbnailPath)
	return nil
}

func handleFileCleanup(conf *Config) {
	if conf.Server.FileTTLEnabled {
		ttlDuration, err := parseTTL(conf.Server.FileTTL)
		if err != nil {
			log.Fatalf("Invalid TTL configuration: %v", err)
		}
		log.Printf("File TTL is enabled. Files older than %v will be deleted.", ttlDuration)

		ticker := time.NewTicker(24 * time.Hour)
		defer ticker.Stop()

		for range ticker.C {
			deleteOldFiles(conf, ttlDuration)
		}
	} else {
		log.Println("File TTL is disabled. No files will be automatically deleted.")
	}
}

func deleteOldFiles(conf *Config, ttl time.Duration) {
	err := filepath.Walk(conf.Server.StoragePath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() {
			// Check if file metadata is cached
			if metadata, found := fileMetadataCache.Get(path); found {
				if fileMetadata, ok := metadata.(FileMetadata); ok {
					if time.Since(fileMetadata.CreationDate) > ttl {
						err := os.Remove(path)
						if err != nil {
							log.Printf("Failed to delete %s: %v", path, err)
						} else {
							log.Printf("Deleted old file: %s", path)
						}
					}
				}
			} else {
				// If metadata is not cached, use file modification time
				if time.Since(info.ModTime()) > ttl {
					err := os.Remove(path)
					if err != nil {
						log.Printf("Failed to delete %s: %v", path, err)
					} else {
						log.Printf("Deleted old file: %s", path)
					}
				}
			}
		}
		return nil
	})
	if err != nil {
		log.Printf("Error during file cleanup: %v", err)
	}
}

func scheduleThumbnailGeneration(ctx context.Context) {
	if (!conf.Thumbnails.Enabled) {
		log.Info("Thumbnail generation is disabled.")
		return
	}

	var scanDir string
	if conf.Deduplication.Enabled {
		scanDir = conf.Deduplication.Directory
	} else {
		scanDir = conf.Server.StoragePath
	}

	tickerInterval, err := time.ParseDuration(conf.Thumbnails.ThumbnailIntervalScan)
	if err != nil {
		log.Fatalf("Invalid thumbnail interval scan duration: %v", err)
	}
	ticker := time.NewTicker(tickerInterval)

	go func() {
		for {
			select {
			case <-ticker.C:
				log.Info("Starting scheduled thumbnail generation...")
				err := filepath.Walk(scanDir, func(path string, info os.FileInfo, err error) error {
					if err != nil {
						return err
					}
					if !info.IsDir() && isImageFile(path) {
						generateThumbnail(path, conf.Thumbnails.Directory, conf.Thumbnails.Size)
					}
					return nil
				})
				if err != nil {
					log.Errorf("Error walking through directory %s: %v", scanDir, err)
				}
				log.Info("Scheduled thumbnail generation completed.")
			case <-ctx.Done():
				ticker.Stop()
				return
			}
		}
	}()
}

func isImageFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	switch ext {
	case ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp":
		return true
	default:
		return false
	}
}