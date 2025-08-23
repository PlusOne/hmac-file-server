// main.go

package main

import (
	"bufio"
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"mime"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/dutchcoders/go-clamd" // ClamAV integration
	"github.com/go-redis/redis/v8"    // Redis integration
	jwt "github.com/golang-jwt/jwt/v5"
	"github.com/patrickmn/go-cache"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/mem"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
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
		return 0, fmt.Errorf("invalid size value: %v", err)
	}

	switch unit {
	case "KB":
		return int64(value) * 1024, nil
	case "MB":
		return int64(value) * 1024 * 1024, nil
	case "GB":
		return int64(value) * 1024 * 1024 * 1024, nil
	default:
		return 0, fmt.Errorf("unknown size unit: %s", unit)
	}
}

// parseTTL converts a human-readable TTL string to a time.Duration
func parseTTL(ttlStr string) (time.Duration, error) {
	ttlStr = strings.ToLower(strings.TrimSpace(ttlStr))
	if ttlStr == "" {
		return 0, fmt.Errorf("TTL string cannot be empty")
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
	case 's':
		return time.Duration(val) * time.Second, nil
	case 'm':
		return time.Duration(val) * time.Minute, nil
	case 'h':
		return time.Duration(val) * time.Hour, nil
	case 'd':
		return time.Duration(val) * 24 * time.Hour, nil
	case 'w':
		return time.Duration(val) * 7 * 24 * time.Hour, nil
	case 'y':
		return time.Duration(val) * 365 * 24 * time.Hour, nil
	default:
		return 0, fmt.Errorf("unknown TTL unit: %c", unit)
	}
}

// Configuration structures
type ServerConfig struct {
	ListenAddress         string   `toml:"listen_address" mapstructure:"listen_address"`
	StoragePath           string   `toml:"storage_path" mapstructure:"storage_path"`
	MetricsEnabled        bool     `toml:"metricsenabled" mapstructure:"metricsenabled"` // Fixed to match config
	MetricsPath           string   `toml:"metrics_path" mapstructure:"metrics_path"`
	PidFile               string   `toml:"pid_file" mapstructure:"pid_file"`
	MaxUploadSize         string   `toml:"max_upload_size" mapstructure:"max_upload_size"`
	MaxHeaderBytes        int      `toml:"max_header_bytes" mapstructure:"max_header_bytes"`
	CleanupInterval       string   `toml:"cleanup_interval" mapstructure:"cleanup_interval"`
	MaxFileAge            string   `toml:"max_file_age" mapstructure:"max_file_age"`
	PreCache              bool     `toml:"pre_cache" mapstructure:"pre_cache"`
	PreCacheWorkers       int      `toml:"pre_cache_workers" mapstructure:"pre_cache_workers"`
	PreCacheInterval      string   `toml:"pre_cache_interval" mapstructure:"pre_cache_interval"`
	GlobalExtensions      []string `toml:"global_extensions" mapstructure:"global_extensions"`
	DeduplicationEnabled  bool     `toml:"deduplication_enabled" mapstructure:"deduplication_enabled"`
	MinFreeBytes          string   `toml:"min_free_bytes" mapstructure:"min_free_bytes"`
	FileNaming            string   `toml:"file_naming" mapstructure:"file_naming"`
	ForceProtocol         string   `toml:"force_protocol" mapstructure:"force_protocol"`
	EnableDynamicWorkers  bool     `toml:"enable_dynamic_workers" mapstructure:"enable_dynamic_workers"`
	WorkerScaleUpThresh   int      `toml:"worker_scale_up_thresh" mapstructure:"worker_scale_up_thresh"`
	WorkerScaleDownThresh int      `toml:"worker_scale_down_thresh" mapstructure:"worker_scale_down_thresh"`
	UnixSocket            bool     `toml:"unixsocket" mapstructure:"unixsocket"`               // Added missing field from example/logs
	MetricsPort           string   `toml:"metricsport" mapstructure:"metricsport"`             // Fixed to match config
	FileTTL               string   `toml:"filettl" mapstructure:"filettl"`                     // Fixed to match config
	FileTTLEnabled        bool     `toml:"filettlenabled" mapstructure:"filettlenabled"`       // Fixed to match config
	AutoAdjustWorkers     bool     `toml:"autoadjustworkers" mapstructure:"autoadjustworkers"` // Fixed to match config
	NetworkEvents         bool     `toml:"networkevents" mapstructure:"networkevents"`         // Fixed to match config
	PIDFilePath           string   `toml:"pidfilepath" mapstructure:"pidfilepath"`             // Fixed to match config
	CleanUponExit         bool     `toml:"clean_upon_exit" mapstructure:"clean_upon_exit"`     // Added missing field
	PreCaching            bool     `toml:"precaching" mapstructure:"precaching"`               // Fixed to match config
	BindIP                string   `toml:"bind_ip" mapstructure:"bind_ip"`                     // Added missing field
}

type UploadsConfig struct {
	AllowedExtensions       []string `toml:"allowed_extensions" mapstructure:"allowed_extensions"`
	ChunkedUploadsEnabled   bool     `toml:"chunked_uploads_enabled" mapstructure:"chunked_uploads_enabled"`
	ChunkSize               string   `toml:"chunk_size" mapstructure:"chunk_size"`
	ResumableUploadsEnabled bool     `toml:"resumable_uploads_enabled" mapstructure:"resumable_uploads_enabled"`
	SessionTimeout          string   `toml:"sessiontimeout" mapstructure:"sessiontimeout"`
	MaxRetries              int      `toml:"maxretries" mapstructure:"maxretries"`
}

type DownloadsConfig struct {
	AllowedExtensions         []string `toml:"allowed_extensions" mapstructure:"allowed_extensions"`
	ChunkedDownloadsEnabled   bool     `toml:"chunked_downloads_enabled" mapstructure:"chunked_downloads_enabled"`
	ChunkSize                 string   `toml:"chunk_size" mapstructure:"chunk_size"`
	ResumableDownloadsEnabled bool     `toml:"resumable_downloads_enabled" mapstructure:"resumable_downloads_enabled"`
}

type SecurityConfig struct {
	Secret        string `toml:"secret" mapstructure:"secret"`
	EnableJWT     bool   `toml:"enablejwt" mapstructure:"enablejwt"` // Added EnableJWT field
	JWTSecret     string `toml:"jwtsecret" mapstructure:"jwtsecret"`
	JWTAlgorithm  string `toml:"jwtalgorithm" mapstructure:"jwtalgorithm"`
	JWTExpiration string `toml:"jwtexpiration" mapstructure:"jwtexpiration"`
}

type LoggingConfig struct {
	Level      string `mapstructure:"level"`
	File       string `mapstructure:"file"`
	MaxSize    int    `mapstructure:"max_size"`
	MaxBackups int    `mapstructure:"max_backups"`
	MaxAge     int    `mapstructure:"max_age"`
	Compress   bool   `mapstructure:"compress"`
}

type DeduplicationConfig struct {
	Enabled   bool   `mapstructure:"enabled"`
	Directory string `mapstructure:"directory"`
	MaxSize   string `mapstructure:"maxsize"`
}

type ISOConfig struct {
	Enabled       bool   `mapstructure:"enabled"`
	MountPoint    string `mapstructure:"mountpoint"`
	Size          string `mapstructure:"size"`
	Charset       string `mapstructure:"charset"`
	ContainerFile string `mapstructure:"containerfile"` // Added missing field
}

type TimeoutConfig struct {
	Read     string `mapstructure:"readtimeout" toml:"readtimeout"`
	Write    string `mapstructure:"writetimeout" toml:"writetimeout"`
	Idle     string `mapstructure:"idletimeout" toml:"idletimeout"`
	Shutdown string `mapstructure:"shutdown" toml:"shutdown"`
}

type VersioningConfig struct {
	Enabled bool   `mapstructure:"enableversioning" toml:"enableversioning"` // Corrected to match example config
	Backend string `mapstructure:"backend" toml:"backend"`
	MaxRevs int    `mapstructure:"maxversions" toml:"maxversions"` // Corrected to match example config
}

type ClamAVConfig struct {
	ClamAVEnabled      bool     `mapstructure:"clamavenabled"`
	ClamAVSocket       string   `mapstructure:"clamavsocket"`
	NumScanWorkers     int      `mapstructure:"numscanworkers"`
	ScanFileExtensions []string `mapstructure:"scanfileextensions"`
	MaxScanSize        string   `mapstructure:"maxscansize"`
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
}

type BuildConfig struct {
	Version string `mapstructure:"version"` // Updated version
}

type NetworkResilienceConfig struct {
	FastDetection               bool     `toml:"fast_detection" mapstructure:"fast_detection"`
	QualityMonitoring          bool     `toml:"quality_monitoring" mapstructure:"quality_monitoring"`
	PredictiveSwitching        bool     `toml:"predictive_switching" mapstructure:"predictive_switching"`
	MobileOptimizations        bool     `toml:"mobile_optimizations" mapstructure:"mobile_optimizations"`
	DetectionInterval          string   `toml:"detection_interval" mapstructure:"detection_interval"`
	QualityCheckInterval       string   `toml:"quality_check_interval" mapstructure:"quality_check_interval"`
	MaxDetectionInterval       string   `toml:"max_detection_interval" mapstructure:"max_detection_interval"`
	
	// Multi-interface support
	MultiInterfaceEnabled      bool     `toml:"multi_interface_enabled" mapstructure:"multi_interface_enabled"`
	InterfacePriority          []string `toml:"interface_priority" mapstructure:"interface_priority"`
	AutoSwitchEnabled          bool     `toml:"auto_switch_enabled" mapstructure:"auto_switch_enabled"`
	SwitchThresholdLatency     string   `toml:"switch_threshold_latency" mapstructure:"switch_threshold_latency"`
	SwitchThresholdPacketLoss  float64  `toml:"switch_threshold_packet_loss" mapstructure:"switch_threshold_packet_loss"`
	QualityDegradationThreshold float64  `toml:"quality_degradation_threshold" mapstructure:"quality_degradation_threshold"`
	MaxSwitchAttempts          int      `toml:"max_switch_attempts" mapstructure:"max_switch_attempts"`
	SwitchDetectionInterval    string   `toml:"switch_detection_interval" mapstructure:"switch_detection_interval"`
}

// ClientNetworkConfigTOML is used for loading from TOML where timeout is a string
type ClientNetworkConfigTOML struct {
	SessionBasedTracking      bool   `toml:"session_based_tracking" mapstructure:"session_based_tracking"`
	AllowIPChanges           bool   `toml:"allow_ip_changes" mapstructure:"allow_ip_changes"`
	SessionMigrationTimeout  string `toml:"session_migration_timeout" mapstructure:"session_migration_timeout"`
	MaxIPChangesPerSession   int    `toml:"max_ip_changes_per_session" mapstructure:"max_ip_changes_per_session"`
	ClientConnectionDetection bool  `toml:"client_connection_detection" mapstructure:"client_connection_detection"`
	AdaptToClientNetwork     bool   `toml:"adapt_to_client_network" mapstructure:"adapt_to_client_network"`
}

// This is the main Config struct to be used
type Config struct {
	Server            ServerConfig             `mapstructure:"server"`
	Logging           LoggingConfig            `mapstructure:"logging"`
	Deduplication     DeduplicationConfig      `mapstructure:"deduplication"` // Added
	ISO               ISOConfig                `mapstructure:"iso"`           // Added
	Timeouts          TimeoutConfig            `mapstructure:"timeouts"`      // Added
	Security          SecurityConfig           `mapstructure:"security"`
	Versioning        VersioningConfig         `mapstructure:"versioning"` // Added
	Uploads           UploadsConfig            `mapstructure:"uploads"`
	Downloads         DownloadsConfig          `mapstructure:"downloads"`
	ClamAV            ClamAVConfig             `mapstructure:"clamav"`
	Redis             RedisConfig              `mapstructure:"redis"`
	Workers           WorkersConfig            `mapstructure:"workers"`
	File              FileConfig               `mapstructure:"file"`
	Build             BuildConfig              `mapstructure:"build"`
	NetworkResilience NetworkResilienceConfig      `mapstructure:"network_resilience"`
	ClientNetwork     ClientNetworkConfigTOML     `mapstructure:"client_network_support"`
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
	CreationDate time.Time
}

// processScan processes a scan task
func processScan(task ScanTask) error {
	// Check if ClamAV is enabled before processing
	confMutex.RLock()
	clamEnabled := conf.ClamAV.ClamAVEnabled
	confMutex.RUnlock()
	
	if !clamEnabled {
		log.Infof("ClamAV disabled, skipping scan for file: %s", task.AbsFilename)
		return nil
	}
	
	log.Infof("Started processing scan for file: %s", task.AbsFilename)
	semaphore <- struct{}{}
	defer func() { <-semaphore }()

	err := scanFileWithClamAV(task.AbsFilename)
	if err != nil {
		log.WithFields(logrus.Fields{"file": task.AbsFilename, "error": err}).Error("Failed to scan file")
		return err
	}

	log.Infof("Finished processing scan for file: %s", task.AbsFilename)
	return nil
}

var (
	conf              Config
	versionString     string
	log               = logrus.New()
	fileInfoCache     *cache.Cache
	fileMetadataCache *cache.Cache
	clamClient        *clamd.Clamd
	redisClient       *redis.Client
	redisConnected    bool
	confMutex         sync.RWMutex // Protects the global 'conf' variable and related critical sections.
	// Use RLock() for reading, Lock() for writing.

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

	filesDeduplicatedTotal    prometheus.Counter
	deduplicationErrorsTotal  prometheus.Counter
	isoContainersCreatedTotal prometheus.Counter
	isoCreationErrorsTotal    prometheus.Counter
	isoContainersMountedTotal prometheus.Counter
	isoMountErrorsTotal       prometheus.Counter

	workerPool    *WorkerPool
	networkEvents chan NetworkEvent

	workerAdjustmentsTotal   prometheus.Counter
	workerReAdjustmentsTotal prometheus.Counter
)

var bufferPool = sync.Pool{
	New: func() interface{} {
		buf := make([]byte, 32*1024)
		return &buf
	},
}

const maxConcurrentOperations = 10

var semaphore = make(chan struct{}, maxConcurrentOperations)

// Global client connection tracker for multi-interface support
var clientTracker *ClientConnectionTracker

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
		log.Errorf("Failed to write PID file: %v", err) // Improved error logging
		return err
	}
	log.Infof("PID %d written to %s", pid, pidPath)
	return nil
}

// removePIDFile removes the PID file
func removePIDFile(pidPath string) {
	err := os.Remove(pidPath)
	if err != nil {
		log.Errorf("Failed to remove PID file: %v", err) // Improved error logging
	} else {
		log.Infof("PID file %s removed successfully", pidPath)
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

func initializeNetworkProtocol(forceProtocol string) (*net.Dialer, error) {
	// Handle empty/default value
	if forceProtocol == "" {
		forceProtocol = "auto"
	}
	
	switch forceProtocol {
	case "ipv4":
		return &net.Dialer{
			Timeout:   5 * time.Second,
			DualStack: false,
			Control: func(network, address string, c syscall.RawConn) error {
				if network == "tcp6" {
					return fmt.Errorf("IPv6 is disabled by forceprotocol setting")
				}
				return nil
			},
		}, nil
	case "ipv6":
		return &net.Dialer{
			Timeout:   5 * time.Second,
			DualStack: false,
			Control: func(network, address string, c syscall.RawConn) error {
				if network == "tcp4" {
					return fmt.Errorf("IPv4 is disabled by forceprotocol setting")
				}
				return nil
			},
		}, nil
	case "auto":
		return &net.Dialer{
			Timeout:   5 * time.Second,
			DualStack: true,
		}, nil
	default:
		return nil, fmt.Errorf("invalid forceprotocol value: %s", forceProtocol)
	}
}

var dualStackClient *http.Client

func main() {
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	var genConfig bool
	var genConfigAdvanced bool
	var genConfigPath string
	var validateOnly bool
	var runConfigTests bool
	var validateQuiet bool
	var validateVerbose bool
	var validateFixable bool
	var validateSecurity bool
	var validatePerformance bool
	var validateConnectivity bool
	var listValidationChecks bool
	var showVersion bool

	flag.BoolVar(&genConfig, "genconfig", false, "Print minimal configuration example and exit.")
	flag.BoolVar(&genConfigAdvanced, "genconfig-advanced", false, "Print advanced configuration template and exit.")
	flag.StringVar(&genConfigPath, "genconfig-path", "", "Write configuration to the given file and exit.")
	flag.BoolVar(&validateOnly, "validate-config", false, "Validate configuration and exit without starting server.")
	flag.BoolVar(&runConfigTests, "test-config", false, "Run configuration validation test scenarios and exit.")
	flag.BoolVar(&validateQuiet, "validate-quiet", false, "Only show errors during validation (suppress warnings and info).")
	flag.BoolVar(&validateVerbose, "validate-verbose", false, "Show detailed validation information including system checks.")
	flag.BoolVar(&validateFixable, "check-fixable", false, "Only show validation issues that can be automatically fixed.")
	flag.BoolVar(&validateSecurity, "check-security", false, "Run only security-related validation checks.")
	flag.BoolVar(&validatePerformance, "check-performance", false, "Run only performance-related validation checks.")
	flag.BoolVar(&validateConnectivity, "check-connectivity", false, "Run only network connectivity validation checks.")
	flag.BoolVar(&listValidationChecks, "list-checks", false, "List all available validation checks and exit.")
	flag.BoolVar(&showVersion, "version", false, "Show version information and exit.")
	flag.Parse()

	if showVersion {
		fmt.Printf("HMAC File Server v3.2\n")
		os.Exit(0)
	}

	if listValidationChecks {
		printValidationChecks()
		os.Exit(0)
	}

	if genConfig {
		fmt.Println("# Option 1: Minimal Configuration (recommended for most users)")
		fmt.Println(GenerateMinimalConfig())
		fmt.Println("\n# Option 2: Advanced Configuration Template (for fine-tuning)")
		fmt.Println("# Use -genconfig-advanced to generate the advanced template")
		os.Exit(0)
	}
	if genConfigAdvanced {
		fmt.Println(GenerateAdvancedConfigTemplate())
		os.Exit(0)
	}
	if genConfigPath != "" {
		var content string
		if genConfigAdvanced {
			content = GenerateAdvancedConfigTemplate()
		} else {
			content = GenerateMinimalConfig()
		}
		
		f, err := os.Create(genConfigPath)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to create file: %v\n", err)
			os.Exit(1)
		}
		defer f.Close()
		w := bufio.NewWriter(f)
		fmt.Fprint(w, content)
		w.Flush()
		fmt.Printf("Configuration written to %s\n", genConfigPath)
		os.Exit(0)
	}
	if runConfigTests {
		RunConfigTests()
		os.Exit(0)
	}

	// Load configuration using simplified approach
	loadedConfig, err := LoadSimplifiedConfig(configFile)
	if err != nil {
		// If no config file exists, offer to create a minimal one
		if configFile == "./config.toml" || configFile == "" {
			fmt.Println("No configuration file found. Creating a minimal config.toml...")
			if err := createMinimalConfig(); err != nil {
				log.Fatalf("Failed to create minimal config: %v", err)
			}
			fmt.Println("Minimal config.toml created. Please review and modify as needed, then restart the server.")
			os.Exit(0)
		}
		log.Fatalf("Failed to load configuration: %v", err)
	}
	conf = *loadedConfig
	configFileGlobal = configFile  // Store for validation helper functions
	log.Info("Configuration loaded successfully.")

	err = validateConfig(&conf)
	if err != nil {
		log.Fatalf("Configuration validation failed: %v", err)
	}
	log.Info("Configuration validated successfully.")

	// Perform comprehensive configuration validation
	validationResult := ValidateConfigComprehensive(&conf)

	// Initialize client connection tracker for multi-interface support
	clientNetworkConfig := &ClientNetworkConfig{
		SessionBasedTracking:     conf.ClientNetwork.SessionBasedTracking,
		AllowIPChanges:          conf.ClientNetwork.AllowIPChanges,
		MaxIPChangesPerSession:  conf.ClientNetwork.MaxIPChangesPerSession,
		AdaptToClientNetwork:    conf.ClientNetwork.AdaptToClientNetwork,
	}
	
	// Parse session migration timeout
	if conf.ClientNetwork.SessionMigrationTimeout != "" {
		if timeout, err := time.ParseDuration(conf.ClientNetwork.SessionMigrationTimeout); err == nil {
			clientNetworkConfig.SessionMigrationTimeout = timeout
		} else {
			clientNetworkConfig.SessionMigrationTimeout = 5 * time.Minute // default
		}
	} else {
		clientNetworkConfig.SessionMigrationTimeout = 5 * time.Minute // default
	}
	
	// Set defaults if not configured
	if clientNetworkConfig.MaxIPChangesPerSession == 0 {
		clientNetworkConfig.MaxIPChangesPerSession = 10
	}
	
	// Initialize the client tracker
	clientTracker = NewClientConnectionTracker(clientNetworkConfig)
	if clientTracker != nil {
		clientTracker.StartCleanupRoutine()
		log.Info("Client multi-interface support initialized")
	}
	PrintValidationResults(validationResult)

	if validationResult.HasErrors() {
		log.Fatal("Cannot start server due to configuration errors. Please fix the above issues and try again.")
	}

	// Handle specialized validation flags
	if validateSecurity || validatePerformance || validateConnectivity || validateQuiet || validateVerbose || validateFixable {
		runSpecializedValidation(&conf, validateSecurity, validatePerformance, validateConnectivity, validateQuiet, validateVerbose, validateFixable)
		os.Exit(0)
	}

	// If only validation was requested, exit now
	if validateOnly {
		if validationResult.HasErrors() {
			log.Error("Configuration validation failed with errors. Review the errors above.")
			os.Exit(1)
		} else if validationResult.HasWarnings() {
			log.Info("Configuration is valid but has warnings. Review the warnings above.")
			os.Exit(0)
		} else {
			log.Info("Configuration validation completed successfully!")
			os.Exit(0)
		}
	}

	// Set log level based on configuration
	level, err := logrus.ParseLevel(conf.Logging.Level)
	if err != nil {
		log.Warnf("Invalid log level '%s', defaulting to 'info'", conf.Logging.Level)
		level = logrus.InfoLevel
	}
	log.SetLevel(level)
	log.Infof("Log level set to: %s", level.String())

	// Log configuration settings using [logging] section
	log.Infof("Server ListenAddress: %s", conf.Server.ListenAddress) // Corrected field name
	log.Infof("Server UnixSocket: %v", conf.Server.UnixSocket)
	log.Infof("Server StoragePath: %s", conf.Server.StoragePath)
	log.Infof("Logging Level: %s", conf.Logging.Level)
	log.Infof("Logging File: %s", conf.Logging.File)
	log.Infof("Server MetricsEnabled: %v", conf.Server.MetricsEnabled)
	log.Infof("Server MetricsPort: %s", conf.Server.MetricsPort) // Corrected field name
	log.Infof("Server FileTTL: %s", conf.Server.FileTTL)         // Corrected field name
	log.Infof("Server MinFreeBytes: %s", conf.Server.MinFreeBytes)
	log.Infof("Server AutoAdjustWorkers: %v", conf.Server.AutoAdjustWorkers) // Corrected field name
	log.Infof("Server NetworkEvents: %v", conf.Server.NetworkEvents)         // Corrected field name
	log.Infof("Server PIDFilePath: %s", conf.Server.PIDFilePath)             // Corrected field name
	log.Infof("Server CleanUponExit: %v", conf.Server.CleanUponExit)         // Corrected field name
	log.Infof("Server PreCaching: %v", conf.Server.PreCaching)               // Corrected field name
	log.Infof("Server FileTTLEnabled: %v", conf.Server.FileTTLEnabled)       // Corrected field name
	log.Infof("Server DeduplicationEnabled: %v", conf.Server.DeduplicationEnabled)
	log.Infof("Server BindIP: %s", conf.Server.BindIP) // Corrected field name
	log.Infof("Server FileNaming: %s", conf.Server.FileNaming)
	log.Infof("Server ForceProtocol: %s", conf.Server.ForceProtocol)

	err = writePIDFile(conf.Server.PIDFilePath) // Corrected field name
	if err != nil {
		log.Fatalf("Error writing PID file: %v", err)
	}
	log.Debug("DEBUG: PID file written successfully")

	log.Debugf("DEBUG: Config logging file: %s", conf.Logging.File)

	setupLogging()
	log.Debug("DEBUG: Logging setup completed")

	logSystemInfo()
	log.Debug("DEBUG: System info logged")

	// Initialize metrics before using any Prometheus counters
	initMetrics()
	log.Debug("DEBUG: Metrics initialized")

	initializeWorkerSettings(&conf.Server, &conf.Workers, &conf.ClamAV)
	log.Debug("DEBUG: Worker settings initialized")

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

	if conf.Server.PreCaching { // Corrected field name
		go func() {
			log.Info("Starting pre-caching of storage path...")
			// Use helper function
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

	// Use helper function
	err = checkFreeSpaceWithRetry(storagePath, 3, 5*time.Second)
	if err != nil {
		log.Fatalf("Insufficient free space: %v", err)
	}

	initializeWorkerSettings(&conf.Server, &conf.Workers, &conf.ClamAV)
	log.Info("Prometheus metrics initialized.")

	networkEvents = make(chan NetworkEvent, 100)
	log.Info("Network event channel initialized.")

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if conf.Server.NetworkEvents { // Corrected field name
		go monitorNetwork(ctx)      // Assuming monitorNetwork is defined in helpers.go or elsewhere
		go handleNetworkEvents(ctx) // Assuming handleNetworkEvents is defined in helpers.go or elsewhere
	}
	go updateSystemMetrics(ctx)

	if conf.ClamAV.ClamAVEnabled {
		var clamErr error
		clamClient, clamErr = initClamAV(conf.ClamAV.ClamAVSocket) // Assuming initClamAV is defined in helpers.go or elsewhere
		if clamErr != nil {
			log.WithError(clamErr).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
		} else {
			log.Info("ClamAV client initialized successfully.")
		}
	}

	if conf.Redis.RedisEnabled {
		initRedis() // Assuming initRedis is defined in helpers.go or elsewhere
	}

	router := setupRouter() // Assuming setupRouter is defined (likely in this file or router.go

	// Initialize enhancements and enhance the router
	InitializeEnhancements(router)

	go handleFileCleanup(&conf) // Directly call handleFileCleanup

	readTimeout, err := time.ParseDuration(conf.Timeouts.Read) // Corrected field name
	if err != nil {
		log.Fatalf("Invalid ReadTimeout: %v", err)
	}

	writeTimeout, err := time.ParseDuration(conf.Timeouts.Write) // Corrected field name
	if err != nil {
		log.Fatalf("Invalid WriteTimeout: %v", err)
	}

	idleTimeout, err := time.ParseDuration(conf.Timeouts.Idle) // Corrected field name
	if err != nil {
		log.Fatalf("Invalid IdleTimeout: %v", err)
	}

	// Initialize network protocol based on forceprotocol setting
	dialer, err := initializeNetworkProtocol(conf.Server.ForceProtocol)
	if err != nil {
		log.Fatalf("Failed to initialize network protocol: %v", err)
	}
	// Enhanced dual-stack HTTP client for robust IPv4/IPv6 and resource management
	// See: https://pkg.go.dev/net/http#Transport for details on these settings
	dualStackClient = &http.Client{
		Transport: &http.Transport{
			DialContext:           dialer.DialContext,
			IdleConnTimeout:       90 * time.Second, // Close idle connections after 90s
			MaxIdleConns:          100,              // Max idle connections across all hosts
			MaxIdleConnsPerHost:   10,               // Max idle connections per host
			TLSHandshakeTimeout:   10 * time.Second, // Timeout for TLS handshake
			ResponseHeaderTimeout: 15 * time.Second, // Timeout for reading response headers
		},
	}

	server := &http.Server{
		Addr:           conf.Server.BindIP + ":" + conf.Server.ListenAddress, // Use BindIP + ListenAddress (port)
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
			log.Infof("Metrics server started on port %s", conf.Server.MetricsPort)       // Corrected field name
			if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil { // Corrected field name
				log.Fatalf("Metrics server failed: %v", err)
			}
			wg.Wait()
		}()
	}

	setupGracefulShutdown(server, cancel) // Assuming setupGracefulShutdown is defined

	if conf.Server.AutoAdjustWorkers { // Corrected field name
		go monitorWorkerPerformance(ctx, &conf.Server, &conf.Workers, &conf.ClamAV)
	}

	versionString = "3.2" // Set a default version for now
	if conf.Build.Version != "" {
		versionString = conf.Build.Version
	}
	log.Infof("Running version: %s", versionString)

	log.Infof("Starting HMAC file server %s...", versionString)
	if conf.Server.UnixSocket {
		socketPath := "/tmp/hmac-file-server.sock" // Use a default socket path since ListenAddress is now a port
		if err := os.RemoveAll(socketPath); err != nil {
			log.Fatalf("Failed to remove existing Unix socket: %v", err)
		}
		listener, err := net.Listen("unix", socketPath)
		if err != nil {
			log.Fatalf("Failed to listen on Unix socket %s: %v", socketPath, err)
		}
		defer listener.Close()
		log.Infof("Server listening on Unix socket: %s", socketPath)
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	} else {
		if conf.Server.BindIP == "0.0.0.0" {
			log.Info("Binding to 0.0.0.0. Any net/http logs you see are normal for this universal address.")
		}
		log.Infof("Server listening on %s", server.Addr)
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	}

	// Start file cleanup in a separate goroutine
	// Use helper function
	go handleFileCleanup(&conf)
}

func printExampleConfig() {
	fmt.Print(`
[server]
bind_ip = "0.0.0.0"
listenport = "8080"
unixsocket = false
storagepath = "./uploads"
logfile = "/var/log/hmac-file-server.log"
metricsenabled = true
metricsport = "9090"
minfreebytes = "100MB"
filettl = "8760h"
filettlenabled = true
autoadjustworkers = true
networkevents = true
pidfilepath = "/var/run/hmacfileserver.pid"
cleanuponexit = true
precaching = true
deduplicationenabled = true
globalextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]
# FileNaming options: "HMAC", "None"
filenaming = "HMAC"
forceprotocol = "auto"

[logging]
level = "info"
file = "/var/log/hmac-file-server.log"
max_size = 100
max_backups = 7
max_age = 30
compress = true

[deduplication]
enabled = true
directory = "./deduplication"

[iso]
enabled = true
size = "1GB"
mountpoint = "/mnt/iso"
charset = "utf-8"
containerfile = "/mnt/iso/container.iso"

[timeouts]
readtimeout = "4800s"
writetimeout = "4800s"
idletimeout = "4800s"

[security]
secret = "changeme"
enablejwt = false
jwtsecret = "anothersecretkey"
jwtalgorithm = "HS256"
jwtexpiration = "24h"

[versioning]
enableversioning = false
maxversions = 1

[uploads]
resumableuploadsenabled = true
chunkeduploadsenabled = true
chunksize = "8192"
allowedextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[downloads]
resumabledownloadsenabled = true
chunkeddownloadsenabled = true
chunksize = "8192"
allowedextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[clamav]
clamavenabled = true
clamavsocket = "/var/run/clamav/clamd.ctl"
numscanworkers = 2
scanfileextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[redis]
redisenabled = true
redisdbindex = 0
redisaddr = "localhost:6379"
redispassword = ""
redishealthcheckinterval = "120s"

[workers]
numworkers = 4
uploadqueuesize = 50

[file]
# Add file-specific configurations here

[build]
version = "3.2"
`)
}

func getExampleConfigString() string {
	return `[server]
listen_address = ":8080"
storage_path = "/srv/hmac-file-server/uploads"
metrics_enabled = true
metrics_path = "/metrics"
pid_file = "/var/run/hmac-file-server.pid"
max_upload_size = "10GB" # Supports B, KB, MB, GB, TB
max_header_bytes = 1048576 # 1MB
cleanup_interval = "24h"
max_file_age = "720h" # 30 days
pre_cache = true
pre_cache_workers = 4
pre_cache_interval = "1h"
global_extensions = [".txt", ".dat", ".iso"] # If set, overrides upload/download extensions
deduplication_enabled = true
min_free_bytes = "1GB" # Minimum free space required for uploads
file_naming = "original" # Options: "original", "HMAC"
force_protocol = "" # Options: "http", "https" - if set, redirects to this protocol
enable_dynamic_workers = true # Enable dynamic worker scaling
worker_scale_up_thresh = 50   # Queue length to scale up workers
worker_scale_down_thresh = 10 # Queue length to scale down workers

[uploads]
allowed_extensions = [".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp"]
chunked_uploads_enabled = true
chunk_size = "10MB"
resumable_uploads_enabled = true
max_resumable_age = "48h"

[downloads]
allowed_extensions = [".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp"]
chunked_downloads_enabled = true
chunk_size = "10MB"
resumable_downloads_enabled = true

[security]
secret = "your-very-secret-hmac-key"
enablejwt = false
jwtsecret = "anothersecretkey"
jwtalgorithm = "HS256"
jwtexpiration = "24h"

[logging]
level = "info"
file = "/var/log/hmac-file-server.log"
max_size = 100
max_backups = 7
max_age = 30
compress = true

[deduplication]
enabled = true
directory = "./deduplication"

[iso]
enabled = true
size = "1GB"
mountpoint = "/mnt/iso"
charset = "utf-8"
containerfile = "/mnt/iso/container.iso"

[timeouts]
readtimeout = "4800s"
writetimeout = "4800s"
idletimeout = "4800s"

[security]
secret = "changeme"
enablejwt = false
jwtsecret = "anothersecretkey"
jwtalgorithm = "HS256"
jwtexpiration = "24h"

[versioning]
enableversioning = false
maxversions = 1

[uploads]
resumableuploadsenabled = true
chunkeduploadsenabled = true
chunksize = "8192"
allowedextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[downloads]
resumabledownloadsenabled = true
chunkeddownloadsenabled = true
chunksize = "8192"
allowedextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[clamav]
clamavenabled = true
clamavsocket = "/var/run/clamav/clamd.ctl"
numscanworkers = 2
scanfileextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[redis]
redisenabled = true
redisdbindex = 0
redisaddr = "localhost:6379"
redispassword = ""
redishealthcheckinterval = "120s"

[workers]
numworkers = 4
uploadqueuesize = 50

[file]
# Add file-specific configurations here

[build]
version = "3.2"
`
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
	workerAdjustmentsTotal.Inc()
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
				workerReAdjustmentsTotal.Inc()
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
	viper.SetDefault("server.listen_address", ":8080")
	viper.SetDefault("server.storage_path", "./uploads")
	viper.SetDefault("server.metrics_enabled", true)
	viper.SetDefault("server.metrics_path", "/metrics")
	viper.SetDefault("server.pid_file", "/var/run/hmac-file-server.pid")
	viper.SetDefault("server.max_upload_size", "10GB")
	viper.SetDefault("server.max_header_bytes", 1048576) // 1MB
	viper.SetDefault("server.cleanup_interval", "24h")
	viper.SetDefault("server.max_file_age", "720h") // 30 days
	viper.SetDefault("server.pre_cache", true)
	viper.SetDefault("server.pre_cache_workers", 4)
	viper.SetDefault("server.pre_cache_interval", "1h")
	viper.SetDefault("server.global_extensions", []string{})
	viper.SetDefault("server.deduplication_enabled", true)
	viper.SetDefault("server.min_free_bytes", "1GB")
	viper.SetDefault("server.file_naming", "original")
	viper.SetDefault("server.force_protocol", "auto")
	viper.SetDefault("server.enable_dynamic_workers", true)
	viper.SetDefault("server.worker_scale_up_thresh", 50)
	viper.SetDefault("server.worker_scale_down_thresh", 10)

	viper.SetDefault("uploads.allowed_extensions", []string{".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp"})
	viper.SetDefault("uploads.chunked_uploads_enabled", true)
	viper.SetDefault("uploads.chunk_size", "10MB")
	viper.SetDefault("uploads.resumable_uploads_enabled", true)
	viper.SetDefault("uploads.max_resumable_age", "48h")

	viper.SetDefault("downloads.allowed_extensions", []string{".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp"})
	viper.SetDefault("downloads.chunked_downloads_enabled", true)
	viper.SetDefault("downloads.chunk_size", "10MB")
	viper.SetDefault("downloads.resumable_downloads_enabled", true)

	viper.SetDefault("security.secret", "your-very-secret-hmac-key")
	viper.SetDefault("security.enablejwt", false)
	viper.SetDefault("security.jwtsecret", "your-256-bit-secret")
	viper.SetDefault("security.jwtalgorithm", "HS256")
	viper.SetDefault("security.jwtexpiration", "24h")

	// Logging defaults
	viper.SetDefault("logging.level", "info")
	viper.SetDefault("logging.file", "/var/log/hmac-file-server.log")
	viper.SetDefault("logging.max_size", 100)
	viper.SetDefault("logging.max_backups", 7)
	viper.SetDefault("logging.max_age", 30)
	viper.SetDefault("logging.compress", true)

	// Deduplication defaults
	viper.SetDefault("deduplication.enabled", false)
	viper.SetDefault("deduplication.directory", "./dedup_store")

	// ISO defaults
	viper.SetDefault("iso.enabled", false)
	viper.SetDefault("iso.mount_point", "/mnt/hmac_iso")
	viper.SetDefault("iso.size", "1GB")
	viper.SetDefault("iso.charset", "utf-8")
	viper.SetDefault("iso.containerfile", "/var/lib/hmac-file-server/data.iso")

	// Timeouts defaults
	viper.SetDefault("timeouts.read", "60s")
	viper.SetDefault("timeouts.write", "60s")
	viper.SetDefault("timeouts.idle", "120s")
	viper.SetDefault("timeouts.shutdown", "30s")

	// Versioning defaults
	viper.SetDefault("versioning.enabled", false)
	viper.SetDefault("versioning.backend", "simple")
	viper.SetDefault("versioning.max_revisions", 5)

	// ... other defaults for Uploads, Downloads, ClamAV, Redis, Workers, File, Build
	viper.SetDefault("build.version", "dev")
}

func validateConfig(c *Config) error {
	if c.Server.ListenAddress == "" { // Corrected field name
		return errors.New("server.listen_address is required")
	}

	if c.Server.FileTTL == "" && c.Server.FileTTLEnabled { // Corrected field names
		return errors.New("server.file_ttl is required when server.file_ttl_enabled is true")
	}

	if _, err := time.ParseDuration(c.Timeouts.Read); err != nil { // Corrected field name
		return fmt.Errorf("invalid timeouts.read: %v", err)
	}
	if _, err := time.ParseDuration(c.Timeouts.Write); err != nil { // Corrected field name
		return fmt.Errorf("invalid timeouts.write: %v", err)
	}
	if _, err := time.ParseDuration(c.Timeouts.Idle); err != nil { // Corrected field name
		return fmt.Errorf("invalid timeouts.idle: %v", err)
	}

	// Corrected VersioningConfig field access
	if c.Versioning.Enabled { // Use the Go struct field name 'Enabled'
		if c.Versioning.MaxRevs <= 0 { // Use the Go struct field name 'MaxRevs'
			return errors.New("versioning.max_revisions must be positive if versioning is enabled")
		}
	}

	// Validate JWT secret if JWT is enabled
	if c.Security.EnableJWT && strings.TrimSpace(c.Security.JWTSecret) == "" {
		return errors.New("security.jwtsecret is required when security.enablejwt is true")
	}

	// Validate HMAC secret if JWT is not enabled (as it's the fallback)
	if !c.Security.EnableJWT && strings.TrimSpace(c.Security.Secret) == "" {
		return errors.New("security.secret is required for HMAC authentication (when JWT is disabled)")
	}

	return nil
}

// validateJWTFromRequest extracts and validates a JWT from the request.
func validateJWTFromRequest(r *http.Request, secret string) (*jwt.Token, error) {
	authHeader := r.Header.Get("Authorization")
	tokenString := ""

	if authHeader != "" {
		splitToken := strings.Split(authHeader, "Bearer ")
		if len(splitToken) == 2 {
			tokenString = splitToken[1]
		} else {
			return nil, errors.New("invalid Authorization header format")
		}
	} else {
		// Fallback to checking 'token' query parameter
		tokenString = r.URL.Query().Get("token")
		if tokenString == "" {
			return nil, errors.New("missing JWT in Authorization header or 'token' query parameter")
		}
	}

	token, err := jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
		if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("unexpected signing method: %v", token.Header["alg"])
		}
		return []byte(secret), nil
	})

	if err != nil {
		return nil, fmt.Errorf("JWT validation failed: %w", err)
	}

	if !token.Valid {
		return nil, errors.New("invalid JWT")
	}

	return token, nil
}

// validateHMAC validates the HMAC signature of the request for legacy protocols and POST uploads.
func validateHMAC(r *http.Request, secret string) error {
	log.Debugf("validateHMAC: Validating request to %s with query: %s", r.URL.Path, r.URL.RawQuery)
	// Check for X-Signature header (for POST uploads)
	signature := r.Header.Get("X-Signature")
	if signature != "" {
		// This is a POST upload with X-Signature header
		message := r.URL.Path
		h := hmac.New(sha256.New, []byte(secret))
		h.Write([]byte(message))
		expectedSignature := hex.EncodeToString(h.Sum(nil))

		if !hmac.Equal([]byte(signature), []byte(expectedSignature)) {
			return errors.New("invalid HMAC signature in X-Signature header")
		}
		return nil
	}

	// Check for legacy URL-based HMAC protocols (v, v2, token)
	query := r.URL.Query()

	var protocolVersion string
	var providedMACHex string

	if query.Get("v2") != "" {
		protocolVersion = "v2"
		providedMACHex = query.Get("v2")
	} else if query.Get("token") != "" {
		protocolVersion = "token"
		providedMACHex = query.Get("token")
	} else if query.Get("v") != "" {
		protocolVersion = "v"
		providedMACHex = query.Get("v")
	} else {
		return errors.New("no HMAC signature found (missing X-Signature header or v/v2/token query parameter)")
	}

	// Extract file path from URL
	fileStorePath := strings.TrimPrefix(r.URL.Path, "/")

	// Calculate HMAC based on protocol version (matching legacy behavior)
	mac := hmac.New(sha256.New, []byte(secret))

	if protocolVersion == "v" {
		// Legacy v protocol: fileStorePath + "\x20" + contentLength
		message := fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)
		mac.Write([]byte(message))
	} else {
		// v2 and token protocols: fileStorePath + "\x00" + contentLength + "\x00" + contentType
		contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
		if contentType == "" {
			contentType = "application/octet-stream"
		}
		message := fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10) + "\x00" + contentType
		log.Debugf("validateHMAC: %s protocol message: %q (len=%d)", protocolVersion, message, len(message))
		mac.Write([]byte(message))
	}

	calculatedMAC := mac.Sum(nil)
	calculatedMACHex := hex.EncodeToString(calculatedMAC)

	// Decode provided MAC
	providedMAC, err := hex.DecodeString(providedMACHex)
	if err != nil {
		return fmt.Errorf("invalid MAC encoding for %s protocol: %v", protocolVersion, err)
	}

	log.Debugf("validateHMAC: %s protocol - calculated: %s, provided: %s", protocolVersion, calculatedMACHex, providedMACHex)

	// Compare MACs
	if !hmac.Equal(calculatedMAC, providedMAC) {
		return fmt.Errorf("invalid MAC for %s protocol", protocolVersion)
	}

	log.Debugf("%s HMAC authentication successful for request: %s", protocolVersion, r.URL.Path)
	return nil
}

// validateV3HMAC validates the HMAC signature for v3 protocol (mod_http_upload_external).
func validateV3HMAC(r *http.Request, secret string) error {
	query := r.URL.Query()

	// Extract v3 signature and expires from query parameters
	signature := query.Get("v3")
	expiresStr := query.Get("expires")

	if signature == "" {
		return errors.New("missing v3 signature parameter")
	}

	if expiresStr == "" {
		return errors.New("missing expires parameter")
	}

	// Parse expires timestamp
	expires, err := strconv.ParseInt(expiresStr, 10, 64)
	if err != nil {
		return fmt.Errorf("invalid expires parameter: %v", err)
	}

	// Check if signature has expired with extended grace period for large files
	now := time.Now().Unix()
	if now > expires {
		// Calculate dynamic grace period based on file size and client type
		gracePeriod := int64(3600) // Default 1 hour grace period
		
		// Check User-Agent to identify XMPP clients and adjust accordingly
		userAgent := r.Header.Get("User-Agent")
		isXMPPClient := strings.Contains(strings.ToLower(userAgent), "gajim") ||
			strings.Contains(strings.ToLower(userAgent), "dino") ||
			strings.Contains(strings.ToLower(userAgent), "conversations") ||
			strings.Contains(strings.ToLower(userAgent), "xmpp")
			
		if isXMPPClient {
			gracePeriod = int64(7200) // 2 hours for XMPP clients
			log.Infof("Detected XMPP client (%s), using extended grace period", userAgent)
		}
		
		// Check Content-Length header to determine file size
		if contentLengthStr := r.Header.Get("Content-Length"); contentLengthStr != "" {
			if contentLength, parseErr := strconv.ParseInt(contentLengthStr, 10, 64); parseErr == nil {
				// For files > 100MB, add additional grace time
				if contentLength > 100*1024*1024 {
					// Add 2 minutes per 100MB for large files
					additionalTime := (contentLength / (100 * 1024 * 1024)) * 120
					gracePeriod += additionalTime
					log.Infof("Extended grace period for large file (%d bytes, %s): %d seconds total", 
						contentLength, userAgent, gracePeriod)
				}
			}
		}
		
		// Apply maximum grace period limit to prevent abuse
		maxGracePeriod := int64(14400) // 4 hours maximum
		if gracePeriod > maxGracePeriod {
			gracePeriod = maxGracePeriod
		}
		
		if now > (expires + gracePeriod) {
			log.Warnf("Signature expired beyond grace period: now=%d, expires=%d, grace_period=%d, user_agent=%s", 
				now, expires, gracePeriod, userAgent)
			return errors.New("signature has expired")
		} else {
			log.Infof("Signature within grace period: now=%d, expires=%d, grace_period=%d, user_agent=%s", 
				now, expires, gracePeriod, userAgent)
		}
	}

	// Construct message for HMAC verification
	// Format: METHOD\nEXPIRES\nPATH
	message := fmt.Sprintf("%s\n%s\n%s", r.Method, expiresStr, r.URL.Path)

	// Calculate expected HMAC signature
	h := hmac.New(sha256.New, []byte(secret))
	h.Write([]byte(message))
	expectedSignature := hex.EncodeToString(h.Sum(nil))

	// Compare signatures
	if !hmac.Equal([]byte(signature), []byte(expectedSignature)) {
		return errors.New("invalid v3 HMAC signature")
	}

	return nil
}

// generateSessionID creates a unique session ID for client tracking
func generateSessionID() string {
	return fmt.Sprintf("session_%d_%x", time.Now().UnixNano(), 
		sha256.Sum256([]byte(fmt.Sprintf("%d%s", time.Now().UnixNano(), conf.Security.Secret))))[:16]
}

// handleUpload handles file uploads.
func handleUpload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Only allow POST method
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		uploadErrorsTotal.Inc()
		return
	}

	// Authentication
	if conf.Security.EnableJWT {
		_, err := validateJWTFromRequest(r, conf.Security.JWTSecret)
		if err != nil {
			http.Error(w, fmt.Sprintf("JWT Authentication failed: %v", err), http.StatusUnauthorized)
			uploadErrorsTotal.Inc()
			return
		}
		log.Debugf("JWT authentication successful for upload request: %s", r.URL.Path)
	} else {
		err := validateHMAC(r, conf.Security.Secret)
		if err != nil {
			http.Error(w, fmt.Sprintf("HMAC Authentication failed: %v", err), http.StatusUnauthorized)
			uploadErrorsTotal.Inc()
			return
		}
		log.Debugf("HMAC authentication successful for upload request: %s", r.URL.Path)
	}

	// Client multi-interface tracking
	var clientSession *ClientSession
	if clientTracker != nil && conf.ClientNetwork.SessionBasedTracking {
		// Generate or extract session ID (from headers, form data, or create new)
		sessionID := r.Header.Get("X-Upload-Session-ID")
		if sessionID == "" {
			// Check if there's a session ID in form data
			sessionID = r.FormValue("session_id")
		}
		if sessionID == "" {
			// Generate new session ID
			sessionID = generateSessionID()
		}
		
		clientIP := getClientIP(r)
		clientSession = clientTracker.TrackClientSession(sessionID, clientIP, r)
		
		// Add session ID to response headers for client to use in subsequent requests
		w.Header().Set("X-Upload-Session-ID", sessionID)
		
		log.Debugf("Client session tracking: %s from IP %s (connection type: %s)", 
			sessionID, clientIP, clientSession.ConnectionType)
	}

	// Parse multipart form
	err := r.ParseMultipartForm(32 << 20) // 32MB max memory
	if err != nil {
		http.Error(w, fmt.Sprintf("Error parsing multipart form: %v", err), http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	// Get file from form
	file, header, err := r.FormFile("file")
	if err != nil {
		http.Error(w, fmt.Sprintf("Error getting file from form: %v", err), http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}
	defer file.Close()

	// Validate file size against max_upload_size if configured
	if conf.Server.MaxUploadSize != "" {
		maxSizeBytes, err := parseSize(conf.Server.MaxUploadSize)
		if err != nil {
			log.Errorf("Invalid max_upload_size configuration: %v", err)
			http.Error(w, "Server configuration error", http.StatusInternalServerError)
			uploadErrorsTotal.Inc()
			return
		}
		if header.Size > maxSizeBytes {
			http.Error(w, fmt.Sprintf("File size %s exceeds maximum allowed size %s", 
				formatBytes(header.Size), conf.Server.MaxUploadSize), http.StatusRequestEntityTooLarge)
			uploadErrorsTotal.Inc()
			return
		}
	}

	// Validate file extension if configured
	if len(conf.Uploads.AllowedExtensions) > 0 {
		ext := strings.ToLower(filepath.Ext(header.Filename))
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
		h.Write([]byte(header.Filename + time.Now().String()))
		filename = hex.EncodeToString(h.Sum(nil)) + filepath.Ext(header.Filename)
	default: // "original" or "None"
		filename = header.Filename
	}

	// Create full file path
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	absFilename := filepath.Join(storagePath, filename)

	// Pre-upload deduplication check: if file already exists and deduplication is enabled, return success immediately
	if conf.Server.DeduplicationEnabled {
		if existingFileInfo, err := os.Stat(absFilename); err == nil {
			// File already exists - return success immediately for deduplication hit
			duration := time.Since(startTime)
			uploadDuration.Observe(duration.Seconds())
			uploadsTotal.Inc()
			uploadSizeBytes.Observe(float64(existingFileInfo.Size()))
			filesDeduplicatedTotal.Inc()
			
			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusOK)
			response := map[string]interface{}{
				"success":  true,
				"filename": filename,
				"size":     existingFileInfo.Size(),
				"message":  "File already exists (deduplication hit)",
			}
			json.NewEncoder(w).Encode(response)
			
			log.Infof("Deduplication hit: file %s already exists (%s), returning success immediately", 
				filename, formatBytes(existingFileInfo.Size()))
			return
		}
	}

	// Create the file
	dst, err := os.Create(absFilename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error creating file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}
	defer dst.Close()

	// Copy file content
	written, err := io.Copy(dst, file)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error saving file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		// Clean up partial file
		os.Remove(absFilename)
		return
	}

	// Handle deduplication if enabled
	if conf.Server.DeduplicationEnabled {
		ctx := context.Background()
		err = handleDeduplication(ctx, absFilename)
		if err != nil {
			log.Warnf("Deduplication failed for %s: %v", absFilename, err)
		}
	}

	// Update metrics
	duration := time.Since(startTime)
	uploadDuration.Observe(duration.Seconds())
	uploadsTotal.Inc()
	uploadSizeBytes.Observe(float64(written))

	// Return success response
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)

	response := map[string]interface{}{
		"success":  true,
		"filename": filename,
		"size":     written,
		"duration": duration.String(),
	}

	// Create JSON response
	if jsonBytes, err := json.Marshal(response); err == nil {
		w.Write(jsonBytes)
	} else {
		fmt.Fprintf(w, `{"success": true, "filename": "%s", "size": %d}`, filename, written)
	}

	log.Infof("Successfully uploaded %s (%s) in %s", filename, formatBytes(written), duration)
}

// handleDownload handles file downloads.
func handleDownload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Authentication
	if conf.Security.EnableJWT {
		_, err := validateJWTFromRequest(r, conf.Security.JWTSecret)
		if err != nil {
			http.Error(w, fmt.Sprintf("JWT Authentication failed: %v", err), http.StatusUnauthorized)
			downloadErrorsTotal.Inc()
			return
		}
		log.Debugf("JWT authentication successful for download request: %s", r.URL.Path)
	} else {
		err := validateHMAC(r, conf.Security.Secret)
		if err != nil {
			http.Error(w, fmt.Sprintf("HMAC Authentication failed: %v", err), http.StatusUnauthorized)
			downloadErrorsTotal.Inc()
			return
		}
		log.Debugf("HMAC authentication successful for download request: %s", r.URL.Path)
	}

	filename := strings.TrimPrefix(r.URL.Path, "/download/")
	if filename == "" {
		http.Error(w, "Filename not specified", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	absFilename, err := sanitizeFilePath(conf.Server.StoragePath, filename) // Use sanitizeFilePath from helpers.go
	if err != nil {
		http.Error(w, fmt.Sprintf("Invalid file path: %v", err), http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	fileInfo, err := os.Stat(absFilename)
	if os.IsNotExist(err) {
		http.Error(w, "File not found", http.StatusNotFound)
		downloadErrorsTotal.Inc()
		return
	}
	if err != nil {
		http.Error(w, fmt.Sprintf("Error accessing file: %v", err), http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}

	if fileInfo.IsDir() {
		http.Error(w, "Cannot download a directory", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	file, err := os.Open(absFilename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error opening file: %v", err), http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}
	defer file.Close()

	w.Header().Set("Content-Disposition", "attachment; filename=\""+filepath.Base(absFilename)+"\"")
	w.Header().Set("Content-Type", "application/octet-stream")
	w.Header().Set("Content-Length", fmt.Sprintf("%d", fileInfo.Size()))

	// Use a pooled buffer for copying
	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)
	buf := *bufPtr

	n, err := io.CopyBuffer(w, file, buf)
	if err != nil {
		log.Errorf("Error during download of %s: %v", absFilename, err)
		// Don't write http.Error here if headers already sent
		downloadErrorsTotal.Inc()
		return // Ensure we don't try to record metrics if there was an error during copy
	}

	duration := time.Since(startTime)
	downloadDuration.Observe(duration.Seconds())
	downloadsTotal.Inc()
	downloadSizeBytes.Observe(float64(n))
	log.Infof("Successfully downloaded %s (%s) in %s", absFilename, formatBytes(n), duration)
}

// handleV3Upload handles PUT requests for v3 protocol (mod_http_upload_external).
func handleV3Upload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Only allow PUT method for v3
	if r.Method != http.MethodPut {
		http.Error(w, "Method not allowed for v3 uploads", http.StatusMethodNotAllowed)
		uploadErrorsTotal.Inc()
		return
	}

	// Validate v3 HMAC signature
	err := validateV3HMAC(r, conf.Security.Secret)
	if err != nil {
		http.Error(w, fmt.Sprintf("v3 Authentication failed: %v", err), http.StatusUnauthorized)
		uploadErrorsTotal.Inc()
		return
	}
	log.Debugf("v3 HMAC authentication successful for upload request: %s", r.URL.Path)

	// Extract filename from the URL path
	// Path format: /uuid/subdir/filename.ext
	pathParts := strings.Split(strings.Trim(r.URL.Path, "/"), "/")
	if len(pathParts) < 1 {
		http.Error(w, "Invalid upload path", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	// Use the last part as filename
	originalFilename := pathParts[len(pathParts)-1]
	if originalFilename == "" {
		http.Error(w, "No filename specified", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
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

	// Validate file size against max_upload_size if configured
	if conf.Server.MaxUploadSize != "" {
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

	// Create full file path
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	absFilename := filepath.Join(storagePath, filename)

	// Pre-upload deduplication check: if file already exists and deduplication is enabled, return success immediately
	if conf.Server.DeduplicationEnabled {
		if existingFileInfo, err := os.Stat(absFilename); err == nil {
			// File already exists - return success immediately for deduplication hit
			duration := time.Since(startTime)
			uploadDuration.Observe(duration.Seconds())
			uploadsTotal.Inc()
			uploadSizeBytes.Observe(float64(existingFileInfo.Size()))
			filesDeduplicatedTotal.Inc()
			
			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusOK)
			response := map[string]interface{}{
				"success":  true,
				"filename": filename,
				"size":     existingFileInfo.Size(),
				"message":  "File already exists (deduplication hit)",
			}
			json.NewEncoder(w).Encode(response)
			
			log.Infof("Deduplication hit: file %s already exists (%s), returning success immediately", 
				filename, formatBytes(existingFileInfo.Size()))
			return
		}
	}

	// Create the file
	dst, err := os.Create(absFilename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error creating file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}
	defer dst.Close()

	// Copy file content from request body
	written, err := io.Copy(dst, r.Body)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error saving file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		// Clean up partial file
		os.Remove(absFilename)
		return
	}

	// Handle deduplication if enabled
	if conf.Server.DeduplicationEnabled {
		ctx := context.Background()
		err = handleDeduplication(ctx, absFilename)
		if err != nil {
			log.Warnf("Deduplication failed for %s: %v", absFilename, err)
		}
	}

	// Update metrics
	duration := time.Since(startTime)
	uploadDuration.Observe(duration.Seconds())
	uploadsTotal.Inc()
	uploadSizeBytes.Observe(float64(written))

	// Return success response
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)

	response := map[string]interface{}{
		"success":  true,
		"filename": filename,
		"size":     written,
		"duration": duration.String(),
	}

	// Create JSON response
	if jsonBytes, err := json.Marshal(response); err == nil {
		w.Write(jsonBytes)
	} else {
		fmt.Fprintf(w, `{"success": true, "filename": "%s", "size": %d}`, filename, written)
	}

	log.Infof("Successfully uploaded %s via v3 protocol (%s) in %s", filename, formatBytes(written), duration)
}

// handleLegacyUpload handles PUT requests for legacy protocols (v, v2, token).
func handleLegacyUpload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	log.Infof("🔥 DEBUG: handleLegacyUpload called - method:%s path:%s query:%s", r.Method, r.URL.Path, r.URL.RawQuery)

	log.Debugf("handleLegacyUpload: Processing request to %s with query: %s", r.URL.Path, r.URL.RawQuery)

	// Only allow PUT method for legacy uploads
	if r.Method != http.MethodPut {
		http.Error(w, "Method not allowed for legacy uploads", http.StatusMethodNotAllowed)
		uploadErrorsTotal.Inc()
		return
	}

	// Validate legacy HMAC signature
	err := validateHMAC(r, conf.Security.Secret)
	if err != nil {
		http.Error(w, fmt.Sprintf("Legacy Authentication failed: %v", err), http.StatusUnauthorized)
		uploadErrorsTotal.Inc()
		return
	}

	log.Debugf("✅ HMAC validation passed for: %s", r.URL.Path)

	// Extract filename from the URL path
	fileStorePath := strings.TrimPrefix(r.URL.Path, "/")
	if fileStorePath == "" {
		log.Debugf("❌ No filename specified")
		http.Error(w, "No filename specified", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	log.Debugf("✅ File path extracted: %s", fileStorePath)

	// Validate file extension if configured
	if len(conf.Uploads.AllowedExtensions) > 0 {
		ext := strings.ToLower(filepath.Ext(fileStorePath))
		log.Infof("� DEBUG: Checking file extension: %s against %d allowed extensions", ext, len(conf.Uploads.AllowedExtensions))
		log.Infof("� DEBUG: Allowed extensions: %v", conf.Uploads.AllowedExtensions)
		allowed := false
		for i, allowedExt := range conf.Uploads.AllowedExtensions {
			log.Infof("� DEBUG: [%d] Comparing '%s' == '%s'", i, ext, allowedExt)
			if ext == allowedExt {
				allowed = true
				log.Infof("🔥 DEBUG: Extension match found!")
				break
			}
		}
		if !allowed {
			log.Infof("🔥 DEBUG: Extension %s not found in allowed list", ext)
			http.Error(w, fmt.Sprintf("File extension %s not allowed", ext), http.StatusBadRequest)
			uploadErrorsTotal.Inc()
			return
		}
		log.Infof("🔥 DEBUG: File extension %s is allowed", ext)
	}

	// Validate file size against max_upload_size if configured
	if conf.Server.MaxUploadSize != "" {
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

	// Create full file path
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	// Generate filename based on configuration
	var absFilename string
	var filename string
	switch conf.Server.FileNaming {
	case "HMAC":
		// Generate HMAC-based filename
		h := hmac.New(sha256.New, []byte(conf.Security.Secret))
		h.Write([]byte(fileStorePath + time.Now().String()))
		filename = hex.EncodeToString(h.Sum(nil)) + filepath.Ext(fileStorePath)
		absFilename = filepath.Join(storagePath, filename)
	default: // "original" or "None"
		// Preserve full directory structure for legacy XMPP compatibility
		var sanitizeErr error
		absFilename, sanitizeErr = sanitizeFilePath(storagePath, fileStorePath)
		if sanitizeErr != nil {
			http.Error(w, fmt.Sprintf("Invalid file path: %v", sanitizeErr), http.StatusBadRequest)
			uploadErrorsTotal.Inc()
			return
		}
		filename = filepath.Base(fileStorePath) // For logging purposes
	}

	// Create directory structure if it doesn't exist
	if err := os.MkdirAll(filepath.Dir(absFilename), 0755); err != nil {
		http.Error(w, fmt.Sprintf("Error creating directory: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}

	// Pre-upload deduplication check: if file already exists and deduplication is enabled, return success immediately
	if conf.Server.DeduplicationEnabled {
		if existingFileInfo, err := os.Stat(absFilename); err == nil {
			// File already exists - return success immediately for deduplication hit
			duration := time.Since(startTime)
			uploadDuration.Observe(duration.Seconds())
			uploadsTotal.Inc()
			uploadSizeBytes.Observe(float64(existingFileInfo.Size()))
			filesDeduplicatedTotal.Inc()
			
			w.WriteHeader(http.StatusCreated) // 201 Created for legacy compatibility
			log.Infof("Deduplication hit: file %s already exists (%s), returning success immediately", 
				filename, formatBytes(existingFileInfo.Size()))
			return
		}
	}

	// Create the file
	dst, err := os.Create(absFilename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error creating file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}
	defer dst.Close()

	// Log upload start for large files
	if r.ContentLength > 10*1024*1024 { // Log for files > 10MB
		log.Infof("Starting upload of %s (%.1f MiB)", filename, float64(r.ContentLength)/(1024*1024))
	}

	// Copy file content from request body with progress reporting
	written, err := copyWithProgress(dst, r.Body, r.ContentLength, filename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error saving file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		// Clean up partial file
		os.Remove(absFilename)
		return
	}

	// Handle deduplication if enabled
	if conf.Server.DeduplicationEnabled {
		ctx := context.Background()
		err = handleDeduplication(ctx, absFilename)
		if err != nil {
			log.Warnf("Deduplication failed for %s: %v", absFilename, err)
		}
	}

	// Update metrics
	duration := time.Since(startTime)
	uploadDuration.Observe(duration.Seconds())
	uploadsTotal.Inc()
	uploadSizeBytes.Observe(float64(written))

	// Return success response (201 Created for legacy compatibility)
	w.WriteHeader(http.StatusCreated)

	log.Infof("Successfully uploaded %s via legacy protocol (%s) in %s", filename, formatBytes(written), duration)
}

// handleLegacyDownload handles GET/HEAD requests for legacy downloads.
func handleLegacyDownload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Extract filename from the URL path
	fileStorePath := strings.TrimPrefix(r.URL.Path, "/")
	if fileStorePath == "" {
		http.Error(w, "No filename specified", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	// Create full file path
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	absFilename := filepath.Join(storagePath, fileStorePath)

	fileInfo, err := os.Stat(absFilename)
	if os.IsNotExist(err) {
		http.Error(w, "File not found", http.StatusNotFound)
		downloadErrorsTotal.Inc()
		return
	}
	if err != nil {
		http.Error(w, fmt.Sprintf("Error accessing file: %v", err), http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}

	if fileInfo.IsDir() {
		http.Error(w, "Cannot download a directory", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	// Set appropriate headers
	contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
	if contentType == "" {
		contentType = "application/octet-stream"
	}
	w.Header().Set("Content-Type", contentType)
	w.Header().Set("Content-Length", strconv.FormatInt(fileInfo.Size(), 10))

	// For HEAD requests, only send headers
	if r.Method == http.MethodHead {
		w.WriteHeader(http.StatusOK)
		downloadsTotal.Inc()
		return
	}

	// For GET requests, serve the file
	file, err := os.Open(absFilename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error opening file: %v", err), http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}
	defer file.Close()

	// Use a pooled buffer for copying
	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)
	buf := *bufPtr

	n, err := io.CopyBuffer(w, file, buf)
	if err != nil {
		log.Errorf("Error during download of %s: %v", absFilename, err)
		downloadErrorsTotal.Inc()
		return
	}

	duration := time.Since(startTime)
	downloadDuration.Observe(duration.Seconds())
	downloadsTotal.Inc()
	downloadSizeBytes.Observe(float64(n))
	log.Infof("Successfully downloaded %s (%s) in %s", absFilename, formatBytes(n), duration)
}

// printValidationChecks prints all available validation checks
func printValidationChecks() {
	fmt.Println("HMAC File Server Configuration Validation Checks")
	fmt.Println("=================================================")
	fmt.Println()

	fmt.Println("🔍 CORE VALIDATION CHECKS:")
	fmt.Println("  ✓ server.*        - Server configuration (ports, paths, protocols)")
	fmt.Println("  ✓ security.*      - Security settings (secrets, JWT, authentication)")
	fmt.Println("  ✓ logging.*       - Logging configuration (levels, files, rotation)")
	fmt.Println("  ✓ timeouts.*      - Timeout settings (read, write, idle)")
	fmt.Println("  ✓ uploads.*       - Upload configuration (extensions, chunk size)")
	fmt.Println("  ✓ downloads.*     - Download configuration (extensions, chunk size)")
	fmt.Println("  ✓ workers.*       - Worker pool configuration (count, queue size)")
	fmt.Println("  ✓ redis.*         - Redis configuration (address, credentials)")
	fmt.Println("  ✓ clamav.*        - ClamAV antivirus configuration")
	fmt.Println("  ✓ versioning.*    - File versioning configuration")
	fmt.Println("  ✓ deduplication.* - File deduplication configuration")
	fmt.Println("  ✓ iso.*           - ISO filesystem configuration")
	fmt.Println()

	fmt.Println("🔐 SECURITY CHECKS:")
	fmt.Println("  ✓ Secret strength analysis (length, entropy, patterns)")
	fmt.Println("  ✓ Default/example value detection")
	fmt.Println("  ✓ JWT algorithm security recommendations")
	fmt.Println("  ✓ Network binding security (0.0.0.0 warnings)")
	fmt.Println("  ✓ File permission analysis")
	fmt.Println("  ✓ Debug logging security implications")
	fmt.Println()

	fmt.Println("⚡ PERFORMANCE CHECKS:")
	fmt.Println("  ✓ Worker count vs CPU cores optimization")
	fmt.Println("  ✓ Queue size vs memory usage analysis")
	fmt.Println("  ✓ Timeout configuration balance")
	fmt.Println("  ✓ Large file handling preparation")
	fmt.Println("  ✓ Memory-intensive configuration detection")
	fmt.Println()

	fmt.Println("🌐 CONNECTIVITY CHECKS:")
	fmt.Println("  ✓ Redis server connectivity testing")
	fmt.Println("  ✓ ClamAV socket accessibility")
	fmt.Println("  ✓ Network address format validation")
	fmt.Println("  ✓ DNS resolution testing")
	fmt.Println()

	fmt.Println("💾 SYSTEM RESOURCE CHECKS:")
	fmt.Println("  ✓ CPU core availability analysis")
	fmt.Println("  ✓ Memory usage monitoring")
	fmt.Println("  ✓ Disk space validation")
	fmt.Println("  ✓ Directory write permissions")
	fmt.Println("  ✓ Goroutine count analysis")
	fmt.Println()

	fmt.Println("🔄 CROSS-SECTION VALIDATION:")
	fmt.Println("  ✓ Path conflict detection")
	fmt.Println("  ✓ Extension compatibility checks")
	fmt.Println("  ✓ Configuration consistency validation")
	fmt.Println()

	fmt.Println("📋 USAGE EXAMPLES:")
	fmt.Println("  hmac-file-server --validate-config              # Full validation")
	fmt.Println("  hmac-file-server --check-security               # Security checks only")
	fmt.Println("  hmac-file-server --check-performance            # Performance checks only")
	fmt.Println("  hmac-file-server --check-connectivity           # Network checks only")
	fmt.Println("  hmac-file-server --validate-quiet               # Errors only")
	fmt.Println("  hmac-file-server --validate-verbose             # Detailed output")
	fmt.Println("  hmac-file-server --check-fixable                # Auto-fixable issues")
	fmt.Println()
}
