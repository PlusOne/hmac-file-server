package main

import (
	"fmt"
	"os"

	"github.com/spf13/viper"
)

// DefaultConfig returns a Config struct populated with sensible defaults
func DefaultConfig() *Config {
	return &Config{
		Server: ServerConfig{
			ListenAddress:         "8080",
			StoragePath:           "./uploads",
			MetricsEnabled:        true,
			MetricsPath:           "/metrics",
			MetricsPort:           "9090",
			PidFile:               "/tmp/hmac-file-server.pid",
			PIDFilePath:           "/tmp/hmac-file-server.pid",
			MaxUploadSize:         "10GB",
			MaxHeaderBytes:        1048576, // 1MB
			CleanupInterval:       "24h",
			MaxFileAge:            "720h", // 30 days
			PreCache:              true,
			PreCacheWorkers:       4,
			PreCacheInterval:      "1h",
			GlobalExtensions:      []string{".txt", ".dat", ".iso", ".mp4", ".mkv", ".avi", ".mov", ".wmv", ".flv", ".webm", ".mpeg"},
			DeduplicationEnabled:  true,
			MinFreeBytes:          "1GB",
			FileNaming:            "original",
			ForceProtocol:         "",
			EnableDynamicWorkers:  true,
			WorkerScaleUpThresh:   40,  // Optimized from previous session
			WorkerScaleDownThresh: 10,
			NetworkEvents:         true,  // Enable network resilience by default
		},
		Uploads: UploadsConfig{
			AllowedExtensions:       []string{".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp", ".txt", ".pdf", ".png", ".jpg", ".jpeg"},
			ChunkedUploadsEnabled:   true,
			ChunkSize:               "10MB",
			ResumableUploadsEnabled: true,
			SessionTimeout:          "60m", // Extended from previous session
			MaxRetries:              3,
		},
		Downloads: DownloadsConfig{
			AllowedExtensions:         []string{".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp", ".zip"},
			ChunkedDownloadsEnabled:   true,
			ChunkSize:                 "10MB",
			ResumableDownloadsEnabled: true,
		},
		Security: SecurityConfig{
			Secret:        "your-very-secret-hmac-key",
			EnableJWT:     false,
			JWTSecret:     "your-256-bit-secret",
			JWTAlgorithm:  "HS256",
			JWTExpiration: "24h",
		},
		Logging: LoggingConfig{
			Level:      "info",
			File:       "/var/log/hmac-file-server.log",
			MaxSize:    100,
			MaxBackups: 7,
			MaxAge:     30,
			Compress:   true,
		},
		Deduplication: DeduplicationConfig{
			Enabled:   true,
			Directory: "./dedup_store",
			MaxSize:   "1GB",
		},
		ISO: ISOConfig{
			Enabled:       false,
			Size:          "1GB",
			MountPoint:    "/mnt/iso",
			Charset:       "utf-8",
			ContainerFile: "/mnt/iso/container.iso",
		},
		Timeouts: TimeoutConfig{
			Read:     "300s", // 5 minutes instead of 4800s 
			Write:    "300s",
			Idle:     "300s",
			Shutdown: "30s",
		},
		Versioning: VersioningConfig{
			Enabled: false,
			Backend: "simple",
			MaxRevs: 1,
		},
		ClamAV: ClamAVConfig{
			ClamAVEnabled:      false,
			ClamAVSocket:       "/var/run/clamav/clamd.ctl",
			NumScanWorkers:     2,
			ScanFileExtensions: []string{".txt", ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".exe", ".zip", ".rar", ".7z", ".tar", ".gz"},
			MaxScanSize:        "200MB",
		},
		Redis: RedisConfig{
			RedisEnabled:             false,
			RedisDBIndex:             0,
			RedisAddr:                "localhost:6379",
			RedisPassword:            "",
			RedisHealthCheckInterval: "120s",
		},
		Workers: WorkersConfig{
			NumWorkers:      4,
			UploadQueueSize: 100, // Optimized from previous session
		},
		NetworkResilience: NetworkResilienceConfig{
			FastDetection:        true,   // Enable fast 1-second detection
			QualityMonitoring:    true,   // Monitor connection quality
			PredictiveSwitching:  true,   // Switch before complete failure
			MobileOptimizations:  true,   // Mobile-friendly thresholds
			DetectionInterval:    "1s",   // Fast detection
			QualityCheckInterval: "5s",   // Regular quality checks
		},
		File: FileConfig{},
		Build: BuildConfig{
			Version: "3.2",
		},
	}
}

// LoadSimplifiedConfig loads configuration with a minimal config file approach
func LoadSimplifiedConfig(configPath string) (*Config, error) {
	// Start with comprehensive defaults
	config := DefaultConfig()

	// If no config file specified, try to find one in common locations
	if configPath == "" {
		possiblePaths := []string{
			"/opt/hmac-file-server/config.toml",
			"/etc/hmac-file-server/config.toml",
			"./config.toml",
			"../config.toml",
		}

		for _, path := range possiblePaths {
			if _, err := os.Stat(path); err == nil {
				configPath = path
				break
			}
		}
	}

	// If a config file exists, load it to override defaults
	if configPath != "" && fileExists(configPath) {
		viper.SetConfigFile(configPath)
		viper.SetConfigType("toml")

		if err := viper.ReadInConfig(); err != nil {
			return nil, fmt.Errorf("failed to read config file %s: %v", configPath, err)
		}

		// Unmarshal only the values that are explicitly set in the config file
		if err := viper.Unmarshal(config); err != nil {
			return nil, fmt.Errorf("failed to unmarshal config: %v", err)
		}
	}

	return config, nil
}

// fileExists checks if a file exists
func fileExists(filename string) bool {
	info, err := os.Stat(filename)
	if os.IsNotExist(err) {
		return false
	}
	return !info.IsDir()
}

// GenerateMinimalConfig creates a minimal config.toml with only essential settings
func GenerateMinimalConfig() string {
	return `# HMAC File Server - Minimal Configuration
# This file contains only the essential settings you might want to customize.
# All other settings use sensible defaults defined in the application.

[server]
# Network binding
listen_address = "8080"

# Storage location for uploaded files
storage_path = "./uploads"

# Security settings
[security]
# IMPORTANT: Change this secret key for production use!
secret = "your-very-secret-hmac-key"

# Logging configuration
[logging]
# Log level: debug, info, warn, error
level = "info"
file = "/var/log/hmac-file-server.log"

# Advanced settings (uncomment and modify if needed)
# [uploads]
# max_resumable_age = "48h"
# chunk_size = "10MB"
# networkevents = true

# [network_resilience]
# enabled = true
# fast_detection = true              # Enable 1-second detection for mobile
# quality_monitoring = true          # Monitor RTT and packet loss
# predictive_switching = true        # Switch before complete failure
# mobile_optimizations = true       # Cellular-friendly thresholds
# upload_resilience = true           # Resume uploads across network changes

# [workers]
# numworkers = 4
# uploadqueuesize = 100

# [deduplication]
# enabled = true
# directory = "./dedup_store"

# [timeouts]
# readtimeout = "4800s"
# writetimeout = "4800s"
# idletimeout = "4800s"

# [clamav]
# clamavenabled = false

# [redis]
# redisenabled = false
`
}

// createMinimalConfig writes a minimal config file to the current directory
func createMinimalConfig() error {
	content := GenerateMinimalConfig()
	return os.WriteFile("config.toml", []byte(content), 0644)
}

// GenerateAdvancedConfigTemplate creates a comprehensive config template for advanced users
func GenerateAdvancedConfigTemplate() string {
	return `# HMAC File Server - Advanced Configuration Template
# This template shows all available configuration options with their default values.
# Uncomment and modify only the settings you want to change.

[server]
listen_address = "8080"
storage_path = "./uploads"
metrics_enabled = true
metrics_path = "/metrics"
pid_file = "/var/run/hmac-file-server.pid"
max_upload_size = "10GB"
max_header_bytes = 1048576
cleanup_interval = "24h"
max_file_age = "720h"
pre_cache = true
pre_cache_workers = 4
pre_cache_interval = "1h"
global_extensions = [".txt", ".dat", ".iso", ".mp4", ".mkv", ".avi", ".mov", ".wmv", ".flv", ".webm", ".mpeg"]
deduplication_enabled = true
min_free_bytes = "1GB"
file_naming = "original"
force_protocol = ""
enable_dynamic_workers = true
worker_scale_up_thresh = 40
worker_scale_down_thresh = 10

[uploads]
allowed_extensions = [".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp", ".txt", ".pdf", ".jpg", ".jpeg", ".png", ".gif", ".webp", ".mp4", ".mov", ".ogg", ".mp3", ".doc", ".docx"]
chunked_uploads_enabled = true
chunk_size = "10MB"
resumable_uploads_enabled = true
max_resumable_age = "48h"
sessiontimeout = "60m"
maxretries = 3
networkevents = false                      # Enable network event monitoring for resilience

# Upload resilience and session management
session_persistence = true                 # Persist sessions across restarts
session_recovery_timeout = "300s"          # Session recovery timeout after network changes
client_reconnect_window = "120s"           # Time window for client reconnection
upload_slot_ttl = "3600s"                  # Upload slot validity time
retry_failed_uploads = true                # Auto-retry failed uploads
max_upload_retries = 3                     # Maximum retry attempts
allow_session_resume = true                # Allow resume from different IPs
session_persistence_duration = "24h"       # How long to keep session data
detect_duplicate_uploads = true            # Detect same upload from different IPs
merge_duplicate_sessions = true            # Merge sessions from same client

[downloads]
allowed_extensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]
chunked_downloads_enabled = true
chunk_size = "8192"
resumable_downloads_enabled = true

[security]
secret = "your-very-secret-hmac-key"
enablejwt = false
jwtsecret = "your-256-bit-secret"
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
directory = "./dedup_store"
maxsize = "1GB"

[iso]
enabled = false
size = "1GB"
mountpoint = "/mnt/iso"
charset = "utf-8"
containerfile = "/mnt/iso/container.iso"

[timeouts]
readtimeout = "4800s"
writetimeout = "4800s"
idletimeout = "4800s"

[versioning]
enableversioning = false
maxversions = 1

[clamav]
clamavenabled = false
clamavsocket = "/var/run/clamav/clamd.ctl"
numscanworkers = 2
scanfileextensions = [".txt", ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".exe", ".zip", ".rar", ".7z", ".tar", ".gz"]
maxscansize = "200MB"

[redis]
redisenabled = false
redisdbindex = 0
redisaddr = "localhost:6379"
redispassword = ""
redishealthcheckinterval = "120s"

[workers]
numworkers = 4
uploadqueuesize = 100

# Network Resilience Configuration (v3.2+)
[network_resilience]
enabled = true                              # Enable network resilience system
fast_detection = true                       # Enable 1-second network change detection
quality_monitoring = true                   # Monitor RTT and packet loss per interface
predictive_switching = true                 # Switch proactively before network failure
mobile_optimizations = true                # Use mobile-friendly thresholds for cellular networks
upload_resilience = true                    # Resume uploads across network changes
detection_interval = "1s"                  # Network change detection interval
quality_check_interval = "5s"              # Connection quality monitoring interval
max_detection_interval = "10s"             # Maximum detection interval during stable periods
network_change_threshold = 3               # Switches required to trigger network change
interface_stability_time = "30s"           # Time to wait before marking interface stable
upload_pause_timeout = "5m"               # Maximum time to pause uploads during network changes
upload_retry_timeout = "10m"              # Maximum time to retry uploads after network changes
rtt_warning_threshold = "200ms"            # RTT threshold for warning
rtt_critical_threshold = "1000ms"          # RTT threshold for critical
packet_loss_warning_threshold = 2.0        # Packet loss % for warning
packet_loss_critical_threshold = 10.0      # Packet loss % for critical

# Multi-Interface Support (v3.2+)
multi_interface_enabled = false            # Enable multi-interface management
interface_priority = ["eth0", "wlan0", "wwan0", "ppp0"]  # Interface priority order
auto_switch_enabled = true                 # Enable automatic interface switching
switch_threshold_latency = "500ms"         # Latency threshold for switching
switch_threshold_packet_loss = 5.0         # Packet loss threshold for switching
quality_degradation_threshold = 0.5        # Quality degradation threshold
max_switch_attempts = 3                    # Maximum switch attempts per detection
switch_detection_interval = "10s"          # Switch detection interval

# Client Network Support (v3.2+)
[client_network_support]
session_based_tracking = false             # Track sessions by ID instead of IP
allow_ip_changes = true                    # Allow session continuation from different IPs
session_migration_timeout = "5m"           # Time to wait for client reconnection
max_ip_changes_per_session = 10           # Maximum IP changes per session
client_connection_detection = false        # Detect client network type
adapt_to_client_network = false           # Optimize parameters based on client connection

[build]
version = "3.2"
`
}
