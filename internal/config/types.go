// Package config contains all configuration types and loading logic.
package config

import (
	"net/http"
	"time"
)

// ServerConfig holds server-level configuration.
type ServerConfig struct {
	ListenAddress         string   `toml:"listen_address" mapstructure:"listen_address"`
	StoragePath           string   `toml:"storage_path" mapstructure:"storage_path"`
	MetricsEnabled        bool     `toml:"metricsenabled" mapstructure:"metricsenabled"`
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
	UnixSocket            bool     `toml:"unixsocket" mapstructure:"unixsocket"`
	MetricsPort           string   `toml:"metricsport" mapstructure:"metricsport"`
	FileTTL               string   `toml:"filettl" mapstructure:"filettl"`
	FileTTLEnabled        bool     `toml:"filettlenabled" mapstructure:"filettlenabled"`
	AutoAdjustWorkers     bool     `toml:"autoadjustworkers" mapstructure:"autoadjustworkers"`
	NetworkEvents         bool     `toml:"networkevents" mapstructure:"networkevents"`
	PIDFilePath           string   `toml:"pidfilepath" mapstructure:"pidfilepath"`
	CleanUponExit         bool     `toml:"clean_upon_exit" mapstructure:"clean_upon_exit"`
	PreCaching            bool     `toml:"precaching" mapstructure:"precaching"`
	BindIP                string   `toml:"bind_ip" mapstructure:"bind_ip"`
}

// UploadsConfig holds upload-related configuration.
type UploadsConfig struct {
	AllowedExtensions       []string `toml:"allowed_extensions" mapstructure:"allowed_extensions"`
	ChunkedUploadsEnabled   bool     `toml:"chunked_uploads_enabled" mapstructure:"chunked_uploads_enabled"`
	ChunkSize               string   `toml:"chunk_size" mapstructure:"chunk_size"`
	ResumableUploadsEnabled bool     `toml:"resumable_uploads_enabled" mapstructure:"resumable_uploads_enabled"`
	SessionTimeout          string   `toml:"sessiontimeout" mapstructure:"sessiontimeout"`
	MaxRetries              int      `toml:"maxretries" mapstructure:"maxretries"`
}

// DownloadsConfig holds download-related configuration.
type DownloadsConfig struct {
	AllowedExtensions         []string `toml:"allowed_extensions" mapstructure:"allowed_extensions"`
	ChunkedDownloadsEnabled   bool     `toml:"chunked_downloads_enabled" mapstructure:"chunked_downloads_enabled"`
	ChunkSize                 string   `toml:"chunk_size" mapstructure:"chunk_size"`
	ResumableDownloadsEnabled bool     `toml:"resumable_downloads_enabled" mapstructure:"resumable_downloads_enabled"`
}

// SecurityConfig holds security-related configuration.
type SecurityConfig struct {
	Secret                    string `toml:"secret" mapstructure:"secret"`
	EnableJWT                 bool   `toml:"enablejwt" mapstructure:"enablejwt"`
	JWTSecret                 string `toml:"jwtsecret" mapstructure:"jwtsecret"`
	JWTAlgorithm              string `toml:"jwtalgorithm" mapstructure:"jwtalgorithm"`
	JWTExpiration             string `toml:"jwtexpiration" mapstructure:"jwtexpiration"`
	EnhancedSecurity          bool   `toml:"enhanced_security" mapstructure:"enhanced_security"`
	ChallengeOnNetworkChange  bool   `toml:"challenge_on_network_change" mapstructure:"challenge_on_network_change"`
	ReauthOnLongStandby       bool   `toml:"reauth_on_long_standby" mapstructure:"reauth_on_long_standby"`
	StandbyThresholdMinutes   int    `toml:"standby_threshold_minutes" mapstructure:"standby_threshold_minutes"`
	LongStandbyThresholdHours int    `toml:"long_standby_threshold_hours" mapstructure:"long_standby_threshold_hours"`
}

// LoggingConfig holds logging configuration.
type LoggingConfig struct {
	Level      string `mapstructure:"level"`
	File       string `mapstructure:"file"`
	MaxSize    int    `mapstructure:"max_size"`
	MaxBackups int    `mapstructure:"max_backups"`
	MaxAge     int    `mapstructure:"max_age"`
	Compress   bool   `mapstructure:"compress"`
}

// DeduplicationConfig holds deduplication configuration.
type DeduplicationConfig struct {
	Enabled   bool   `mapstructure:"enabled"`
	Directory string `mapstructure:"directory"`
	MaxSize   string `mapstructure:"maxsize"`
}

// ISOConfig holds ISO filesystem configuration.
type ISOConfig struct {
	Enabled       bool   `mapstructure:"enabled"`
	MountPoint    string `mapstructure:"mountpoint"`
	Size          string `mapstructure:"size"`
	Charset       string `mapstructure:"charset"`
	ContainerFile string `mapstructure:"containerfile"`
}

// TimeoutConfig holds timeout configuration.
type TimeoutConfig struct {
	Read     string `mapstructure:"readtimeout" toml:"readtimeout"`
	Write    string `mapstructure:"writetimeout" toml:"writetimeout"`
	Idle     string `mapstructure:"idletimeout" toml:"idletimeout"`
	Shutdown string `mapstructure:"shutdown" toml:"shutdown"`
}

// VersioningConfig holds versioning configuration.
type VersioningConfig struct {
	Enabled bool   `mapstructure:"enableversioning" toml:"enableversioning"`
	Backend string `mapstructure:"backend" toml:"backend"`
	MaxRevs int    `mapstructure:"maxversions" toml:"maxversions"`
}

// ClamAVConfig holds ClamAV configuration.
type ClamAVConfig struct {
	ClamAVEnabled      bool     `mapstructure:"clamavenabled"`
	ClamAVSocket       string   `mapstructure:"clamavsocket"`
	NumScanWorkers     int      `mapstructure:"numscanworkers"`
	ScanFileExtensions []string `mapstructure:"scanfileextensions"`
	MaxScanSize        string   `mapstructure:"maxscansize"`
}

// RedisConfig holds Redis configuration.
type RedisConfig struct {
	RedisEnabled             bool   `mapstructure:"redisenabled"`
	RedisDBIndex             int    `mapstructure:"redisdbindex"`
	RedisAddr                string `mapstructure:"redisaddr"`
	RedisPassword            string `mapstructure:"redispassword"`
	RedisHealthCheckInterval string `mapstructure:"redishealthcheckinterval"`
}

// WorkersConfig holds worker pool configuration.
type WorkersConfig struct {
	NumWorkers      int `mapstructure:"numworkers"`
	UploadQueueSize int `mapstructure:"uploadqueuesize"`
}

// FileConfig holds file-specific configuration.
type FileConfig struct{}

// BuildConfig holds build metadata.
type BuildConfig struct {
	Version string `mapstructure:"version"`
}

// NetworkResilienceConfig holds network resilience configuration.
type NetworkResilienceConfig struct {
	FastDetection        bool   `toml:"fast_detection" mapstructure:"fast_detection"`
	QualityMonitoring    bool   `toml:"quality_monitoring" mapstructure:"quality_monitoring"`
	PredictiveSwitching  bool   `toml:"predictive_switching" mapstructure:"predictive_switching"`
	MobileOptimizations  bool   `toml:"mobile_optimizations" mapstructure:"mobile_optimizations"`
	DetectionInterval    string `toml:"detection_interval" mapstructure:"detection_interval"`
	QualityCheckInterval string `toml:"quality_check_interval" mapstructure:"quality_check_interval"`
	MaxDetectionInterval string `toml:"max_detection_interval" mapstructure:"max_detection_interval"`

	MultiInterfaceEnabled       bool     `toml:"multi_interface_enabled" mapstructure:"multi_interface_enabled"`
	InterfacePriority           []string `toml:"interface_priority" mapstructure:"interface_priority"`
	AutoSwitchEnabled           bool     `toml:"auto_switch_enabled" mapstructure:"auto_switch_enabled"`
	SwitchThresholdLatency      string   `toml:"switch_threshold_latency" mapstructure:"switch_threshold_latency"`
	SwitchThresholdPacketLoss   float64  `toml:"switch_threshold_packet_loss" mapstructure:"switch_threshold_packet_loss"`
	QualityDegradationThreshold float64  `toml:"quality_degradation_threshold" mapstructure:"quality_degradation_threshold"`
	MaxSwitchAttempts           int      `toml:"max_switch_attempts" mapstructure:"max_switch_attempts"`
	SwitchDetectionInterval     string   `toml:"switch_detection_interval" mapstructure:"switch_detection_interval"`
}

// ClientNetworkConfigTOML is used for loading from TOML where timeout is a string.
type ClientNetworkConfigTOML struct {
	SessionBasedTracking      bool   `toml:"session_based_tracking" mapstructure:"session_based_tracking"`
	AllowIPChanges            bool   `toml:"allow_ip_changes" mapstructure:"allow_ip_changes"`
	SessionMigrationTimeout   string `toml:"session_migration_timeout" mapstructure:"session_migration_timeout"`
	MaxIPChangesPerSession    int    `toml:"max_ip_changes_per_session" mapstructure:"max_ip_changes_per_session"`
	ClientConnectionDetection bool   `toml:"client_connection_detection" mapstructure:"client_connection_detection"`
	AdaptToClientNetwork      bool   `toml:"adapt_to_client_network" mapstructure:"adapt_to_client_network"`
}

// AuditConfig holds audit logging configuration.
type AuditConfig struct {
	Enabled bool     `toml:"enabled" mapstructure:"enabled"`
	Output  string   `toml:"output" mapstructure:"output"`
	Path    string   `toml:"path" mapstructure:"path"`
	Format  string   `toml:"format" mapstructure:"format"`
	Events  []string `toml:"events" mapstructure:"events"`
	MaxSize int      `toml:"max_size" mapstructure:"max_size"`
	MaxAge  int      `toml:"max_age" mapstructure:"max_age"`
}

// ValidationConfig holds content validation configuration.
type ValidationConfig struct {
	CheckMagicBytes bool     `toml:"check_magic_bytes" mapstructure:"check_magic_bytes"`
	AllowedTypes    []string `toml:"allowed_types" mapstructure:"allowed_types"`
	BlockedTypes    []string `toml:"blocked_types" mapstructure:"blocked_types"`
	MaxFileSize     string   `toml:"max_file_size" mapstructure:"max_file_size"`
	StrictMode      bool     `toml:"strict_mode" mapstructure:"strict_mode"`
}

// QuotaConfig holds per-user quota configuration.
type QuotaConfig struct {
	Enabled  bool              `toml:"enabled" mapstructure:"enabled"`
	Default  string            `toml:"default" mapstructure:"default"`
	Tracking string            `toml:"tracking" mapstructure:"tracking"`
	Custom   map[string]string `toml:"custom" mapstructure:"custom"`
}

// AdminConfig holds admin API configuration.
type AdminConfig struct {
	Enabled    bool            `toml:"enabled" mapstructure:"enabled"`
	Bind       string          `toml:"bind" mapstructure:"bind"`
	PathPrefix string          `toml:"path_prefix" mapstructure:"path_prefix"`
	Auth       AdminAuthConfig `toml:"auth" mapstructure:"auth"`
}

// AdminAuthConfig holds admin authentication configuration.
type AdminAuthConfig struct {
	Type     string `toml:"type" mapstructure:"type"`
	Token    string `toml:"token" mapstructure:"token"`
	Username string `toml:"username" mapstructure:"username"`
	Password string `toml:"password" mapstructure:"password"`
}

// KeyRotationConfig holds HMAC key rotation configuration.
type KeyRotationConfig struct {
	Enabled          bool   `toml:"enabled" mapstructure:"enabled"`
	RotationInterval string `toml:"rotation_interval" mapstructure:"rotation_interval"`
	GracePeriod      string `toml:"grace_period" mapstructure:"grace_period"`
	KeyStoragePath   string `toml:"key_storage" mapstructure:"key_storage"`
}

// RateLimitConfig holds rate limiting configuration.
type RateLimitConfig struct {
	Enabled         bool     `toml:"enabled" mapstructure:"enabled"`
	RequestsPerMin  int      `toml:"requests_per_minute" mapstructure:"requests_per_minute"`
	BurstSize       int      `toml:"burst_size" mapstructure:"burst_size"`
	CleanupInterval string   `toml:"cleanup_interval" mapstructure:"cleanup_interval"`
	ByJID           bool     `toml:"by_jid" mapstructure:"by_jid"`
	ByIP            bool     `toml:"by_ip" mapstructure:"by_ip"`
	WhitelistedIPs  []string `toml:"whitelisted_ips" mapstructure:"whitelisted_ips"`
	WhitelistedJIDs []string `toml:"whitelisted_jids" mapstructure:"whitelisted_jids"`
}

// MetadataStoreConfig holds SQLite metadata store configuration.
type MetadataStoreConfig struct {
	Enabled  bool   `toml:"enabled" mapstructure:"enabled"`
	DBPath   string `toml:"db_path" mapstructure:"db_path"`
	PurgeAge string `toml:"purge_age" mapstructure:"purge_age"` // age after which soft-deleted records are purged
}

// Config is the top-level configuration struct.
type Config struct {
	Server            ServerConfig            `mapstructure:"server"`
	Logging           LoggingConfig           `mapstructure:"logging"`
	Deduplication     DeduplicationConfig     `mapstructure:"deduplication"`
	ISO               ISOConfig               `mapstructure:"iso"`
	Timeouts          TimeoutConfig           `mapstructure:"timeouts"`
	Security          SecurityConfig          `mapstructure:"security"`
	Versioning        VersioningConfig        `mapstructure:"versioning"`
	Uploads           UploadsConfig           `mapstructure:"uploads"`
	Downloads         DownloadsConfig         `mapstructure:"downloads"`
	ClamAV            ClamAVConfig            `mapstructure:"clamav"`
	Redis             RedisConfig             `mapstructure:"redis"`
	Workers           WorkersConfig           `mapstructure:"workers"`
	File              FileConfig              `mapstructure:"file"`
	Build             BuildConfig             `mapstructure:"build"`
	NetworkResilience NetworkResilienceConfig `mapstructure:"network_resilience"`
	ClientNetwork     ClientNetworkConfigTOML `mapstructure:"client_network_support"`
	Audit             AuditConfig             `mapstructure:"audit"`
	Validation        ValidationConfig        `mapstructure:"validation"`
	Quotas            QuotaConfig             `mapstructure:"quotas"`
	Admin             AdminConfig             `mapstructure:"admin"`
	RateLimit         RateLimitConfig         `mapstructure:"rate_limit"`
	KeyRotation       KeyRotationConfig       `mapstructure:"key_rotation"`
	Metadata          MetadataStoreConfig     `mapstructure:"metadata"`
}

// UploadTask represents a queued upload task.
type UploadTask struct {
	AbsFilename string
	Request     *http.Request
	Result      chan error
}

// ScanTask represents a queued virus-scan task.
type ScanTask struct {
	AbsFilename string
	Result      chan error
}

// FileMetadata stores extra file metadata like creation date.
type FileMetadata struct {
	CreationDate time.Time
}
