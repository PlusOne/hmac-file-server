package config

import (
	"fmt"
	"github.com/spf13/viper"
	"log"
	"strconv"
	"strings"
)

// ServerConfig holds server-related configurations
type ServerConfig struct {
	ListenPort           string `mapstructure:"ListenPort"`
	UnixSocket           bool   `mapstructure:"UnixSocket"`
	StoragePath          string `mapstructure:"StoragePath"`
	LogLevel             string `mapstructure:"LogLevel"`
	LogFile              string `mapstructure:"LogFile"`
	MetricsEnabled       bool   `mapstructure:"MetricsEnabled"`
	MetricsPort          string `mapstructure:"MetricsPort"`
	FileTTL              string `mapstructure:"FileTTL"`
	MinFreeBytesStr      string `mapstructure:"MinFreeBytes"` // Changed to string
	MinFreeBytes         int64  `mapstructure:"-"`            // Parsed value
	DeduplicationEnabled bool   `mapstructure:"DeduplicationEnabled"`
	AutoAdjustWorkers    bool   `mapstructure:"AutoAdjustWorkers"`
	NetworkEvents        bool   `mapstructure:"NetworkEvents"`
	LogRateLimiter       bool   `mapstructure:"LogRateLimiter"`
	// Removed duplicate Versioning field
	Uploads    UploadsConfig    `mapstructure:"uploads"`
	ClamAV     ClamAVConfig     `mapstructure:"clamav"`
	Workers    WorkersConfig    `mapstructure:"workers"`
	Storage    StorageConfig    `mapstructure:"storage"`
	Versioning VersioningConfig `mapstructure:"versioning"`
	File       FileConfig       `mapstructure:"file"`
}

// TimeoutConfig holds timeout-related configurations
type TimeoutConfig struct {
	ReadTimeout  string `mapstructure:"ReadTimeout"`
	WriteTimeout string `mapstructure:"WriteTimeout"`
	IdleTimeout  string `mapstructure:"IdleTimeout"`
}

// SecurityConfig holds security-related configurations
type SecurityConfig struct {
	Secret string `mapstructure:"Secret"`
}

// VersioningConfig holds versioning-related configurations
type VersioningConfig struct {
	EnableVersioning bool `mapstructure:"EnableVersioning"`
	MaxVersions      int  `mapstructure:"MaxVersions"`
}

// UploadsConfig holds upload-related configurations
type UploadsConfig struct {
	ResumableUploadsEnabled bool     `mapstructure:"ResumableUploadsEnabled"`
	ChunkedUploadsEnabled   bool     `mapstructure:"ChunkedUploadsEnabled"`
	ChunkSize               string   `mapstructure:"ChunkSize"`
	AllowedExtensions       []string `mapstructure:"AllowedExtensions"`
}

// ClamAVConfig holds ClamAV-related configurations
type ClamAVConfig struct {
	ClamAVEnabled      bool     `mapstructure:"ClamAVEnabled"`
	ClamAVSocket       string   `mapstructure:"ClamAVSocket"`
	NumScanWorkers     int      `mapstructure:"NumScanWorkers"`
	ScanFileExtensions []string `mapstructure:"ScanFileExtensions"`
}

// RedisConfig holds Redis-related configurations
type RedisConfig struct {
	RedisEnabled             bool   `mapstructure:"RedisEnabled"`
	RedisDBIndex             int    `mapstructure:"RedisDBIndex"`
	RedisAddr                string `mapstructure:"RedisAddr"`
	RedisPassword            string `mapstructure:"RedisPassword"`
	RedisHealthCheckInterval string `mapstructure:"RedisHealthCheckInterval"`
}

// WorkersConfig holds worker pool-related configurations
type WorkersConfig struct {
	NumWorkers      int `mapstructure:"NumWorkers"`
	UploadQueueSize int `mapstructure:"UploadQueueSize"`
}

// FileConfig holds file-related configurations
type FileConfig struct {
	FileRevision int `mapstructure:"FileRevision"`
}

// ISOConfig holds ISO-related configurations
type ISOConfig struct {
	Enabled    bool   `mapstructure:"enabled"`
	Size       string `mapstructure:"size"`
	MountPoint string `mapstructure:"mountpoint"`
	Charset    string `mapstructure:"charset"`
}

// StorageConfig holds storage-related configurations
type StorageConfig struct {
	Type  string `mapstructure:"type"`
	Local struct {
		Path string `mapstructure:"path"`
	} `mapstructure:"local"`
	ISO struct {
		MountPoint string `mapstructure:"mountpoint"`
		Size       string `mapstructure:"size"`
		Charset    string `mapstructure:"charset"`
	} `mapstructure:"iso"`
	FTP struct {
		Server   string `mapstructure:"server"`
		Username string `mapstructure:"username"`
		Password string `mapstructure:"password"`
		BasePath string `mapstructure:"basepath"`
	} `mapstructure:"ftp"`
	S3 struct {
		Endpoint  string `mapstructure:"endpoint"`
		AccessKey string `mapstructure:"accesskey"`
		SecretKey string `mapstructure:"secretkey"`
		Bucket    string `mapstructure:"bucket"`
		Region    string `mapstructure:"region"`
	} `mapstructure:"s3"`
	// Add additional storage types and their configurations as needed
}

// Config is the root configuration struct
type Config struct {
	Server     ServerConfig     `mapstructure:"server"`
	Security   SecurityConfig   `mapstructure:"security"`
	Redis      RedisConfig      `mapstructure:"redis"`
	ISO        ISOConfig        `mapstructure:"iso"`
	Timeouts   TimeoutConfig    `mapstructure:"timeouts"`
	Versioning VersioningConfig `mapstructure:"versioning"`
	Uploads    UploadsConfig    `mapstructure:"uploads"`
	ClamAV     ClamAVConfig     `mapstructure:"clamav"`
	Workers    WorkersConfig    `mapstructure:"workers"`
	Storage    StorageConfig    `mapstructure:"storage"`
}

// parseSize parses human-readable size strings (e.g., "100GB") to bytes.
func parseSize(sizeStr string) (int64, error) {
    sizeStr = strings.TrimSpace(sizeStr)
    if sizeStr == "" {
        return 0, fmt.Errorf("size string is empty")
    }

    var multiplier int64 = 1
    if strings.HasSuffix(sizeStr, "KB") {
        multiplier = 1 << 10
        sizeStr = strings.TrimSuffix(sizeStr, "KB")
    } else if strings.HasSuffix(sizeStr, "MB") {
        multiplier = 1 << 20
        sizeStr = strings.TrimSuffix(sizeStr, "MB")
    } else if strings.HasSuffix(sizeStr, "GB") {
        multiplier = 1 << 30
        sizeStr = strings.TrimSuffix(sizeStr, "GB")
    } else if strings.HasSuffix(sizeStr, "TB") {
        multiplier = 1 << 40
        sizeStr = strings.TrimSuffix(sizeStr, "TB")
    }

    value, err := strconv.ParseFloat(strings.TrimSpace(sizeStr), 64)
    if err != nil {
        return 0, fmt.Errorf("invalid size value: %v", err)
    }

    return int64(value * float64(multiplier)), nil
}

// setDefaults sets the default values for the configuration
func setDefaults(conf *Config) {
	// Set default values for ServerConfig
	conf.Server.ListenPort = "8080"
	conf.Server.LogLevel = "info"
	conf.Server.MetricsPort = "9090"
	// Removed recursive call to setDefaults to avoid infinite recursion
	conf.Server.MinFreeBytesStr = "1GB"

	// Set default values for TimeoutConfig
	conf.Timeouts.ReadTimeout = "5s"
	conf.Timeouts.WriteTimeout = "10s"
	conf.Timeouts.IdleTimeout = "120s"

	// Set default values for SecurityConfig
	conf.Security.Secret = "default-secret"

	// Set default values for VersioningConfig
	conf.Versioning.EnableVersioning = false
	conf.Versioning.MaxVersions = 5

	// Set default values for UploadsConfig
	conf.Uploads.ResumableUploadsEnabled = false
	conf.Uploads.ChunkedUploadsEnabled = false
	conf.Uploads.ChunkSize = "10MB"
	conf.Uploads.AllowedExtensions = []string{".txt", ".jpg", ".png"}

	// Set default values for ClamAVConfig
	conf.ClamAV.ClamAVEnabled = false
	conf.ClamAV.ClamAVSocket = "/var/run/clamav/clamd.ctl"
	conf.ClamAV.NumScanWorkers = 5
	conf.ClamAV.ScanFileExtensions = []string{".exe", ".dll"}

	// Set default values for RedisConfig
	conf.Redis.RedisEnabled = false
	conf.Redis.RedisDBIndex = 0
	conf.Redis.RedisAddr = "localhost:6379"
	conf.Redis.RedisPassword = ""
	conf.Redis.RedisHealthCheckInterval = "30s"

	// Set default values for WorkersConfig
	conf.Workers.NumWorkers = 10
	conf.Workers.UploadQueueSize = 100

	// Set default values for StorageConfig
	conf.Storage.Type = "local"
	conf.Storage.Local.Path = "/var/lib/hmac-file-server/storage"
}

// ReadConfig loads the configuration from the given file into conf
func ReadConfig(configFile string, conf *Config) error {
    viper.SetConfigFile(configFile)
    viper.SetConfigType("toml")

    // Set default values
	setDefaults(conf)

    // Read environment variables that match
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

    // Parse human-readable sizes
    minFreeBytes, err := parseSize(conf.Server.MinFreeBytesStr)
    if err != nil {
        return fmt.Errorf("invalid MinFreeBytes: %v", err)
    }
    conf.Server.MinFreeBytes = minFreeBytes

    // Validate the configuration
    if err := validateConfig(conf); err != nil {
        return fmt.Errorf("configuration validation failed: %w", err)
    }

    return nil
}

// validateConfig performs comprehensive checks on the configuration
func validateConfig(conf *Config) error {
	// Server configuration
	if conf.Server.ListenPort == "" {
		return fmt.Errorf("server.ListenPort must be set")
	}
	if conf.Server.LogLevel == "" {
		return fmt.Errorf("server.LogLevel must be set")
	}
	if conf.Server.LogFile == "" {
		return fmt.Errorf("server.LogFile must be set")
	}
	if conf.Server.StoragePath == "" {
		return fmt.Errorf("server.StoragePath must be set")
	}

	// Timeout configuration
	if conf.Timeouts.ReadTimeout == "" {
		return fmt.Errorf("timeouts.ReadTimeout must be set")
	}
	if conf.Timeouts.WriteTimeout == "" {
		return fmt.Errorf("timeouts.WriteTimeout must be set")
	}
	if conf.Timeouts.IdleTimeout == "" {
		return fmt.Errorf("timeouts.IdleTimeout must be set")
	}

	// Security configuration
	if conf.Security.Secret == "" {
		return fmt.Errorf("security.Secret must be set")
	}

	// Versioning configuration
	if conf.Versioning.EnableVersioning && conf.Versioning.MaxVersions <= 0 {
		return fmt.Errorf("versioning.MaxVersions must be greater than 0 when versioning is enabled")
	}

	// Uploads configuration
	if conf.Uploads.ResumableUploadsEnabled && conf.Uploads.ChunkSize == "" {
		return fmt.Errorf("uploads.ChunkSize must be set when resumable uploads are enabled")
	}
	if len(conf.Uploads.AllowedExtensions) == 0 {
		return fmt.Errorf("uploads.AllowedExtensions must have at least one extension")
	}

	// ClamAV configuration
	if conf.ClamAV.ClamAVEnabled && conf.ClamAV.ClamAVSocket == "" {
		return fmt.Errorf("clamav.ClamAVSocket must be set when ClamAV is enabled")
	}

	// Redis configuration
	if conf.Redis.RedisEnabled {
		if conf.Redis.RedisAddr == "" {
			return fmt.Errorf("redis.RedisAddr must be set when Redis is enabled")
		}
		if conf.Redis.RedisHealthCheckInterval == "" {
			return fmt.Errorf("redis.RedisHealthCheckInterval must be set when Redis is enabled")
		}
	}

	// Workers configuration
	if conf.Workers.NumWorkers <= 0 {
		return fmt.Errorf("workers.NumWorkers must be greater than 0")
	}
	if conf.Workers.UploadQueueSize <= 0 {
		return fmt.Errorf("workers.UploadQueueSize must be greater than 0")
	}

	// Storage configuration
	if conf.Storage.Type == "" {
		return fmt.Errorf("storage.Type must be set")
	}
	switch conf.Storage.Type {
	case "local":
		if conf.Storage.Local.Path == "" {
			return fmt.Errorf("storage.local.path must be set for local storage")
		}
	case "s3":
		if conf.Storage.S3.Endpoint == "" {
			return fmt.Errorf("storage.s3.endpoint must be set for S3 storage")
		}
		if conf.Storage.S3.AccessKey == "" {
			return fmt.Errorf("storage.s3.accesskey must be set for S3 storage")
		}
		if conf.Storage.S3.SecretKey == "" {
			return fmt.Errorf("storage.s3.secretkey must be set for S3 storage")
		}
		if conf.Storage.S3.Bucket == "" {
			return fmt.Errorf("storage.s3.bucket must be set for S3 storage")
		}
		if conf.Storage.S3.Region == "" {
			return fmt.Errorf("storage.s3.region must be set for S3 storage")
		}
	// Add additional storage types and their validations as needed
	default:
		return fmt.Errorf("unsupported storage type: %s", conf.Storage.Type)
	}

	return nil
}

// LoadConfig loads the configuration from file and environment
func LoadConfig() (*Config, error) {
    var conf Config
    viper.SetConfigFile("./config.toml")
    viper.SetConfigType("toml")
    viper.AutomaticEnv()
    viper.SetEnvPrefix("HMAC")

    if err := viper.ReadInConfig(); err != nil {
        log.Printf("Error reading config: %v", err)
        return nil, err
    }

    if err := viper.Unmarshal(&conf); err != nil {
        log.Printf("Error unmarshaling config: %v", err)
        return nil, err
    }

    return &conf, nil
}
