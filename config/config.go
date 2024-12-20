package config

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/renz/hmac-file-server/utils" // Corrected import path for utils
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

type ServerConfig struct {
	ListenPort           string `mapstructure:"ListenPort"`
	UnixSocket           bool   `mapstructure:"UnixSocket"`
	StoragePath          string `mapstructure:"StoragePath" json:"storage_path"`
	LogLevel             string `mapstructure:"LogLevel"`
	LogFile              string `mapstructure:"LogFile"`
	MetricsEnabled       bool   `mapstructure:"MetricsEnabled"`
	MetricsPort          string `mapstructure:"MetricsPort"`
	FileTTL              string `mapstructure:"FileTTL"`
	MinFreeBytes         string `mapstructure:"MinFreeBytes" json:"min_free_bytes"`
	DeduplicationEnabled bool   `mapstructure:"DeduplicationEnabled" json:"deduplication_enabled"`
	AutoAdjustWorkers    bool   `mapstructure:"AutoAdjustWorkers"`
	NetworkEvents        bool   `mapstructure:"NetworkEvents"`
	TempPath             string `json:"temp_path"`
	LoggingJSON          bool   `mapstructure:"LoggingJSON"`
	PidFilePath          string `mapstructure:"PidFilePath"`
	CleanupOnExit        bool   `mapstructure:"CleanupOnExit"`
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

type DownloadsConfig struct {
	ResumableDownloadsEnabled bool   `mapstructure:"ResumableDownloadsEnabled"`
	ChunkedDownloadsEnabled   bool   `mapstructure:"ChunkedDownloadsEnabled"`
	ChunkSize                 string `mapstructure:"ChunkSize"`
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
	Downloads  DownloadsConfig  `mapstructure:"downloads"`
	ClamAV     ClamAVConfig     `mapstructure:"clamav"`
	Redis      RedisConfig      `mapstructure:"redis"`
	Workers    WorkersConfig    `mapstructure:"workers"`
	File       FileConfig       `mapstructure:"file"`
	ISO        ISOConfig        `mapstructure:"iso"`
}

// Update LoadConfig to handle day-based durations if utils.ParseDuration is extended
func LoadConfig(configFile string) (*Config, error) {
	viper.SetConfigFile(configFile)
	viper.SetConfigType("toml")

	setDefaults()

	viper.AutomaticEnv()
	viper.SetEnvPrefix("HMAC")
	viper.SetEnvKeyReplacer(strings.NewReplacer(".", "_"))

	if err := viper.ReadInConfig(); err != nil {
		return nil, fmt.Errorf("error reading config: %w", err)
	}

	var conf Config
	if err := viper.Unmarshal(&conf); err != nil {
		return nil, fmt.Errorf("error unmarshaling config: %w", err)
	}

	if err := validateConfig(&conf); err != nil {
		return nil, fmt.Errorf("invalid config: %w", err)
	}

	// Log StoragePath for debug
	logrus.Infof("StoragePath set to: %s", conf.Server.StoragePath)

	return &conf, nil
}

func setDefaults() {
	viper.SetDefault("server.ListenPort", "8080")
	viper.SetDefault("server.UnixSocket", false)
	viper.SetDefault("server.StoragePath", "./uploads")
	viper.SetDefault("server.LogLevel", "info")
	viper.SetDefault("server.LogFile", "")
	viper.SetDefault("server.MetricsEnabled", true)
	viper.SetDefault("server.MetricsPort", "9090")
	viper.SetDefault("server.FileTTL", "365d") // Updated to use 'd' for days
	viper.SetDefault("server.MinFreeBytes", "100MB")
	viper.SetDefault("server.DeduplicationEnabled", true)
	viper.SetDefault("server.AutoAdjustWorkers", true)
	viper.SetDefault("server.NetworkEvents", false)
	viper.SetDefault("server.LoggingJSON", false)
	viper.SetDefault("server.PidFilePath", "./hmac_server.pid")
	viper.SetDefault("server.CleanupOnExit", true)

	viper.SetDefault("timeouts.ReadTimeout", "4800s")
	viper.SetDefault("timeouts.WriteTimeout", "4800s")
	viper.SetDefault("timeouts.IdleTimeout", "65s")

	viper.SetDefault("security.Secret", "changeme")

	viper.SetDefault("versioning.EnableVersioning", false)
	viper.SetDefault("versioning.MaxVersions", 1)

	viper.SetDefault("uploads.ResumableUploadsEnabled", false)
	viper.SetDefault("uploads.ChunkedUploadsEnabled", true)
	viper.SetDefault("uploads.ChunkSize", "8192")
	viper.SetDefault("uploads.AllowedExtensions", []string{
		".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp",
		".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v",
		".3gp", ".3g2", ".mp3", ".ogg",
	})

	viper.SetDefault("clamav.ClamAVEnabled", false)
	viper.SetDefault("clamav.ClamAVSocket", "/var/run/clamav/clamd.ctl")
	viper.SetDefault("clamav.NumScanWorkers", 4)
	viper.SetDefault("clamav.ScanFileExtensions", []string{".exe", ".dll", ".js", ".php", ".scr", ".bat"})

	viper.SetDefault("redis.RedisEnabled", true)
	viper.SetDefault("redis.RedisAddr", "localhost:6379")
	viper.SetDefault("redis.RedisPassword", "")
	viper.SetDefault("redis.RedisDBIndex", 0)
	viper.SetDefault("redis.RedisHealthCheckInterval", "120s")

	viper.SetDefault("workers.NumWorkers", 4)
	viper.SetDefault("workers.UploadQueueSize", 50)

	viper.SetDefault("iso.Enabled", false)
	viper.SetDefault("iso.Size", "2TB")
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

	// Replace direct usage of time.ParseDuration with utils.ParseDuration
	if _, err := utils.ParseDuration(conf.Timeouts.ReadTimeout); err != nil {
		return fmt.Errorf("invalid ReadTimeout: %w", err)
	}

	if _, err := utils.ParseDuration(conf.Timeouts.WriteTimeout); err != nil {
		return fmt.Errorf("invalid WriteTimeout: %w", err)
	}

	if _, err := utils.ParseDuration(conf.Timeouts.IdleTimeout); err != nil {
		return fmt.Errorf("invalid IdleTimeout: %w", err)
	}

	// Validate FileTTL
	fileTTL, err := utils.ParseDuration(conf.Server.FileTTL)
	if err != nil {
		return fmt.Errorf("invalid FileTTL: %w", err)
	}
	if fileTTL <= 0 {
		return fmt.Errorf("FileTTL must be positive")
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
		if _, err := utils.ParseSize(conf.ISO.Size); err != nil {
			return fmt.Errorf("invalid ISO size '%s': %w", conf.ISO.Size, err)
		}
		if conf.ISO.MountPoint == "" {
			return fmt.Errorf("ISO mount point must be set")
		}
		if conf.ISO.Charset == "" {
			return fmt.Errorf("ISO charset must be set")
		}
	}

	if conf.ClamAV.ClamAVEnabled && conf.ClamAV.ClamAVSocket == "" {
		return fmt.Errorf("ClamAV is enabled but ClamAVSocket is not set")
	}

	fileInfo, err := os.Stat(conf.Server.StoragePath)
	if os.IsNotExist(err) {
		if err := os.MkdirAll(conf.Server.StoragePath, 0755); err != nil {
			return fmt.Errorf("failed to create StoragePath: %w", err)
		}
		logrus.Infof("Created StoragePath: %s", conf.Server.StoragePath)
		fileInfo, err = os.Stat(conf.Server.StoragePath) // Re-check after creating the directory
		if err != nil {
			return fmt.Errorf("error accessing StoragePath: %w", err)
		}
	} else if err != nil {
		return fmt.Errorf("error accessing StoragePath: %w", err)
	}
	if !fileInfo.IsDir() {
		return fmt.Errorf("StoragePath is not a directory: %s", conf.Server.StoragePath)
	}

	tempFilePath := filepath.Join(conf.Server.StoragePath, ".perm_test")
	file, err := os.Create(tempFilePath)
	if err != nil {
		return fmt.Errorf("no write permission for StoragePath: %s", conf.Server.StoragePath)
	}
	file.Close()
	if err := os.Remove(tempFilePath); err != nil {
		return fmt.Errorf("no delete permission for StoragePath: %s", conf.Server.StoragePath)
	}

	// Validate Uploads ChunkSize
	if _, err := utils.ParseSize(conf.Uploads.ChunkSize); err != nil {
		return fmt.Errorf("invalid Uploads.ChunkSize '%s': %w", conf.Uploads.ChunkSize, err)
	}

	// Validate Downloads ChunkSize
	if conf.Downloads.ChunkedDownloadsEnabled {
		if _, err := utils.ParseSize(conf.Downloads.ChunkSize); err != nil {
			return fmt.Errorf("invalid Downloads.ChunkSize '%s': %w", conf.Downloads.ChunkSize, err)
		}
	}

	// DeduplicationEnabled is a boolean; no additional validation needed

	// ...additional validations if necessary...

	return nil
}

// You may need to update utils.ParseDuration to handle 'd' for days
