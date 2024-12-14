package config

import (
	"fmt"

	"github.com/spf13/viper"
)

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
	LogRateLimiter       bool   `mapstructure:"LogRateLimiter"`
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

func readConfig(configFilename string, conf *Config) error {
	viper.SetConfigFile(configFilename)
	viper.SetConfigType("toml")

	viper.AutomaticEnv()
	viper.SetEnvPrefix("HMAC")

	// Load configuration file
	if err := viper.ReadInConfig(); err != nil {
		return fmt.Errorf("error reading config file: %w", err)
	}

	// Unmarshal configuration into the struct
	if err := viper.Unmarshal(conf); err != nil {
		return fmt.Errorf("unable to decode into struct: %w", err)
	}

	// Additional validation
	if err := validateConfig(conf); err != nil {
		return fmt.Errorf("configuration validation failed: %w", err)
	}

	return nil
}

// validateConfig performs basic checks on the configuration
func validateConfig(conf *Config) error {
	if conf.Server.ListenPort == "" {
		return fmt.Errorf("server.ListenPort must be set")
	}
	if conf.Security.Secret == "" {
		return fmt.Errorf("security.Secret must be set")
	}
	if conf.Server.StoragePath == "" {
		return fmt.Errorf("server.StoragePath must be set")
	}
	if conf.Timeouts.ReadTimeout == "" || conf.Timeouts.WriteTimeout == "" || conf.Timeouts.IdleTimeout == "" {
		return fmt.Errorf("timeouts must be fully configured (ReadTimeout, WriteTimeout, IdleTimeout)")
	}
	return nil
}
