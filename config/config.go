package config

import (
	"fmt"

	"github.com/spf13/viper"
)

// Configuration structures...

type ServerConfig struct {
	ListenPort     string `mapstructure:"ListenPort"`
	StoragePath    string `mapstructure:"StoragePath"`
	FileTTL        string `mapstructure:"FileTTL"`
	MetricsEnabled bool   `mapstructure:"MetricsEnabled"`
	MetricsPort    string `mapstructure:"MetricsPort"`
	UnixSocket     bool   `mapstructure:"UnixSocket"`
}

type ISOConfig struct {
	Enabled bool `mapstructure:"Enabled"`
}

type TimeoutConfig struct {
	ReadTimeout  string `mapstructure:"ReadTimeout"`
	WriteTimeout string `mapstructure:"WriteTimeout"`
	IdleTimeout  string `mapstructure:"IdleTimeout"`
}

type SecurityConfig struct {
	// Define security configuration fields
}

type VersioningConfig struct {
	// Define versioning configuration fields
}

type UploadsConfig struct {
	// Define uploads configuration fields
}

type ClamAVConfig struct {
	ClamAVEnabled bool   `mapstructure:"ClamAVEnabled"`
	ClamAVSocket  string `mapstructure:"ClamAVSocket"`
}

type RedisConfig struct {
	RedisEnabled             bool   `mapstructure:"RedisEnabled"`
	RedisHealthCheckInterval string `mapstructure:"RedisHealthCheckInterval"`
}

type WorkersConfig struct {
	NumWorkers      int `mapstructure:"NumWorkers"`
	UploadQueueSize int `mapstructure:"UploadQueueSize"`
	ScanQueueSize   int
}

type Config struct {
	Server     ServerConfig     `mapstructure:"server"`
	ISO        ISOConfig        `mapstructure:"iso"`
	Timeouts   TimeoutConfig    `mapstructure:"timeouts"`
	Security   SecurityConfig   `mapstructure:"security"`
	Versioning VersioningConfig `mapstructure:"versioning"`
	Uploads    UploadsConfig    `mapstructure:"uploads"`
	ClamAV     ClamAVConfig     `mapstructure:"clamav"`
	Redis      RedisConfig      `mapstructure:"redis"`
	Workers    WorkersConfig    `mapstructure:"workers"`
}

var Conf Config

func LoadConfig(configFile string) error {
	viper.SetConfigFile(configFile)
	viper.SetConfigType("toml")
	viper.AutomaticEnv()
	viper.SetEnvPrefix("HMAC")

	// Set defaults
	setDefaults()

	if err := viper.ReadInConfig(); err != nil {
		return fmt.Errorf("error reading config file: %w", err)
	}

	if err := viper.Unmarshal(&Conf); err != nil {
		return fmt.Errorf("unable to decode into struct: %w", err)
	}

	// Validate configuration
	if err := validateConfig(&Conf); err != nil {
		return fmt.Errorf("configuration validation failed: %w", err)
	}

	return nil
}

func setDefaults() {
	// Set default values as in main.go
	// ...
}

func validateConfig(_ *Config) error {
	// Validation logic as in main.go
	// ...
	return nil
}
