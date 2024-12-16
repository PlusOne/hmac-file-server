package config

import (
	"fmt"
	"time"

	"github.com/spf13/viper"
)

type Config struct {
	Server   ServerConfig
	ISO      ISOConfig
	Timeouts TimeoutConfig
	Workers  WorkersConfig
	ClamAV   ClamAVConfig
	Security SecurityConfig
	Redis    RedisConfig

	Versioning struct {
		EnableVersioning bool
		MaxVersions      int
	} `toml:"versioning"`

	Uploads struct {
		ResumableUploadsEnabled bool
		ChunkedUploadsEnabled   bool
		ChunkSize               string
	} `toml:"uploads"`
}

type ServerConfig struct {
	ListenPort              string
	UnixSocket              bool
	StoragePath             string
	LogLevel                string
	LogFile                 string
	MetricsEnabled          bool
	MetricsPort             string
	DeduplicationEnabled    bool
	FileTTL                 string
	MinFreeBytes            string
	NetworkChangeMonitoring bool
	AutoAdjustWorkers       bool
	ResumableUploads        bool
	ResumableDownloads      bool
	LogLimiter              bool
}

type ISOConfig struct {
	Enabled    bool
	Size       string
	Mountpoint string
	Charset    string
}

type TimeoutConfig struct {
	ReadTimeout  string
	WriteTimeout string
	IdleTimeout  string
}

type WorkersConfig struct {
	UploadQueueSize int
	NumWorkers      int
	NumScanWorkers  int `toml:"workers"`
}

type ClamAVConfig struct {
	ClamAVEnabled       bool
	ClamAVSocket        string
	NumScanWorkers      int
	ScanFileExtensions  []string `toml:"clamav"`
}

type RedisConfig struct {
	RedisEnabled             bool
	RedisAddr                string
	RedisPassword            string
	RedisDBIndex             int
	RedisHealthCheckInterval string
}

type SecurityConfig struct {
	Secret string `mapstructure:"secret_key"`
}

func LoadConfig(configFile string) (*Config, error) {
	viper.SetConfigFile(configFile)
	viper.SetConfigType("toml")
	viper.AutomaticEnv()

	var config Config
	if err := viper.ReadInConfig(); err != nil {
		return nil, err
	}

	err := viper.Unmarshal(&config)
	if err != nil {
		return nil, err
	}

	return &config, nil
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

	// Validate timeouts
	if _, err := time.ParseDuration(conf.Timeouts.ReadTimeout); err != nil {
		return fmt.Errorf("invalid ReadTimeout: %v", err)
	}
	if _, err := time.ParseDuration(conf.Timeouts.WriteTimeout); err != nil {
		return fmt.Errorf("invalid WriteTimeout: %v", err)
	}
	if _, err := time.ParseDuration(conf.Timeouts.IdleTimeout); err != nil {
		return fmt.Errorf("invalid IdleTimeout: %v", err)
	}

	return nil
}
