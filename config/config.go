package config

import (
	"fmt"
	"time"

	"github.com/spf13/viper"
)

type ServerConfig struct {
	ListenPort              string `mapstructure:"ListenPort"`
	StoragePath             string `mapstructure:"StoragePath"`
	FileTTL                 string `mapstructure:"FileTTL"`
	MetricsEnabled          bool   `mapstructure:"MetricsEnabled"`
	MetricsPort             string `mapstructure:"MetricsPort"`
	UnixSocket              bool   `mapstructure:"UnixSocket"`
	LogFile                 string `mapstructure:"LogFile"`
	LogLevel                string `mapstructure:"LogLevel"`
	DeduplicationEnabled    bool   `mapstructure:"DeduplicationEnabled"`
	NetworkChangeMonitoring bool   `mapstructure:"NetworkChangeMonitoring"`
	AutoAdjustWorkers       bool   `mapstructure:"AutoAdjustWorkers"`
	LogLimiter              bool   `mapstructure:"LogLimiter"`
}

type TimeoutConfig struct {
	ReadTimeout  string `mapstructure:"ReadTimeout"`
	WriteTimeout string `mapstructure:"WriteTimeout"`
	IdleTimeout  string `mapstructure:"IdleTimeout"`
}

type SecurityConfig struct {
	Secret string `mapstructure:"secret"`
}

type ISOConfig struct {
	Enabled bool `mapstructure:"enabled"`
}

type WorkersConfig struct {
	UploadQueueSize int `mapstructure:"upload_queue_size"`
	NumWorkers      int `mapstructure:"num_workers"`
	NumScanWorkers  int `mapstructure:"num_scan_workers"`
}

type ClamAVConfig struct {
	ClamAVEnabled bool   `mapstructure:"clamav_enabled"`
	ClamAVSocket  string `mapstructure:"clamav_socket"`
}

type RedisConfig struct {
	RedisEnabled             bool   `mapstructure:"redis_enabled"`
	RedisAddr                string `mapstructure:"redis_addr"`
	RedisPassword            string `mapstructure:"redis_password"`
	RedisDBIndex             int    `mapstructure:"redis_db_index"`
	RedisHealthCheckInterval string `mapstructure:"redis_health_check_interval"`
}

type Config struct {
	Server   ServerConfig   `mapstructure:"server"`
	Timeouts TimeoutConfig  `mapstructure:"timeouts"`
	Workers  WorkersConfig  `mapstructure:"workers"`
	ClamAV   ClamAVConfig   `mapstructure:"clamav"`
	Redis    RedisConfig    `mapstructure:"redis"`
	Security SecurityConfig `mapstructure:"security"`
	ISO      ISOConfig      `mapstructure:"iso"`
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
