package config

import (
	"fmt"
	"time"

	"github.com/spf13/viper"
)

type Config struct {
	Server     ServerConfig     `mapstructure:"server"`
	ISO        ISOConfig        `mapstructure:"iso"`
	Timeouts   TimeoutConfig    `mapstructure:"timeouts"`
	Security   SecurityConfig   `mapstructure:"security"`
	Versioning VersioningConfig `mapstructure:"versioning"`
	Uploads    UploadsConfig    `mapstructure:"uploads"`
	Downloads  DownloadsConfig  `mapstructure:"downloads"`
	ClamAV     ClamAVConfig     `mapstructure:"clamav"`
	Redis      RedisConfig      `mapstructure:"redis"`
	Workers    WorkersConfig    `mapstructure:"workers"`
	File       FileConfig       `mapstructure:"file"`
}

type ServerConfig struct {
	ListenPort              string `mapstructure:"listenport"`
	UnixSocket              bool   `mapstructure:"unixsocket"`
	StoragePath             string `mapstructure:"storagepath"`
	LogLevel                string `mapstructure:"loglevel"`
	LogFile                 string `mapstructure:"logfile"`
	MetricsEnabled          bool   `mapstructure:"metricsenabled"`
	MetricsPort             string `mapstructure:"metricsport"`
	DeduplicationEnabled    bool   `mapstructure:"deduplicationenabled"`
	NetworkChangeMonitoring bool   `mapstructure:"networkchangemonitoring"`
	AutoAdjustWorkers       bool   `mapstructure:"autoadjustworkers"`
	ResumableUploads        bool   `mapstructure:"resumableuploads"`
	ResumableDownloads      bool   `mapstructure:"resumabledownloads"`
	LogLimiter              bool   `mapstructure:"loglimiter"`
	FileTTL                 string `mapstructure:"filettl"` // Add this line
}

type ISOConfig struct {
	Enabled    bool   `mapstructure:"enabled"`
	Size       string `mapstructure:"size"`
	Mountpoint string `mapstructure:"mountpoint"`
	Charset    string `mapstructure:"charset"`
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
	ResumableUploadsEnabled bool   `mapstructure:"resumableuploadsenabled"`
	ChunkedUploadsEnabled   bool   `mapstructure:"chunkeduploadsenabled"`
	ChunkSize               string `mapstructure:"chunksize"`
}

type DownloadsConfig struct {
	ResumableDownloadsEnabled bool   `mapstructure:"resumabledownloadsenabled"`
	ChunkedDownloadsEnabled   bool   `mapstructure:"chunkeddownloadsenabled"`
	ChunkSize                 string `mapstructure:"chunksize"`
}

type ClamAVConfig struct {
	ClamAVEnabled      bool     `mapstructure:"clamavenabled"`
	ClamAVSocket       string   `mapstructure:"clamavsocket"`
	NumScanWorkers     int      `mapstructure:"numscanworkers"`
	ScanFileExtensions []string `mapstructure:"scanfileextensions"`
}

type RedisConfig struct {
	RedisEnabled             bool   `mapstructure:"redisenabled"`
	RedisAddr                string `mapstructure:"redisaddr"`
	RedisPassword            string `mapstructure:"redispassword"`
	RedisDBIndex             int    `mapstructure:"redisdbindex"`
	RedisHealthCheckInterval string `mapstructure:"redishealthcheckinterval"`
}

type WorkersConfig struct {
	UploadQueueSize int `mapstructure:"uploadqueuesize"`
	NumWorkers      int `mapstructure:"numworkers"`
	NumScanWorkers  int `mapstructure:"numscanworkers"`
}

type FileConfig struct {
	FileRevision int `mapstructure:"filerevision"`
}

func LoadConfig(configFile string) (*Config, error) {
	viper.SetConfigFile(configFile)
	viper.SetConfigType("toml")
	viper.AutomaticEnv()

	viper.SetDefault("server.metricsport", "2112")

	var config Config
	if err := viper.ReadInConfig(); err != nil {
		return nil, err
	}

	err := viper.Unmarshal(&config)
	if err != nil {
		return nil, err
	}

	if err := validateConfig(&config); err != nil {
		return nil, err
	}

	return &config, nil
}

func validateConfig(conf *Config) error {
	// Check for required configuration fields
	if conf.Server.ListenPort == "" {
		return fmt.Errorf("server listen port is not configured")
	}
	if conf.Server.MetricsEnabled && conf.Server.MetricsPort == "" {
		return fmt.Errorf("metrics port is not configured")
	}
	if conf.Server.MetricsPort == conf.Server.ListenPort {
		return fmt.Errorf("metrics port and server port cannot be the same")
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
