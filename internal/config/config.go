package config

import (
	"github.com/spf13/viper"
)

type Config struct {
	Server struct {
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
		MinFreeByte          string `mapstructure:"MinFreeByte"`
		AutoAdjustWorkers    bool   `mapstructure:"AutoAdjustWorkers"`
		NetworkEvents        bool   `mapstructure:"NetworkEvents"`
		LogRateLimiter       bool   `mapstructure:"LogRateLimiter"`
		StorageType          string `mapstructure:"StorageType"`
	} `mapstructure:"server"`
	Timeouts struct {
		ReadTimeout  string `mapstructure:"ReadTimeout"`
		WriteTimeout string `mapstructure:"WriteTimeout"`
		IdleTimeout  string `mapstructure:"IdleTimeout"`
	} `mapstructure:"timeouts"`
	Security struct {
		Secret string `mapstructure:"Secret"`
	} `mapstructure:"security"`
	Versioning struct {
		EnableVersioning bool `mapstructure:"EnableVersioning"`
		MaxVersions      int  `mapstructure:"MaxVersions"`
	} `mapstructure:"versioning"`
	Uploads struct {
		ResumableUploadsEnabled bool     `mapstructure:"ResumableUploadsEnabled"`
		ChunkedUploadsEnabled   bool     `mapstructure:"ChunkedUploadsEnabled"`
		ChunkSize               string   `mapstructure:"ChunkSize"`
		AllowedExtensions       []string `mapstructure:"AllowedExtensions"`
	} `mapstructure:"uploads"`
	ClamAV struct {
		ClamAVEnabled      bool     `mapstructure:"ClamAVEnabled"`
		ClamAVSocket       string   `mapstructure:"ClamAVSocket"`
		NumScanWorkers     int      `mapstructure:"NumScanWorkers"`
		ScanFileExtensions []string `mapstructure:"ScanFileExtensions"`
	} `mapstructure:"clamav"`
	Redis struct {
		RedisEnabled             bool   `mapstructure:"RedisEnabled"`
		RedisDBIndex             int    `mapstructure:"RedisDBIndex"`
		RedisAddr                string `mapstructure:"RedisAddr"`
		RedisPassword            string `mapstructure:"RedisPassword"`
		RedisHealthCheckInterval string `mapstructure:"RedisHealthCheckInterval"`
	} `mapstructure:"redis"`
	Workers struct {
		NumWorkers      int `mapstructure:"NumWorkers"`
		UploadQueueSize int `mapstructure:"UploadQueueSize"`
	} `mapstructure:"workers"`
	File struct {
		FileRevision int `mapstructure:"FileRevision"`
	} `mapstructure:"file"`
	ISO struct {
		Enabled    bool   `mapstructure:"enabled"`
		Size       string `mapstructure:"size"`
		MountPoint string `mapstructure:"mountpoint"`
		Charset    string `mapstructure:"charset"`
	} `mapstructure:"iso"`
	FTP struct {
		Server   string `mapstructure:"server"`
		Username string `mapstructure:"username"`
		Password string `mapstructure:"password"`
		BasePath string `mapstructure:"basePath"`
	} `mapstructure:"ftp"`
	S3 struct {
		Endpoint  string `mapstructure:"endpoint"`
		AccessKey string `mapstructure:"accessKey"`
		SecretKey string `mapstructure:"secretKey"`
		Bucket    string `mapstructure:"bucket"`
		Region    string `mapstructure:"region"`
	} `mapstructure:"s3"`
}

func ReadConfig(configFile string) (*Config, error) {
	viper.SetConfigFile(configFile)
	viper.SetConfigType("toml")

	viper.AutomaticEnv()
	viper.SetEnvPrefix("HMAC")

	if err := viper.ReadInConfig(); err != nil {
		return nil, err
	}

	var conf Config
	if err := viper.Unmarshal(&conf); err != nil {
		return nil, err
	}

	return &conf, nil
}
