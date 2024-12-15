package config

import (
    "github.com/spf13/viper"
)

type ServerConfig struct {
    ListenPort              string `mapstructure:"listenport"`
    StoragePath             string `mapstructure:"storagepath"`
    FileTTL                 string `mapstructure:"filettl"`
    MetricsEnabled          bool   `mapstructure:"metricsenabled"`
    MetricsPort             string `mapstructure:"metricsport"`
    UnixSocket              bool   `mapstructure:"unixsocket"`
    LogFile                 string `mapstructure:"logfile"`
    LogLevel                string `mapstructure:"loglevel"`
    NetworkChangeMonitoring bool   `mapstructure:"NetworkChangeMonitoring"`
    ResumeableUploads       bool   `mapstructure:"ResumeableUploads"`
    ResumeableDownloads     bool   `mapstructure:"ResumeableDownloads"`
    AutoAdjustWorkers       bool   `mapstructure:"AutoAdjustWorkers"`
    DeduplicationEnabled    bool   `mapstructure:"DeduplicationEnabled"`
    MinFreeBytes            string `mapstructure:"minfreebytes"`
    LogLimiter              bool   `mapstructure:"LogLimiter"`
}

type ISOConfig struct {
    Enabled    bool   `mapstructure:"enabled"`
    Size       string `mapstructure:"size"`
    MountPoint string `mapstructure:"mountpoint"`
    Charset    string `mapstructure:"charset"`
}

type TimeoutsConfig struct {
    ReadTimeout  string `mapstructure:"readtimeout"`
    WriteTimeout string `mapstructure:"writetimeout"`
    IdleTimeout  string `mapstructure:"idletimeout"`
}

type WorkersConfig struct {
    UploadQueueSize int `mapstructure:"UploadQueueSize"`
    NumWorkers      int `mapstructure:"NumWorkers"`
    NumScanWorkers  int `mapstructure:"NumScanWorkers"`
}

type ClamAVConfig struct {
    ClamAVEnabled bool   `mapstructure:"clamavenabled"`
    ClamAVSocket  string `mapstructure:"clamavsocket"`
}

type RedisConfig struct {
    RedisEnabled             bool   `mapstructure:"redisenabled"`
    RedisDBIndex             int    `mapstructure:"redisdbindex"`
    RedisAddr                string `mapstructure:"redisaddr"`
    RedisPassword            string `mapstructure:"redispassword"`
    RedisHealthCheckInterval string `mapstructure:"redishealthcheckinterval"`
}

type Config struct {
    Server   ServerConfig   `mapstructure:"server"`
    ISO      ISOConfig      `mapstructure:"iso"`
    Timeouts TimeoutsConfig `mapstructure:"timeouts"`
    Workers  WorkersConfig  `mapstructure:"workers"`
    ClamAV   ClamAVConfig   `mapstructure:"clamav"`
    Redis    RedisConfig    `mapstructure:"redis"`
    File     struct {
        FileRevision int `mapstructure:"filerevision"`
    } `mapstructure:"file"`
}

func LoadConfig(configFile string) (*Config, error) {
    var conf Config
    viper.SetConfigFile(configFile)
    if err := viper.ReadInConfig(); err != nil {
        return nil, err
    }
    if err := viper.Unmarshal(&conf); err != nil {
        return nil, err
    }
    return &conf, nil
}
