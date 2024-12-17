package main

import (
    "github.com/spf13/viper"
    "fmt"
    "time"
)

// ServerConfig definiert die Server-bezogenen Konfigurationsoptionen
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
}

// TimeoutConfig definiert die Read/Write/Idle-Timeouts
type TimeoutConfig struct {
    ReadTimeout  string `mapstructure:"ReadTimeout"`
    WriteTimeout string `mapstructure:"WriteTimeout"`
    IdleTimeout  string `mapstructure:"IdleTimeout"`
}

// SecurityConfig für Sicherheitskonfigurationen
type SecurityConfig struct {
    Secret string `mapstructure:"Secret"`
}

// RedisConfig für Redis-Verbindungseinstellungen
type RedisConfig struct {
    RedisEnabled             bool   `mapstructure:"RedisEnabled"`
    RedisAddr                string `mapstructure:"RedisAddr"`
    RedisPassword            string `mapstructure:"RedisPassword"`
    RedisDBIndex             int    `mapstructure:"RedisDBIndex"`
    RedisHealthCheckInterval string `mapstructure:"RedisHealthCheckInterval"`
}

// ClamAVConfig für ClamAV-Einstellungen
type ClamAVConfig struct {
    ClamAVEnabled      bool     `mapstructure:"ClamAVEnabled"`
    ClamAVSocket       string   `mapstructure:"ClamAVSocket"`
    NumScanWorkers     int      `mapstructure:"NumScanWorkers"`
    ScanFileExtensions []string `mapstructure:"ScanFileExtensions"`
}

// WorkersConfig definiert Worker-Einstellungen
type WorkersConfig struct {
    NumWorkers      int `mapstructure:"NumWorkers"`
    UploadQueueSize int `mapstructure:"UploadQueueSize"`
}

// Hauptkonfiguration
type Config struct {
    Server   ServerConfig   `mapstructure:"server"`
    Timeouts TimeoutConfig  `mapstructure:"timeouts"`
    Security SecurityConfig `mapstructure:"security"`
    Redis    RedisConfig    `mapstructure:"redis"`
    ClamAV   ClamAVConfig   `mapstructure:"clamav"`
    Workers  WorkersConfig  `mapstructure:"workers"`
}

func readConfig(configFilename string, conf *Config) error {
    viper.SetConfigFile(configFilename)
    viper.SetConfigType("toml")

    viper.AutomaticEnv()
    viper.SetEnvPrefix("HMAC")

    if err := viper.ReadInConfig(); err != nil {
        return fmt.Errorf("error reading config file: %w", err)
    }

    if err := viper.Unmarshal(conf); err != nil {
        return fmt.Errorf("unable to decode into struct: %w", err)
    }

    if err := validateConfig(conf); err != nil {
        return fmt.Errorf("configuration validation failed: %w", err)
    }

    return nil
}

func setDefaults() {
    viper.SetDefault("server.ListenPort", "8080")
    viper.SetDefault("server.UnixSocket", false)
    viper.SetDefault("server.StoragePath", "./uploads")
    viper.SetDefault("server.LogLevel", "info")
    viper.SetDefault("server.MetricsEnabled", true)
    viper.SetDefault("server.MetricsPort", "9090")
    viper.SetDefault("server.FileTTL", "8760h")
    viper.SetDefault("server.MinFreeBytes", "100MB")
    viper.SetDefault("server.AutoAdjustWorkers", true)

    viper.SetDefault("timeouts.ReadTimeout", "30s")
    viper.SetDefault("timeouts.WriteTimeout", "30s")
    viper.SetDefault("timeouts.IdleTimeout", "30s")

    viper.SetDefault("security.Secret", "changeme")

    viper.SetDefault("redis.RedisEnabled", true)
    viper.SetDefault("redis.RedisAddr", "localhost:6379")
    viper.SetDefault("redis.RedisPassword", "")
    viper.SetDefault("redis.RedisDBIndex", 0)
    viper.SetDefault("redis.RedisHealthCheckInterval", "120s")

    viper.SetDefault("clamav.ClamAVEnabled", true)
    viper.SetDefault("clamav.ClamAVSocket", "/var/run/clamav/clamd.ctl")
    viper.SetDefault("clamav.NumScanWorkers", 2)
    viper.SetDefault("clamav.ScanFileExtensions", []string{".txt", ".pdf", ".png", ".jpg"})

    viper.SetDefault("workers.NumWorkers", 4)
    viper.SetDefault("workers.UploadQueueSize", 50)
}

func validateConfig(conf *Config) error {
    if conf.Server.ListenPort == "" {
        return fmt.Errorf("ListenPort must be set")
    }
    if conf.Security.Secret == "" {
        return fmt.Errorf("Secret must be set")
    }
    if conf.Server.StoragePath == "" {
        return fmt.Errorf("StoragePath must be set")
    }
    if _, err := time.ParseDuration(conf.Timeouts.ReadTimeout); err != nil {
        return fmt.Errorf("invalid ReadTimeout: %v", err)
    }
    if _, err := time.ParseDuration(conf.Timeouts.WriteTimeout); err != nil {
        return fmt.Errorf("invalid WriteTimeout: %v", err)
    }
    return nil
}
