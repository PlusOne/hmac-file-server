// internal/config/config.go

package config

import (
	"fmt"
	"time"

	"github.com/spf13/viper"
	"github.com/sirupsen/logrus"
)

// ServerConfig enthält die Server-spezifischen Konfigurationen
type ServerConfig struct {
	ListenPort           string `mapstructure:"ListenPort"`
	UnixSocket           bool   `mapstructure:"UnixSocket"`
	StoragePath          string `mapstructure:"StoragePath"`
	LogLevel             string `mapstructure:"LogLevel"`
	LogFile              string `mapstructure:"LogFile"`
	MetricsEnabled       bool   `mapstructure:"MetricsEnabled"`
	MetricsPort          string `mapstructure:"MetricsPort"`
	FileTTL              string `mapstructure:"FileTTL"`
	MinFreeBytes         int64  `mapstructure:"MinFreeBytes"` // Mindestfreier Speicherplatz in Bytes
	DeduplicationEnabled bool   `mapstructure:"DeduplicationEnabled"`
}

// TimeoutConfig enthält die Timeout-spezifischen Konfigurationen
type TimeoutConfig struct {
	ReadTimeout  string `mapstructure:"ReadTimeout"`
	WriteTimeout string `mapstructure:"WriteTimeout"`
	IdleTimeout  string `mapstructure:"IdleTimeout"`
}

// SecurityConfig enthält sicherheitsrelevante Konfigurationen
type SecurityConfig struct {
	Secret string `mapstructure:"Secret"`
}

// VersioningConfig enthält Konfigurationen zur Dateiversionierung
type VersioningConfig struct {
	EnableVersioning bool `mapstructure:"EnableVersioning"`
	MaxVersions      int  `mapstructure:"MaxVersions"`
}

// UploadsConfig enthält Konfigurationen für Datei-Uploads
type UploadsConfig struct {
	ResumableUploadsEnabled bool     `mapstructure:"ResumableUploadsEnabled"`
	ChunkedUploadsEnabled   bool     `mapstructure:"ChunkedUploadsEnabled"`
	ChunkSize               int64    `mapstructure:"ChunkSize"`
	AllowedExtensions       []string `mapstructure:"AllowedExtensions"`
}

// ClamAVConfig enthält Konfigurationen für ClamAV
type ClamAVConfig struct {
	ClamAVEnabled  bool   `mapstructure:"ClamAVEnabled"`
	ClamAVSocket   string `mapstructure:"ClamAVSocket"`
	NumScanWorkers int    `mapstructure:"NumScanWorkers"`
}

// RedisConfig enthält Konfigurationen für Redis
type RedisConfig struct {
	RedisEnabled             bool   `mapstructure:"RedisEnabled"`
	RedisDBIndex             int    `mapstructure:"RedisDBIndex"`
	RedisAddr                string `mapstructure:"RedisAddr"`
	RedisPassword            string `mapstructure:"RedisPassword"`
	RedisHealthCheckInterval string `mapstructure:"RedisHealthCheckInterval"`
}

// WorkersConfig enthält Konfigurationen für Worker-Pools
type WorkersConfig struct {
	NumWorkers      int `mapstructure:"NumWorkers"`
	UploadQueueSize int `mapstructure:"UploadQueueSize"`
}

// FileConfig enthält allgemeine Datei-spezifische Konfigurationen
type FileConfig struct {
	FileRevision int `mapstructure:"FileRevision"`
}

// ISOConfig enthält Konfigurationen für ISO-Einstellungen
type ISOConfig struct {
	Enabled    bool   `mapstructure:"enabled"`
	Size       string `mapstructure:"size"`
	MountPoint string `mapstructure:"mountpoint"`
	Charset    string `mapstructure:"charset"`
}

// Config ist die Hauptkonfigurationsstruktur, die alle Unterkonfigurationen beinhaltet
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

var Conf Config
var VersionString string = "v2.0-dev"

// SetDefaults setzt Standardwerte für die Konfiguration
func SetDefaults() {
	// Server-Defaults
	viper.SetDefault("server.ListenPort", "8080")
	viper.SetDefault("server.UnixSocket", false)
	viper.SetDefault("server.StoragePath", "./uploads")
	viper.SetDefault("server.LogLevel", "info")
	viper.SetDefault("server.LogFile", "")
	viper.SetDefault("server.MetricsEnabled", true)
	viper.SetDefault("server.MetricsPort", "9090")
	viper.SetDefault("server.FileTTL", "8760h")      // 365 Tage -> 8760 Stunden
	viper.SetDefault("server.MinFreeBytes", 100<<20) // 100 MB

	// Timeout-Defaults
	viper.SetDefault("timeouts.ReadTimeout", "4800s") // unterstützt 's'
	viper.SetDefault("timeouts.WriteTimeout", "4800s")
	viper.SetDefault("timeouts.IdleTimeout", "4800s")

	// Security-Defaults
	viper.SetDefault("security.Secret", "changeme")

	// Versioning-Defaults
	viper.SetDefault("versioning.EnableVersioning", false)
	viper.SetDefault("versioning.MaxVersions", 1)

	// Uploads-Defaults
	viper.SetDefault("uploads.ResumableUploadsEnabled", true)
	viper.SetDefault("uploads.ChunkedUploadsEnabled", true)
	viper.SetDefault("uploads.ChunkSize", 8192)
	viper.SetDefault("uploads.AllowedExtensions", []string{
		".txt", ".pdf",
		".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp",
		".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2",
		".mp3", ".ogg",
	})

	// ClamAV-Defaults
	viper.SetDefault("clamav.ClamAVEnabled", true)
	viper.SetDefault("clamav.ClamAVSocket", "/var/run/clamav/clamd.ctl")
	viper.SetDefault("clamav.NumScanWorkers", 2)

	// Redis-Defaults
	viper.SetDefault("redis.RedisEnabled", true)
	viper.SetDefault("redis.RedisAddr", "localhost:6379")
	viper.SetDefault("redis.RedisPassword", "")
	viper.SetDefault("redis.RedisDBIndex", 0)
	viper.SetDefault("redis.RedisHealthCheckInterval", "120s")

	// Workers-Defaults
	viper.SetDefault("workers.NumWorkers", 2)
	viper.SetDefault("workers.UploadQueueSize", 50)

	// Deduplication-Defaults
	viper.SetDefault("deduplication.Enabled", true)

	// ISO-Defaults
	viper.SetDefault("iso.Enabled", true)
	viper.SetDefault("iso.Size", "1GB")
	viper.SetDefault("iso.MountPoint", "/mnt/iso")
	viper.SetDefault("iso.Charset", "utf-8")
}

// ReadConfig lädt die Konfiguration aus der angegebenen Datei
func ReadConfig(configFilename string) error {
	viper.SetConfigFile(configFilename)
	viper.SetConfigType("toml")

	// Umgebungsvariablen einlesen, die übereinstimmen
	viper.AutomaticEnv()
	viper.SetEnvPrefix("HMAC") // Präfix für Umgebungsvariablen

	// Konfigurationsdatei einlesen
	if err := viper.ReadInConfig(); err != nil {
		return fmt.Errorf("Fehler beim Lesen der Konfigurationsdatei: %w", err)
	}

	// Konfiguration in die Config-Struktur unmarshallen
	if err := viper.Unmarshal(&Conf); err != nil {
		return fmt.Errorf("Fehler beim Dekodieren der Konfiguration: %w", err)
	}

	// Debug-Log der geladenen Konfiguration
	logrus.Debugf("Geladene Konfiguration: %+v", Conf.Server)

	// Konfiguration validieren
	if err := ValidateConfig(); err != nil {
		return fmt.Errorf("Konfigurationsvalidierung fehlgeschlagen: %w", err)
	}

	// Deduplication Enabled setzen
	Conf.Server.DeduplicationEnabled = viper.GetBool("deduplication.Enabled")

	return nil
}

// ValidateConfig validiert die Konfigurationsfelder
func ValidateConfig() error {
	if Conf.Server.ListenPort == "" {
		return fmt.Errorf("ListenPort muss gesetzt sein")
	}
	if Conf.Security.Secret == "" {
		return fmt.Errorf("Secret muss gesetzt sein")
	}
	if Conf.Server.StoragePath == "" {
		return fmt.Errorf("StoragePath muss gesetzt sein")
	}
	if Conf.Server.FileTTL == "" {
		return fmt.Errorf("FileTTL muss gesetzt sein")
	}

	// Timeouts validieren
	if _, err := time.ParseDuration(Conf.Timeouts.ReadTimeout); err != nil {
		return fmt.Errorf("ungültiger ReadTimeout: %v", err)
	}
	if _, err := time.ParseDuration(Conf.Timeouts.WriteTimeout); err != nil {
		return fmt.Errorf("ungültiger WriteTimeout: %v", err)
	}
	if _, err := time.ParseDuration(Conf.Timeouts.IdleTimeout); err != nil {
		return fmt.Errorf("ungültiger IdleTimeout: %v", err)
	}

	// Redis-Konfiguration validieren, falls aktiviert
	if Conf.Redis.RedisEnabled {
		if Conf.Redis.RedisAddr == "" {
			return fmt.Errorf("RedisAddr muss gesetzt sein, wenn Redis aktiviert ist")
		}
	}

	// ISO-Konfiguration validieren
	if Conf.ISO.Enabled {
		if Conf.ISO.Size == "" {
			return fmt.Errorf("ISO Größe muss gesetzt sein")
		}
		if Conf.ISO.MountPoint == "" {
			return fmt.Errorf("ISO MountPoint muss gesetzt sein")
		}
		if Conf.ISO.Charset == "" {
			return fmt.Errorf("ISO Charset muss gesetzt sein")
		}
	}

	// Weitere Validierungen nach Bedarf hinzufügen

	return nil
}
