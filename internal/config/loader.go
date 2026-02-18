package config

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// ConfigFileGlobal stores the config file path for validation helpers.
var ConfigFileGlobal string

// LoadSimplifiedConfig loads configuration from a TOML file using viper.
func LoadSimplifiedConfig(configFile string) (*Config, error) {
	if configFile == "" {
		configFile = "./config.toml"
	}

	if !fileExists(configFile) {
		return nil, fmt.Errorf("configuration file not found: %s", configFile)
	}

	viper.SetConfigFile(configFile)
	viper.SetConfigType("toml")

	if err := viper.ReadInConfig(); err != nil {
		return nil, fmt.Errorf("error reading config file: %w", err)
	}

	var conf Config
	if err := viper.Unmarshal(&conf); err != nil {
		return nil, fmt.Errorf("error parsing config file: %w", err)
	}

	applyDefaults(&conf)

	log.Infof("Configuration loaded from %s", configFile)
	return &conf, nil
}

// DefaultConfig returns a Config with sensible defaults.
func DefaultConfig() *Config {
	conf := &Config{}
	applyDefaults(conf)
	return conf
}

func applyDefaults(conf *Config) {
	if conf.Server.ListenAddress == "" {
		conf.Server.ListenAddress = "8080"
	}
	if conf.Server.StoragePath == "" {
		conf.Server.StoragePath = "./uploads"
	}
	if conf.Server.MinFreeBytes == "" {
		conf.Server.MinFreeBytes = "100MB"
	}
	if conf.Server.FileNaming == "" {
		conf.Server.FileNaming = "original"
	}
	if conf.Server.ForceProtocol == "" {
		conf.Server.ForceProtocol = "auto"
	}
	if conf.Server.PIDFilePath == "" {
		conf.Server.PIDFilePath = "/var/run/hmacfileserver.pid"
	}

	if conf.Logging.Level == "" {
		conf.Logging.Level = "info"
	}
	if conf.Logging.MaxSize == 0 {
		conf.Logging.MaxSize = 100
	}
	if conf.Logging.MaxBackups == 0 {
		conf.Logging.MaxBackups = 7
	}
	if conf.Logging.MaxAge == 0 {
		conf.Logging.MaxAge = 30
	}

	if conf.Timeouts.Read == "" {
		conf.Timeouts.Read = "4800s"
	}
	if conf.Timeouts.Write == "" {
		conf.Timeouts.Write = "4800s"
	}
	if conf.Timeouts.Idle == "" {
		conf.Timeouts.Idle = "4800s"
	}
	if conf.Timeouts.Shutdown == "" {
		conf.Timeouts.Shutdown = "30s"
	}

	if conf.Workers.NumWorkers == 0 {
		conf.Workers.NumWorkers = 4
	}
	if conf.Workers.UploadQueueSize == 0 {
		conf.Workers.UploadQueueSize = 50
	}

	if conf.Build.Version == "" {
		conf.Build.Version = "3.4.0"
	}

	if conf.Security.JWTAlgorithm == "" {
		conf.Security.JWTAlgorithm = "HS256"
	}
	if conf.Security.JWTExpiration == "" {
		conf.Security.JWTExpiration = "24h"
	}

	if conf.Admin.PathPrefix == "" {
		conf.Admin.PathPrefix = "/admin"
	}

	if conf.Quotas.Custom == nil {
		conf.Quotas.Custom = make(map[string]string)
	}
}

// ValidateConfig performs basic configuration validation.
func ValidateConfig(c *Config) error {
	if c.Server.ListenAddress == "" {
		return errors.New("server.listen_address is required")
	}

	if c.Server.FileTTL == "" && c.Server.FileTTLEnabled {
		return errors.New("server.file_ttl is required when server.file_ttl_enabled is true")
	}

	if _, err := time.ParseDuration(c.Timeouts.Read); err != nil {
		return fmt.Errorf("invalid timeouts.read: %v", err)
	}
	if _, err := time.ParseDuration(c.Timeouts.Write); err != nil {
		return fmt.Errorf("invalid timeouts.write: %v", err)
	}
	if _, err := time.ParseDuration(c.Timeouts.Idle); err != nil {
		return fmt.Errorf("invalid timeouts.idle: %v", err)
	}

	if c.Versioning.Enabled {
		if c.Versioning.MaxRevs <= 0 {
			return errors.New("versioning.max_revisions must be positive if versioning is enabled")
		}
	}

	if c.Security.EnableJWT && strings.TrimSpace(c.Security.JWTSecret) == "" {
		return errors.New("security.jwtsecret is required when security.enablejwt is true")
	}

	if !c.Security.EnableJWT && strings.TrimSpace(c.Security.Secret) == "" {
		return errors.New("security.secret is required for HMAC authentication (when JWT is disabled)")
	}

	return nil
}

func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

// GenerateMinimalConfig returns a minimal example configuration string.
func GenerateMinimalConfig() string {
	return `# HMAC File Server - Minimal Configuration
# For full options, use --genconfig-advanced

[server]
listen_address = "8080"
bind_ip = "0.0.0.0"
storage_path = "./uploads"
metricsenabled = true
metricsport = "9090"
min_free_bytes = "100MB"
filettl = "8760h"
filettlenabled = true
pidfilepath = "/var/run/hmacfileserver.pid"

[security]
secret = "change-this-to-your-secret"

[logging]
level = "info"
file = "/var/log/hmac-file-server.log"
max_size = 100
max_backups = 7
max_age = 30
compress = true

[timeouts]
readtimeout = "4800s"
writetimeout = "4800s"
idletimeout = "4800s"

[workers]
numworkers = 4
uploadqueuesize = 50

[build]
version = "3.4.0"
`
}

// CreateMinimalConfig writes a minimal config.toml to disk.
func CreateMinimalConfig() error {
	content := GenerateMinimalConfig()
	f, err := os.Create("config.toml")
	if err != nil {
		return err
	}
	defer f.Close()
	w := bufio.NewWriter(f)
	_, err = fmt.Fprint(w, content)
	if err != nil {
		return err
	}
	return w.Flush()
}

// GenerateAdvancedConfigTemplate returns an advanced configuration template.
func GenerateAdvancedConfigTemplate() string {
	return `# HMAC File Server - Advanced Configuration Template
# Generated by hmac-file-server --genconfig-advanced

[server]
listen_address = "8080"
bind_ip = "0.0.0.0"
storage_path = "./uploads"
unixsocket = false
metricsenabled = true
metricsport = "9090"
min_free_bytes = "100MB"
max_upload_size = "10GB"
filettl = "8760h"
filettlenabled = true
autoadjustworkers = true
networkevents = true
pidfilepath = "/var/run/hmacfileserver.pid"
clean_upon_exit = true
precaching = true
deduplication_enabled = true
file_naming = "original"
force_protocol = "auto"
global_extensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]
enable_dynamic_workers = true
worker_scale_up_thresh = 50
worker_scale_down_thresh = 10

[security]
secret = "your-secret-key-here"
enablejwt = false
jwtsecret = "your-jwt-secret"
jwtalgorithm = "HS256"
jwtexpiration = "24h"
enhanced_security = false

[logging]
level = "info"
file = "/var/log/hmac-file-server.log"
max_size = 100
max_backups = 7
max_age = 30
compress = true

[timeouts]
readtimeout = "4800s"
writetimeout = "4800s"
idletimeout = "4800s"
shutdown = "30s"

[uploads]
allowed_extensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]
chunked_uploads_enabled = true
chunk_size = "10MB"
resumable_uploads_enabled = true

[downloads]
allowed_extensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]
chunked_downloads_enabled = true
chunk_size = "10MB"
resumable_downloads_enabled = true

[deduplication]
enabled = false
directory = "./deduplication"

[iso]
enabled = false
size = "1GB"
mountpoint = "/mnt/iso"
charset = "utf-8"
containerfile = "/mnt/iso/container.iso"

[versioning]
enableversioning = false
maxversions = 1

[clamav]
clamavenabled = false
clamavsocket = "/var/run/clamav/clamd.ctl"
numscanworkers = 2
scanfileextensions = [".exe", ".dll", ".bat", ".cmd", ".com", ".vbs", ".js"]

[redis]
redisenabled = false
redisdbindex = 0
redisaddr = "localhost:6379"
redispassword = ""
redishealthcheckinterval = "120s"

[workers]
numworkers = 4
uploadqueuesize = 50

[network_resilience]
fast_detection = true
quality_monitoring = true
predictive_switching = false
mobile_optimizations = true
detection_interval = "5s"
quality_check_interval = "30s"
max_detection_interval = "60s"

[client_network_support]
session_based_tracking = true
allow_ip_changes = true
session_migration_timeout = "5m"
max_ip_changes_per_session = 10

[audit]
enabled = false
output = "file"
path = "/var/log/hmac-audit.log"
format = "json"
events = ["upload", "download", "auth_success", "auth_failure"]
max_size = 100
max_age = 30

[validation]
check_magic_bytes = false

[quotas]
enabled = false
default = "100MB"
tracking = "memory"

[admin]
enabled = false
path_prefix = "/admin"

[admin.auth]
type = "bearer"

[build]
version = "3.4.0"
`
}
