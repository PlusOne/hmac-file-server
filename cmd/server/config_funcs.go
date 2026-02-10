// config_funcs.go — Configuration utility functions (parsing, validation, defaults).
package main

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"os/exec"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/mem"
	"github.com/spf13/viper"
)

// parseSize converts a human-readable size string to bytes
func parseSize(sizeStr string) (int64, error) {
	sizeStr = strings.TrimSpace(sizeStr)
	if len(sizeStr) < 2 {
		return 0, fmt.Errorf("invalid size string: %s", sizeStr)
	}

	unit := strings.ToUpper(sizeStr[len(sizeStr)-2:])
	valueStr := sizeStr[:len(sizeStr)-2]
	value, err := strconv.Atoi(valueStr)
	if err != nil {
		return 0, fmt.Errorf("invalid size value: %v", err)
	}

	switch unit {
	case "KB":
		return int64(value) * 1024, nil
	case "MB":
		return int64(value) * 1024 * 1024, nil
	case "GB":
		return int64(value) * 1024 * 1024 * 1024, nil
	default:
		return 0, fmt.Errorf("unknown size unit: %s", unit)
	}
}

// parseTTL converts a human-readable TTL string to a time.Duration
func parseTTL(ttlStr string) (time.Duration, error) {
	ttlStr = strings.ToLower(strings.TrimSpace(ttlStr))
	if ttlStr == "" {
		return 0, fmt.Errorf("TTL string cannot be empty")
	}
	var valueStr string
	var unit rune
	for _, r := range ttlStr {
		if r >= '0' && r <= '9' {
			valueStr += string(r)
		} else {
			unit = r
			break
		}
	}
	val, err := strconv.Atoi(valueStr)
	if err != nil {
		return 0, fmt.Errorf("invalid TTL value: %v", err)
	}
	switch unit {
	case 's':
		return time.Duration(val) * time.Second, nil
	case 'm':
		return time.Duration(val) * time.Minute, nil
	case 'h':
		return time.Duration(val) * time.Hour, nil
	case 'd':
		return time.Duration(val) * 24 * time.Hour, nil
	case 'w':
		return time.Duration(val) * 7 * 24 * time.Hour, nil
	case 'y':
		return time.Duration(val) * 365 * 24 * time.Hour, nil
	default:
		return 0, fmt.Errorf("unknown TTL unit: %c", unit)
	}
}

//nolint:unused
var logMessages []string

//nolint:unused
var logMu sync.Mutex

//nolint:unused
func flushLogMessages() {
	logMu.Lock()
	defer logMu.Unlock()
	for _, msg := range logMessages {
		log.Info(msg)
	}
	logMessages = []string{}
}

// writePIDFile writes the current process ID to the specified pid file
func writePIDFile(pidPath string) error {
	pid := os.Getpid()
	pidStr := strconv.Itoa(pid)
	err := os.WriteFile(pidPath, []byte(pidStr), 0644)
	if err != nil {
		log.Errorf("Failed to write PID file: %v", err)
		return err
	}
	log.Infof("PID %d written to %s", pid, pidPath)
	return nil
}

// removePIDFile removes the PID file
func removePIDFile(pidPath string) {
	err := os.Remove(pidPath)
	if err != nil {
		log.Errorf("Failed to remove PID file: %v", err)
	} else {
		log.Infof("PID file %s removed successfully", pidPath)
	}
}

// createAndMountISO creates an ISO container and mounts it to the specified mount point
func createAndMountISO(size, mountpoint, charset string) error {
	isoPath := conf.ISO.ContainerFile

	// Create an empty ISO file
	cmd := exec.Command("dd", "if=/dev/zero", fmt.Sprintf("of=%s", isoPath), fmt.Sprintf("bs=%s", size), "count=1")
	if err := cmd.Run(); err != nil {
		isoCreationErrorsTotal.Inc()
		return fmt.Errorf("failed to create ISO file: %w", err)
	}

	// Format the ISO file with a filesystem
	cmd = exec.Command("mkfs", "-t", "iso9660", "-input-charset", charset, isoPath)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to format ISO file: %w", err)
	}

	// Create the mount point directory if it doesn't exist
	if err := os.MkdirAll(mountpoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	// Mount the ISO file
	cmd = exec.Command("mount", "-o", "loop", isoPath, mountpoint)
	if err := cmd.Run(); err != nil {
		isoMountErrorsTotal.Inc()
		return fmt.Errorf("failed to mount ISO file: %w", err)
	}

	isoContainersCreatedTotal.Inc()
	isoContainersMountedTotal.Inc()
	return nil
}

func initializeNetworkProtocol(forceProtocol string) (*net.Dialer, error) {
	// Handle empty/default value
	if forceProtocol == "" {
		forceProtocol = "auto"
	}

	switch forceProtocol {
	case "ipv4":
		return &net.Dialer{
			Timeout:   5 * time.Second,
			DualStack: false,
			Control: func(network, address string, c syscall.RawConn) error {
				if network == "tcp6" {
					return fmt.Errorf("IPv6 is disabled by forceprotocol setting")
				}
				return nil
			},
		}, nil
	case "ipv6":
		return &net.Dialer{
			Timeout:   5 * time.Second,
			DualStack: false,
			Control: func(network, address string, c syscall.RawConn) error {
				if network == "tcp4" {
					return fmt.Errorf("IPv4 is disabled by forceprotocol setting")
				}
				return nil
			},
		}, nil
	case "auto":
		return &net.Dialer{
			Timeout:   5 * time.Second,
			DualStack: true,
		}, nil
	default:
		return nil, fmt.Errorf("invalid forceprotocol value: %s", forceProtocol)
	}
}

// printExampleConfig prints an example configuration file
// nolint:unused
func printExampleConfig() {
	fmt.Print(`
[server]
bind_ip = "0.0.0.0"
listenport = "8080"
unixsocket = false
storagepath = "./uploads"
logfile = "/var/log/hmac-file-server.log"
metricsenabled = true
metricsport = "9090"
minfreebytes = "100MB"
filettl = "8760h"
filettlenabled = true
autoadjustworkers = true
networkevents = true
pidfilepath = "/var/run/hmacfileserver.pid"
cleanuponexit = true
precaching = true
deduplicationenabled = true
globalextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]
# FileNaming options: "HMAC", "None"
filenaming = "HMAC"
forceprotocol = "auto"

[logging]
level = "info"
file = "/var/log/hmac-file-server.log"
max_size = 100
max_backups = 7
max_age = 30
compress = true

[deduplication]
enabled = true
directory = "./deduplication"

[iso]
enabled = true
size = "1GB"
mountpoint = "/mnt/iso"
charset = "utf-8"
containerfile = "/mnt/iso/container.iso"

[timeouts]
readtimeout = "4800s"
writetimeout = "4800s"
idletimeout = "4800s"

[security]
secret = "changeme"
enablejwt = false
jwtsecret = "anothersecretkey"
jwtalgorithm = "HS256"
jwtexpiration = "24h"

[versioning]
enableversioning = false
maxversions = 1

[uploads]
resumableuploadsenabled = true
chunkeduploadsenabled = true
chunksize = "8192"
allowedextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[downloads]
resumabledownloadsenabled = true
chunkeddownloadsenabled = true
chunksize = "8192"
allowedextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[clamav]
clamavenabled = true
clamavsocket = "/var/run/clamav/clamd.ctl"
numscanworkers = 2
scanfileextensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[redis]
redisenabled = true
redisdbindex = 0
redisaddr = "localhost:6379"
redispassword = ""
redishealthcheckinterval = "120s"

[workers]
numworkers = 4
uploadqueuesize = 50

[file]
# Add file-specific configurations here

[build]
version = "3.3.0"
`)
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func autoAdjustWorkers() (int, int) {
	v, _ := mem.VirtualMemory()
	cpuCores, _ := cpu.Counts(true)

	numWorkers := cpuCores * 2
	if v.Available < 4*1024*1024*1024 { // Less than 4GB available
		numWorkers = max(numWorkers/2, 1)
	} else if v.Available < 8*1024*1024*1024 { // Less than 8GB available
		numWorkers = max(numWorkers*3/4, 1)
	}
	queueSize := numWorkers * 10

	log.Infof("Auto-adjusting workers: NumWorkers=%d, UploadQueueSize=%d", numWorkers, queueSize)
	workerAdjustmentsTotal.Inc()
	return numWorkers, queueSize
}

func initializeWorkerSettings(server *ServerConfig, workers *WorkersConfig, clamav *ClamAVConfig) {
	if server.AutoAdjustWorkers {
		numWorkers, queueSize := autoAdjustWorkers()
		workers.NumWorkers = numWorkers
		workers.UploadQueueSize = queueSize
		clamav.NumScanWorkers = max(numWorkers/2, 1)

		log.Infof("AutoAdjustWorkers enabled: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
			workers.NumWorkers, workers.UploadQueueSize, clamav.NumScanWorkers)
	} else {
		log.Infof("Manual configuration in effect: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
			workers.NumWorkers, workers.UploadQueueSize, clamav.NumScanWorkers)
	}
}

func monitorWorkerPerformance(ctx context.Context, server *ServerConfig, w *WorkersConfig, clamav *ClamAVConfig) {
	ticker := time.NewTicker(5 * time.Minute)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping worker performance monitor.")
			return
		case <-ticker.C:
			if server.AutoAdjustWorkers {
				numWorkers, queueSize := autoAdjustWorkers()
				w.NumWorkers = numWorkers
				w.UploadQueueSize = queueSize
				clamav.NumScanWorkers = max(numWorkers/2, 1)

				log.Infof("Re-adjusted workers: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
					w.NumWorkers, w.UploadQueueSize, clamav.NumScanWorkers)
				workerReAdjustmentsTotal.Inc()
			}
		}
	}
}

// readConfig reads configuration from a file
// nolint:unused
func readConfig(configFilename string, c *Config) error {
	viper.SetConfigFile(configFilename)
	if err := viper.ReadInConfig(); err != nil {
		log.WithError(err).Errorf("Unable to read config from %s", configFilename)
		return err
	}
	if err := viper.Unmarshal(c); err != nil {
		return fmt.Errorf("unable to decode config into struct: %v", err)
	}
	return nil
}

// setDefaults sets default configuration values
// nolint:unused
func setDefaults() {
	viper.SetDefault("server.listen_address", ":8080")
	viper.SetDefault("server.storage_path", "./uploads")
	viper.SetDefault("server.metrics_enabled", true)
	viper.SetDefault("server.metrics_path", "/metrics")
	viper.SetDefault("server.pid_file", "/var/run/hmac-file-server.pid")
	viper.SetDefault("server.max_upload_size", "10GB")
	viper.SetDefault("server.max_header_bytes", 1048576) // 1MB
	viper.SetDefault("server.cleanup_interval", "24h")
	viper.SetDefault("server.max_file_age", "720h") // 30 days
	viper.SetDefault("server.pre_cache", true)
	viper.SetDefault("server.pre_cache_workers", 4)
	viper.SetDefault("server.pre_cache_interval", "1h")
	viper.SetDefault("server.global_extensions", []string{})
	viper.SetDefault("server.deduplication_enabled", true)
	viper.SetDefault("server.min_free_bytes", "1GB")
	viper.SetDefault("server.file_naming", "original")
	viper.SetDefault("server.force_protocol", "auto")
	viper.SetDefault("server.enable_dynamic_workers", true)
	viper.SetDefault("server.worker_scale_up_thresh", 50)
	viper.SetDefault("server.worker_scale_down_thresh", 10)

	viper.SetDefault("uploads.allowed_extensions", []string{".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp"})
	viper.SetDefault("uploads.chunked_uploads_enabled", true)
	viper.SetDefault("uploads.chunk_size", "10MB")
	viper.SetDefault("uploads.resumable_uploads_enabled", true)
	viper.SetDefault("uploads.max_resumable_age", "48h")

	viper.SetDefault("downloads.allowed_extensions", []string{".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp"})
	viper.SetDefault("downloads.chunked_downloads_enabled", true)
	viper.SetDefault("downloads.chunk_size", "10MB")
	viper.SetDefault("downloads.resumable_downloads_enabled", true)

	viper.SetDefault("security.secret", "your-very-secret-hmac-key")
	viper.SetDefault("security.enablejwt", false)
	viper.SetDefault("security.jwtsecret", "your-256-bit-secret")
	viper.SetDefault("security.jwtalgorithm", "HS256")
	viper.SetDefault("security.jwtexpiration", "24h")

	// Logging defaults
	viper.SetDefault("logging.level", "info")
	viper.SetDefault("logging.file", "/var/log/hmac-file-server.log")
	viper.SetDefault("logging.max_size", 100)
	viper.SetDefault("logging.max_backups", 7)
	viper.SetDefault("logging.max_age", 30)
	viper.SetDefault("logging.compress", true)

	// Deduplication defaults
	viper.SetDefault("deduplication.enabled", false)
	viper.SetDefault("deduplication.directory", "./dedup_store")

	// ISO defaults
	viper.SetDefault("iso.enabled", false)
	viper.SetDefault("iso.mount_point", "/mnt/hmac_iso")
	viper.SetDefault("iso.size", "1GB")
	viper.SetDefault("iso.charset", "utf-8")
	viper.SetDefault("iso.containerfile", "/var/lib/hmac-file-server/data.iso")

	// Timeouts defaults
	viper.SetDefault("timeouts.read", "60s")
	viper.SetDefault("timeouts.write", "60s")
	viper.SetDefault("timeouts.idle", "120s")
	viper.SetDefault("timeouts.shutdown", "30s")

	// Versioning defaults
	viper.SetDefault("versioning.enabled", false)
	viper.SetDefault("versioning.backend", "simple")
	viper.SetDefault("versioning.max_revisions", 5)

	// Session store defaults for network resilience
	viper.SetDefault("session_store.enabled", true)
	viper.SetDefault("session_store.backend", "memory")
	viper.SetDefault("session_store.max_sessions", 10000)
	viper.SetDefault("session_store.cleanup_interval", "30m")
	viper.SetDefault("session_store.max_session_age", "72h")
	viper.SetDefault("session_store.redis_url", "")

	// ... other defaults for Uploads, Downloads, ClamAV, Redis, Workers, File, Build
	viper.SetDefault("build.version", "dev")
}

func validateConfig(c *Config) error {
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

	// Validate JWT secret if JWT is enabled
	if c.Security.EnableJWT && strings.TrimSpace(c.Security.JWTSecret) == "" {
		return errors.New("security.jwtsecret is required when security.enablejwt is true")
	}

	// Validate HMAC secret if JWT is not enabled (as it's the fallback)
	if !c.Security.EnableJWT && strings.TrimSpace(c.Security.Secret) == "" {
		return errors.New("security.secret is required for HMAC authentication (when JWT is disabled)")
	}

	return nil
}

// Suppress unused import warnings for packages only used in specific functions.
var _ = runtime.Version
var _ = context.Background
