package utils

import (
	"context" // Standard library
	"net"
	"net/http" // Fixed: Added missing closing quote
	"os"
	"os/signal" // Added import
	"path/filepath" // Added import for file path
	"runtime" // Added import for runtime info
	"strings"
	"syscall"
	"time"
	"fmt" // Added import for error formatting
	"strconv" // Added import for size parsing
	"sync" // Added import for synchronization

	"github.com/prometheus/client_golang/prometheus/promhttp" // Third-party imports
	"github.com/sirupsen/logrus"                              // Third-party imports
	// "gopkg.in/natefinch/lumberjack.v2"                        // Removed import for Lumberjack
	"github.com/shirou/gopsutil/v3/cpu"                        // Updated import
	"github.com/shirou/gopsutil/v3/disk"                       // Updated import
	"github.com/shirou/gopsutil/v3/host"                       // Updated import
	"github.com/shirou/gopsutil/v3/mem"                        // Updated import
	// Removed import to avoid import cycle
)

var mu sync.Mutex // Added mutex for synchronization

// ...existing code...

func SetupLogging(logLevel string, logFile string) {
	level, err := logrus.ParseLevel(logLevel)
	if (err != nil) {
		logrus.Fatalf("Invalid log level: %s", logLevel)
	}
	logrus.SetLevel(level)

	if logFile != "" {
		// Use standard file output without lumberjack
		file, err := os.OpenFile(logFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			logrus.Fatalf("Failed to open log file %s: %v", logFile, err)
		}
		logrus.SetOutput(file)
	} else {
		logrus.SetOutput(os.Stdout)
	}

	logrus.SetFormatter(&logrus.JSONFormatter{ // Use JSONFormatter for structured logging
		TimestampFormat: time.RFC3339,
	})
}

func PrometheusHandler() http.Handler {
	return promhttp.Handler()
}

func GetClientIP(r *http.Request) string {
	clientIP := r.Header.Get("X-Real-IP")
	if (clientIP == "") {
		clientIP, _, _ = net.SplitHostPort(r.RemoteAddr)
	}
	return clientIP
}

func IsMultipart(contentType string) bool {
	return strings.Contains(contentType, "multipart/form-data")
}

// SetupGracefulShutdown sets up a graceful shutdown for the HTTP server.
func SetupGracefulShutdown(server *http.Server, ctx context.Context, cancel context.CancelFunc) {
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-quit
		logrus.Infof("Received signal %s. Shutting down server...", sig)
		ctxShutdown, cancelShutdown := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancelShutdown()
		if err := server.Shutdown(ctxShutdown); err != nil {
			logrus.Errorf("Error during server shutdown: %v", err)
		}
		cancel()
	}()
}

// Modify ParseDuration to return an error
func ParseDuration(durationStr string) (time.Duration, error) {
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		return 0, fmt.Errorf("invalid duration '%s': %w", durationStr, err)
	}
	return duration, nil
}

// AutoAdjustWorkers dynamically adjusts the number of HMAC workers based on system resources.
func AutoAdjustWorkers() int {
	cpuCores := runtime.NumCPU()
	numWorkers := cpuCores * 2
	logrus.Infof("Auto-adjusted HMAC workers: %d", numWorkers)
	return numWorkers
}

// AutoAdjustClamAVWorkers dynamically adjusts the number of ClamAV workers based on system resources with a minimum limit.
func AutoAdjustClamAVWorkers() int {
	cpuCores := runtime.NumCPU()
	numWorkers := cpuCores / 2
	if numWorkers < 1 {
		numWorkers = 1
	}
	maxWorkers := 16
	if numWorkers > maxWorkers {
		numWorkers = maxWorkers
	}
	logrus.Infof("Auto-adjusted ClamAV workers: %d", numWorkers)
	return numWorkers
}

// logSystemInfo logs detailed system information.
func LogSystemInfo(versionString string) {
	logrus.Info("========================================")
	logrus.Infof("       HMAC File Server - %s          ", versionString)
	logrus.Info("  Secure File Handling with HMAC Auth   ")
	logrus.Info("========================================")

	logrus.Info("Features: Prometheus Metrics, Chunked Uploads, ClamAV Scanning")
	logrus.Info("Build Date: 2024-10-28")

	logrus.Infof("Operating System: %s", runtime.GOOS)
	logrus.Infof("Architecture: %s", runtime.GOARCH)
	logrus.Infof("Number of CPUs: %d", runtime.NumCPU())
	logrus.Infof("Go Version: %s", runtime.Version())

	v, _ := mem.VirtualMemory()
	logrus.Infof("Total Memory: %v MB", v.Total/1024/1024)
	logrus.Infof("Free Memory: %v MB", v.Free/1024/1024)
	logrus.Infof("Used Memory: %v MB", v.Used/1024/1024)

	cpuInfo, _ := cpu.Info()
	for _, info := range cpuInfo {
		logrus.Infof("CPU Model: %s, Cores: %d, Mhz: %f", info.ModelName, info.Cores, info.Mhz)
	}

	partitions, _ := disk.Partitions(false)
	for _, partition := range partitions {
		usage, _ := disk.Usage(partition.Mountpoint)
		logrus.Infof("Disk Mountpoint: %s, Total: %v GB, Free: %v GB, Used: %v GB",
			partition.Mountpoint, usage.Total/1024/1024/1024, usage.Free/1024/1024/1024, usage.Used/1024/1024/1024)
	}

	hInfo, _ := host.Info()
	logrus.Infof("Hostname: %s", hInfo.Hostname)
	logrus.Infof("Uptime: %v seconds", hInfo.Uptime)
	logrus.Infof("Boot Time: %v", time.Unix(int64(hInfo.BootTime), 0))
	logrus.Infof("Platform: %s", hInfo.Platform)
	logrus.Infof("Platform Family: %s", hInfo.PlatformFamily)
	logrus.Infof("Platform Version: %s", hInfo.PlatformVersion)
	logrus.Infof("Kernel Version: %s", hInfo.KernelVersion)
}

// Update functions that call ParseDuration to handle the error
func CleanupExpiredFiles(ctx context.Context, storagePath string, fileTTL string) {
	ticker := time.NewTicker(1 * time.Hour) // Adjust the interval as needed
	defer ticker.Stop()

	for {
		select {
		case <-ticker.C:
			ttl, err := ParseDuration(fileTTL)
			if err != nil {
				logrus.Errorf("Invalid FileTTL: %v", err)
				return
			}
			err = filepath.Walk(storagePath, func(path string, info os.FileInfo, err error) error {
				if err != nil {
					logrus.Errorf("Error accessing path %s: %v", path, err)
					return nil
				}
				if (!info.IsDir()) {
					fileAge := time.Since(info.ModTime())
					if fileAge > ttl {
						err := os.Remove(path)
						if err != nil { // Fixed: Removed extra closing parenthesis
							logrus.Errorf("Failed to delete expired file %s: %v", path, err)
						}
					}
				}
				return nil
			})
			if err != nil {
				logrus.Errorf("Error during cleanup: %v", err)
			}
		case <-ctx.Done():
			logrus.Info("CleanupExpiredFiles routine stopped.")
			return
		}
	}
}

// ParseSize parses a size string (e.g., "2TB", "100MB") and returns the size in bytes.
func ParseSize(sizeStr string) (int64, error) {
	sizeStr = strings.TrimSpace(sizeStr)
	if len(sizeStr) < 2 {
		return 0, fmt.Errorf("invalid size format")
	}

	// Split the numeric part and the unit
	var valueStr string
	var unit string
	for i, r := range sizeStr {
		if r < '0' || r > '9' {
			valueStr = strings.TrimSpace(sizeStr[:i])
			unit = strings.TrimSpace(sizeStr[i:])
			break
		}
	}
	if valueStr == "" || unit == "" {
		return 0, fmt.Errorf("invalid size format")
	}

	value, err := strconv.ParseFloat(valueStr, 64)
	if err != nil {
		return 0, err
	}

	var multiplier int64
	switch strings.ToUpper(unit) {
	case "B":
		multiplier = 1
	case "KB", "K":
		multiplier = 1024
	case "MB", "M":
		multiplier = 1024 * 1024
	case "GB", "G":
		multiplier = 1024 * 1024 * 1024
	case "TB", "T":
		multiplier = 1024 * 1024 * 1024 * 1024
	case "KIB":
		multiplier = 1024
	case "MIB":
		multiplier = 1024 * 1024
	case "GIB":
		multiplier = 1024 * 1024 * 1024
	case "TIB":
		multiplier = 1024 * 1024 * 1024 * 1024
	default:
		return 0, fmt.Errorf("unknown size unit: %s", unit)
	}

	return int64(value * float64(multiplier)), nil
}
