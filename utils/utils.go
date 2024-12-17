package utils

import (
	"context" // Standard library
	"net"
	"net/http"
	"os"
	"os/signal" // Added import
	"runtime" // Added import for runtime info
	"strings"
	"syscall"
	"time"

	"github.com/prometheus/client_golang/prometheus/promhttp" // Third-party imports
	"github.com/sirupsen/logrus"                              // Third-party imports
	"gopkg.in/natefinch/lumberjack.v2"                        // Added import for Lumberjack
	"github.com/shirou/gopsutil/cpu"                          // Added import for CPU info
	"github.com/shirou/gopsutil/disk"                         // Added import for disk info
	"github.com/shirou/gopsutil/host"                         // Added import for host info
	"github.com/shirou/gopsutil/mem"                          // Added import for memory info
)

func SetupLogging(logLevel string, logFile string) {
	level, err := logrus.ParseLevel(logLevel)
	if err != nil {
		logrus.Fatalf("Invalid log level: %s", logLevel)
	}
	logrus.SetLevel(level)

	if logFile != "" {
		logrus.SetOutput(&lumberjack.Logger{ // Use Lumberjack for log rotation
			Filename:   logFile,
			MaxSize:    10, // megabytes
			MaxBackups: 3,
			MaxAge:     28, // days
			Compress:   true, // compress old log files
		})
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
	if clientIP == "" {
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
		logrus.Infof("Received signal %s. Initiating shutdown...", sig)

		ctxShutdown, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()

		if err := server.Shutdown(ctxShutdown); err != nil {
			logrus.Errorf("Error during server shutdown: %v", err)
		}
		cancel()

		logrus.Info("Shutdown process completed. Exiting application.")
		os.Exit(0)
	}()
}

// ParseDuration parses a duration string and handles the error.
func ParseDuration(durationStr string) time.Duration {
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		logrus.Fatalf("Invalid duration: %s", durationStr)
	}
	return duration
}

// autoAdjustWorkers dynamically adjusts the number of workers based on system resources.
func AutoAdjustWorkers() (int, int) {
	v, _ := mem.VirtualMemory()
	cpuCores, _ := cpu.Counts(true)

	numWorkers := cpuCores * 2
	if v.Available < 2*1024*1024*1024 {
		numWorkers = max(numWorkers/2, 1)
	}
	queueSize := numWorkers * 10

	logrus.Infof("Auto-adjusting workers: NumWorkers=%d, UploadQueueSize=%d", numWorkers, queueSize)
	return numWorkers, queueSize
}

// max returns the larger of x or y.
func max(x, y int) int {
	if x > y {
		return x
	}
	return y
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
