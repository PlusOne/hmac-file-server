// internal/logging/logging.go

package logging

import (
	"io"
	"os"
	"runtime"
	"time"

	"github.com/PlusOne/hmac-file-server/internal/config"

	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/disk"
	"github.com/shirou/gopsutil/host"
	"github.com/shirou/gopsutil/mem"
	"github.com/sirupsen/logrus"
)

var Log *logrus.Logger

// InitLogger initialisiert den Logger basierend auf der Konfiguration
func InitLogger() {
	Log = logrus.New()

	level, err := logrus.ParseLevel(config.Conf.Server.LogLevel)
	if err != nil {
		Log.Fatalf("Invalid log level: %s", config.Conf.Server.LogLevel)
	}
	Log.SetLevel(level)

	if config.Conf.Server.LogFile != "" {
		logFile, err := os.OpenFile(config.Conf.Server.LogFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			Log.Fatalf("Failed to open log file: %v", err)
		}
		Log.SetOutput(io.MultiWriter(os.Stdout, logFile))
	} else {
		Log.SetOutput(os.Stdout)
	}

	// Use Text formatter for human-readable logs
	Log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
		// You can customize the format further if needed
	})
}

// LogSystemInfo protokolliert Systeminformationen
func LogSystemInfo() {
	Log.Info("========================================")
	Log.Infof("       HMAC File Server - %s          ", config.VersionString)
	Log.Info("  Secure File Handling with HMAC Auth   ")
	Log.Info("========================================")

	Log.Info("Features: Prometheus Metrics, Chunked Uploads, ClamAV Scanning")
	Log.Info("Build Date: 2024-10-28")

	Log.Infof("Operating System: %s", runtime.GOOS)
	Log.Infof("Architecture: %s", runtime.GOARCH)
	Log.Infof("Number of CPUs: %d", runtime.NumCPU())
	Log.Infof("Go Version: %s", runtime.Version())

	v, _ := mem.VirtualMemory()
	Log.Infof("Total Memory: %v MB", v.Total/1024/1024)
	Log.Infof("Free Memory: %v MB", v.Free/1024/1024)
	Log.Infof("Used Memory: %v MB", v.Used/1024/1024)

	cpuInfo, _ := cpu.Info()
	for _, info := range cpuInfo {
		Log.Infof("CPU Model: %s, Cores: %d, Mhz: %f", info.ModelName, info.Cores, info.Mhz)
	}

	partitions, _ := disk.Partitions(false)
	for _, partition := range partitions {
		usage, _ := disk.Usage(partition.Mountpoint)
		Log.Infof("Disk Mountpoint: %s, Total: %v GB, Free: %v GB, Used: %v GB",
			partition.Mountpoint, usage.Total/1024/1024/1024, usage.Free/1024/1024/1024, usage.Used/1024/1024/1024)
	}

	hInfo, _ := host.Info()
	Log.Infof("Hostname: %s", hInfo.Hostname)
	Log.Infof("Uptime: %v seconds", hInfo.Uptime)
	Log.Infof("Boot Time: %v", time.Unix(int64(hInfo.BootTime), 0))
	Log.Infof("Platform: %s", hInfo.Platform)
	Log.Infof("Platform Family: %s", hInfo.PlatformFamily)
	Log.Infof("Platform Version: %s", hInfo.PlatformVersion)
	Log.Infof("Kernel Version: %s", hInfo.KernelVersion)
}
