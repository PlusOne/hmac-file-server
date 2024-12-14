package main

import (
	"context"
	"flag"
	"io"
	"net/http"
	"os"
	"os/signal"
	"runtime"
	"syscall"
	"time"

	"github.com/renz/source/hmac-file-server/internal/config"
	"github.com/renz/source/hmac-file-server/internal/metrics"
	"github.com/renz/source/hmac-file-server/internal/server"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/disk"
	"github.com/shirou/gopsutil/host"
	"github.com/shirou/gopsutil/mem"
	"github.com/sirupsen/logrus"
)

var (
	log  = logrus.New()
	conf config.Config
)

func parseDuration(durationStr string, logger *logrus.Logger) time.Duration {
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		logger.Fatalf("Failed to parse duration: %v", err)
	}
	return duration
}

func main() {
	// Initialize logger
	logger := logrus.New()
	logger.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})
	logger.SetOutput(os.Stdout)

	// Flags for configuration file
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	// Load configuration
	conf, err := config.LoadConfig(configFile)
	if err != nil {
		logger.Fatalf("Failed to load config: %v", err)
	}

	// Initialize metrics
	metrics.InitMetrics(conf.Server.MetricsEnabled, logger)

	// Create HTTP server
	srv := &http.Server{
		Addr:         ":" + conf.Server.ListenPort,
		Handler:      server.SetupRouter(),
		ReadTimeout:  parseDuration(conf.Timeouts.ReadTimeout, logger),
		WriteTimeout: parseDuration(conf.Timeouts.WriteTimeout, logger),
		IdleTimeout:  parseDuration(conf.Timeouts.IdleTimeout, logger),
	}

	// Create context with cancellation
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Handle OS signals for graceful shutdown
	go func() {
		quit := make(chan os.Signal, 1)
		signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
		sig := <-quit
		logger.Infof("Received signal %s. Initiating shutdown...", sig)
		cancel()
	}()

	// Start the server
	server.Start(ctx, srv, logger)
}

func configureLogging() {
	logFile, err := os.OpenFile(conf.Server.LogFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalf("Failed to open log file: %v", err)
	}
	log.SetOutput(io.MultiWriter(os.Stdout, logFile))
	log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})
}

func setDefaults() {
	conf.Server.LogLevel = "info"
	conf.Server.ListenPort = "8080"
	conf.Server.LogFile = "server.log"
	conf.Server.MinFreeBytes = 107374182400 // 100GB in bytes
}

func loadConfig(configFile string) error {
	return config.ReadConfig(configFile, &conf)
}

func helloHandler(w http.ResponseWriter) {
	w.Write([]byte("Hello, World!"))
}

func logSystemInfo() {
	log.Info("========================================")
	log.Info("       HMAC File Server Information      ")
	log.Info("========================================")

	log.Infof("Operating System: %s", runtime.GOOS)
	log.Infof("Architecture: %s", runtime.GOARCH)
	log.Infof("Number of CPUs: %d", runtime.NumCPU())
	log.Infof("Go Version: %s", runtime.Version())

	v, err := mem.VirtualMemory()
	if err == nil {
		log.Infof("Total Memory: %v MB", v.Total/1024/1024)
		log.Infof("Used Memory: %v MB (%.2f%%)", (v.Total-v.Free)/1024/1024, v.UsedPercent)
	}

	cpuInfo, err := cpu.Info()
	if err == nil && len(cpuInfo) > 0 {
		log.Infof("CPU Model: %s", cpuInfo[0].ModelName)
		log.Infof("CPU Cores: %d", runtime.NumCPU())
	}

	partitions, err := disk.Partitions(false)
	if err == nil {
		for _, partition := range partitions {
			usage, err := disk.Usage(partition.Mountpoint)
			if err == nil {
				log.Infof("Disk Mountpoint: %s, Total: %v GB, Used: %v GB (%.2f%%), Free: %v GB",
					partition.Mountpoint,
					usage.Total/1024/1024/1024,
					usage.Used/1024/1024/1024,
					usage.UsedPercent,
					usage.Free/1024/1024/1024)
			}
		}
	}

	hostInfo, err := host.Info()
	if err == nil {
		log.Infof("Hostname: %s", hostInfo.Hostname)
		log.Infof("Uptime: %v seconds", hostInfo.Uptime)
		log.Infof("Kernel Version: %s", hostInfo.KernelVersion)
	}
}

func worker(ctx context.Context) {
	log.Info("Worker started.")
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Worker received shutdown signal. Exiting...")
			return
		case <-ticker.C:
			// Implement your worker's periodic tasks here
			log.Info("Worker is performing its task...")
		}
	}
}
