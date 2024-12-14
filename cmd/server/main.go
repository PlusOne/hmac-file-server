package main

import (
	"context"
	"flag"
	"io"
	"net/http"
	"os"
	"runtime"

	"github.com/renz/source/hmac-file-server/internal/config"
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

func main() {
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	log.Infof("Using configuration file: %s", configFile)

	// Set defaults and load config
	setDefaults()
	if err := loadConfig(configFile); err != nil {
		log.Fatalf("Error reading config: %v", err)
	}

	// Configure logging
	configureLogging()

	// Set log level
	level, err := logrus.ParseLevel(conf.Server.LogLevel)
	if err != nil {
		log.Fatalf("Invalid log level: %v", err)
	}
	log.SetLevel(level)

	// Log system info
	logSystemInfo()

	// Set up server
	mux := http.NewServeMux()
	mux.HandleFunc("/", helloHandler)
	serverInstance := &http.Server{
		Addr:    ":" + conf.Server.ListenPort,
		Handler: mux,
	}

	// Context for graceful shutdown
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Start the server using the server package
	server.Start(ctx, serverInstance, log)

	log.Println("Shutting down...")
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
}

func loadConfig(configFile string) error {
	return config.ReadConfig(configFile, &conf)
}

func helloHandler(w http.ResponseWriter, r *http.Request) {
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

	v, _ := mem.VirtualMemory()
	log.Infof("Total Memory: %v MB", v.Total/1024/1024)
	log.Infof("Used Memory: %v MB (%.2f%%)", (v.Total-v.Free)/1024/1024, v.UsedPercent)

	cpuInfo, _ := cpu.Info()
	if len(cpuInfo) > 0 {
		log.Infof("CPU Model: %s", cpuInfo[0].ModelName)
		log.Infof("CPU Cores: %d", runtime.NumCPU())
	}

	partitions, _ := disk.Partitions(false)
	for _, partition := range partitions {
		usage, _ := disk.Usage(partition.Mountpoint)
		log.Infof("Disk Mountpoint: %s, Total: %v GB, Used: %v GB (%.2f%%), Free: %v GB",
			partition.Mountpoint, usage.Total/1024/1024/1024, usage.Used/1024/1024/1024, usage.UsedPercent, usage.Free/1024/1024/1024)
	}

	hostInfo, _ := host.Info()
	log.Infof("Hostname: %s", hostInfo.Hostname)
	log.Infof("Uptime: %v seconds", hostInfo.Uptime)
	log.Infof("Kernel Version: %s", hostInfo.KernelVersion)
}
