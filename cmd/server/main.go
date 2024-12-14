package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/signal"
	"runtime"
	"syscall"

	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/disk"
	"github.com/shirou/gopsutil/host"
	"github.com/shirou/gopsutil/mem"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

type Config struct {
	LogLevel   string `mapstructure:"log_level"`
	ListenPort string `mapstructure:"listen_port"`
}

var (
	log  = logrus.New()
	conf Config
)

func main() {
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	log.Infof("Using configuration file: %s", configFile) // Add this line

	// Set defaults and load config
	setDefaults()
	if err := loadConfig(); err != nil {
		log.Fatalf("Error reading config: %v", err)
	}

	// Configure logging
	configureLogging()

	// Set log level
	level, err := logrus.ParseLevel(conf.LogLevel)
	if err != nil {
		log.Fatalf("Invalid log level: %v", err)
	}
	log.SetLevel(level)

	// Log system info
	logSystemInfo()

	// Set up server
	mux := http.NewServeMux()
	mux.HandleFunc("/", helloHandler)
	server := &http.Server{
		Addr:    ":" + conf.ListenPort,
		Handler: mux,
	}

	// Graceful shutdown
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	quit := make(chan os.Signal, 1)
	setupGracefulShutdown(server, quit, cancel)

	// Start server
	log.Infof("Starting server on port %s...", conf.ListenPort)
	if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		log.Fatalf("Could not listen on %s: %v", server.Addr, err)
	}

	<-ctx.Done() // Wait until context is canceled
	log.Println("Shutting down...")
}

func configureLogging() {
	logFile, err := os.OpenFile("server.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalf("Failed to open log file: %v", err)
	}
	log.SetOutput(io.MultiWriter(os.Stdout, logFile))
	log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})
}

func setupGracefulShutdown(server *http.Server, quit chan os.Signal, ctxCancel context.CancelFunc) {
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-quit
		log.Infof("Received signal %s. Initiating shutdown...", sig)

		if err := server.Shutdown(context.Background()); err != nil {
			log.Fatalf("Server Shutdown: %v", err)
		}

		ctxCancel()
	}()
}

func setDefaults() {
	conf.LogLevel = "info"
	conf.ListenPort = "8080"
}

func loadConfig() error {
	viper.SetConfigFile("./config.toml")
	viper.AutomaticEnv()
	if err := viper.ReadInConfig(); err != nil {
		return fmt.Errorf("error reading config file: %w", err)
	}
	if err := viper.Unmarshal(&conf); err != nil {
		return fmt.Errorf("unable to decode config: %w", err)
	}
	return nil
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
