package main

import (
	"context"
	"flag"
	"fmt"
	"net"
	"net/http"
	"os"
	"time"

	"github.com/patrickmn/go-cache"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/sirupsen/logrus"

	"github.com/PlusOne/hmac-file-server/internal/config"
	"github.com/PlusOne/hmac-file-server/internal/iso"
	"github.com/PlusOne/hmac-file-server/internal/logging"
	"github.com/PlusOne/hmac-file-server/internal/metrics"
	"github.com/PlusOne/hmac-file-server/internal/network"
	"github.com/PlusOne/hmac-file-server/internal/redis"
	"github.com/PlusOne/hmac-file-server/internal/router"
	"github.com/PlusOne/hmac-file-server/internal/scanning"
	"github.com/PlusOne/hmac-file-server/internal/storage"
	"github.com/PlusOne/hmac-file-server/internal/uploads"
)

var (
	fileInfoCache *cache.Cache
)

func main() {
	// Set default configuration values
	config.SetDefaults()

	// Flags for configuration file
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	// Load configuration
	err := config.ReadConfig(configFile, &config.Conf)
	if err != nil {
		logging.Log.Fatalf("Error reading config: %v", err) // Fatal: application cannot proceed
	}
	logging.Log.Info("Configuration loaded successfully.")

	// Verify and create ISO container if it doesn't exist
	if config.Conf.ISO.Enabled {
		err = iso.VerifyAndCreateISOContainer()
		if err != nil {
			logging.Log.Fatalf("ISO container verification failed: %v", err)
		}
	}

	// Initialize file info cache
	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)

	// Create store directory
	err = os.MkdirAll(config.Conf.Server.StoragePath, os.ModePerm)
	if err != nil {
		logging.Log.Fatalf("Error creating store directory: %v", err)
	}
	logging.Log.WithField("directory", config.Conf.Server.StoragePath).Info("Store directory is ready")

	// Check free space with retry
	err = storage.CheckFreeSpaceWithRetry(config.Conf.Server.StoragePath, 3, 5*time.Second)
	if err != nil {
		logging.Log.Fatalf("Insufficient free space: %v", err)
	}

	// Setup logging
	logging.SetupLogging()

	// Log system information
	logging.LogSystemInfo()

	// Initialize Prometheus metrics
	metrics.InitMetrics()
	logging.Log.Info("Prometheus metrics initialized.")

	// Initialize upload and scan queues
	uploads.UploadQueue = make(chan uploads.UploadTask, config.Conf.Workers.UploadQueueSize)
	logging.Log.Infof("Upload queue initialized with size: %d", config.Conf.Workers.UploadQueueSize)
	scanning.ScanQueue = make(chan scanning.ScanTask, config.Conf.Workers.UploadQueueSize)
	network.NetworkEvents = make(chan network.NetworkEvent, 100)
	logging.Log.Info("Upload, scan, and network event channels initialized.")

	// Context for goroutines
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Start network monitoring
	go network.MonitorNetwork(ctx)
	go network.HandleNetworkEvents(ctx)

	// Update system metrics
	go metrics.UpdateSystemMetrics(ctx)

	// Initialize ClamAV client if enabled
	if config.Conf.ClamAV.ClamAVEnabled {
		scanning.ClamClient, err = scanning.InitClamAV(config.Conf.ClamAV.ClamAVSocket)
		if err != nil {
			logrus.WithFields(logrus.Fields{
				"error": err.Error(),
			}).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
		} else {
			logging.Log.Info("ClamAV client initialized successfully.")
		}
	}

	// Initialize Redis client if enabled
	if config.Conf.Redis.RedisEnabled {
		redis.InitRedis()
	}

	// Redis Initialization
	redis.InitRedis()
	logging.Log.Info("Redis client initialized and connected successfully.")

	// ClamAV Initialization
	if config.Conf.ClamAV.ClamAVEnabled {
		scanning.ClamClient, err = scanning.InitClamAV(config.Conf.ClamAV.ClamAVSocket)
		if err != nil {
			logrus.WithFields(logrus.Fields{
				"error": err.Error(),
			}).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
		} else {
			logging.Log.Info("ClamAV client initialized successfully.")
		}
	}

	// Initialize worker pools
	uploads.InitializeUploadWorkerPool(ctx)
	if config.Conf.ClamAV.ClamAVEnabled && scanning.ClamClient != nil {
		scanning.InitializeScanWorkerPool(ctx)
	}

	// Start Redis health monitor if Redis is enabled
	if config.Conf.Redis.RedisEnabled && redis.RedisClient != nil {
		go redis.MonitorRedisHealth(ctx, redis.RedisClient, redis.ParseDuration(config.Conf.Redis.RedisHealthCheckInterval))
	}

	// Setup router
	r := router.SetupRouter()

	// Start file cleaner
	fileTTL, err := time.ParseDuration(config.Conf.Server.FileTTL)
	if err != nil {
		logging.Log.Fatalf("Invalid FileTTL: %v", err)
	}
	go storage.RunFileCleaner(ctx, config.Conf.Server.StoragePath, fileTTL)

	// Parse timeout durations
	readTimeout, err := time.ParseDuration(config.Conf.Timeouts.ReadTimeout)
	if err != nil {
		logging.Log.Fatalf("Invalid ReadTimeout: %v", err)
	}

	writeTimeout, err := time.ParseDuration(config.Conf.Timeouts.WriteTimeout)
	if err != nil {
		logging.Log.Fatalf("Invalid WriteTimeout: %v", err)
	}

	idleTimeout, err := time.ParseDuration(config.Conf.Timeouts.IdleTimeout)
	if err != nil {
		logging.Log.Fatalf("Invalid IdleTimeout: %v", err)
	}

	// Configure HTTP server
	server := &http.Server{
		Addr:         ":" + config.Conf.Server.ListenPort, // Prepend colon to ListenPort
		Handler:      r,
		ReadTimeout:  readTimeout,
		WriteTimeout: writeTimeout,
		IdleTimeout:  idleTimeout,
	}

	// Start metrics server if enabled
	if config.Conf.Server.MetricsEnabled {
		go func() {
			http.Handle("/metrics", promhttp.Handler())
			logging.Log.Infof("Metrics server started on port %s", config.Conf.Server.MetricsPort)
			if err := http.ListenAndServe(":"+config.Conf.Server.MetricsPort, nil); err != nil {
				logging.Log.Fatalf("Metrics server failed: %v", err)
			}
		}()
	}

	// Setup graceful shutdown
	storage.SetupGracefulShutdown(server, cancel)

	// Start server
	logging.Log.Infof("Starting HMAC file server %s...", config.VersionString)
	if config.Conf.Server.UnixSocket {
		// Listen on Unix socket
		if err := os.RemoveAll(config.Conf.Server.ListenPort); err != nil {
			logging.Log.Fatalf("Failed to remove existing Unix socket: %v", err)
		}
		listener, err := net.Listen("unix", config.Conf.Server.ListenPort)
		if err != nil {
			logging.Log.Fatalf("Failed to listen on Unix socket %s: %v", config.Conf.Server.ListenPort, err)
		}
		defer listener.Close()
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			logging.Log.Fatalf("Server failed: %v", err)
		}
	} else {
		// Listen on TCP port
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			logging.Log.Fatalf("Server failed: %v", err)
		}
	}

	// Example files to include in the ISO container
	files := []string{"file1.txt", "file2.txt"}
	isoPath := "/path/to/container.iso"

	// Create ISO container
	err = iso.CreateISOContainer(files, isoPath, config.Conf.ISO.Size, config.Conf.ISO.Charset)
	if err != nil {
		fmt.Printf("Failed to create ISO container: %v\n", err)
		return
	}

	// Mount ISO container
	err = iso.MountISOContainer(isoPath, config.Conf.ISO.MountPoint)
	if err != nil {
		fmt.Printf("Failed to mount ISO container: %v\n", err)
		return
	}

	// Unmount ISO container (example)
	err = iso.UnmountISOContainer(config.Conf.ISO.MountPoint)
	if err != nil {
		fmt.Printf("Failed to unmount ISO container: %v\n", err)
		return
	}
}
