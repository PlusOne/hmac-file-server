package main

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"time"
	"os"
	"strconv"

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"github.com/renz/hmac-file-server/config"   // Corrected import path
	"github.com/renz/hmac-file-server/handlers" // Corrected import path
	"github.com/renz/hmac-file-server/utils"    // Corrected import path
	"github.com/sirupsen/logrus"
	"github.com/patrickmn/go-cache" // Added import for in-memory cache
)

var (
	versionString = "v1.0.0" // Define the version string
)

func main() {
	conf, err := config.LoadConfig("config.toml")
	if err != nil {
		logrus.Fatalf("Error loading configuration: %v", err)
	}
	
	logrus.Info("Configuration loaded successfully.")

	utils.SetupLogging(conf.Server.LogLevel, conf.Server.LogFile, conf.Server.LoggingJSON)

	utils.LogSystemInfo(versionString) // Log system information

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Initialize HMAC worker pool without using global variables
	logrus.Infof("HMAC worker pool initialized with %d workers.", conf.Workers.NumWorkers)

	// Initialize HMAC worker pool
	HMACWorkerPool := make(chan struct{}, conf.Workers.NumWorkers) // Added initialization
	for i := 0; i < conf.Workers.NumWorkers; i++ {
		HMACWorkerPool <- struct{}{}
	}

	// Initialize ClamAV worker pool
	clamAVWorkers := utils.AutoAdjustClamAVWorkers()
	ClamAVWorkerPool := make(chan struct{}, clamAVWorkers) // Added initialization
	for i := 0; i < clamAVWorkers; i++ {
		ClamAVWorkerPool <- struct{}{}
	}

	// Initialize ClamAV and Redis clients as needed
	var redisClient *redis.Client

	if conf.ClamAV.ClamAVEnabled {
		if _, err := initClamAV(conf.ClamAV.ClamAVSocket); err != nil {
			logrus.Fatalf("Failed to initialize ClamAV: %v", err)
		}
		logrus.Info("ClamAV initialized successfully.")
	}

	if conf.Redis.RedisEnabled {
		redisClient = initRedis(ctx, conf.Redis)
		if redisClient == nil {
			logrus.Fatalf("Failed to initialize Redis client.")
		}
		logrus.Info("Redis client initialized successfully.")
	} else {
		logrus.Info("Redis is not enabled. Using fallback mechanisms.")
	}

	// Initialize caches
	var inMemoryCache *cache.Cache
	if !conf.Redis.RedisEnabled {
		inMemoryCache = cache.New(cache.DefaultExpiration, cache.DefaultExpiration)
	}

	// Setup Handler Dependencies
	var ClamAVClient *clamd.Clamd
	if conf.ClamAV.ClamAVEnabled {
		client, err := initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			logrus.Fatalf("Failed to initialize ClamAV: %v", err)
		}
		ClamAVClient = client
	}

	handlerDeps := &handlers.HandlerDependencies{
		RedisClient:      redisClient,
		InMemoryCache:    inMemoryCache,
		ClamAVClient:     ClamAVClient, // Ensure ClamAVClient is initialized
		HMACWorkerPool:   HMACWorkerPool,
		ClamAVWorkerPool: ClamAVWorkerPool,
	}

	// Start the cleanup routine with error handling
	go utils.CleanupExpiredFiles(ctx, conf.Server.StoragePath, conf.Server.FileTTL)

	router := handlers.SetupRouter(conf, handlerDeps)

	// Configure server timeouts
	server := &http.Server{
		Addr:         ":" + conf.Server.ListenPort,
		Handler:      router,
		ReadTimeout:  utils.ParseDurationOrDefault(conf.Timeouts.ReadTimeout, 5*time.Second),
		WriteTimeout: utils.ParseDurationOrDefault(conf.Timeouts.WriteTimeout, 10*time.Second),
		IdleTimeout:  utils.ParseDurationOrDefault(conf.Timeouts.IdleTimeout, 120*time.Second),
	}

	utils.SetupGracefulShutdown(server, ctx, cancel)

	// PID handling
	cleanupPID, err := utils.ManagePID(conf.Server.PidFilePath)
	if err != nil {
		logrus.Fatalf("PID management failed: %v", err)
	}
	if conf.Server.CleanupOnExit && cleanupPID != nil {
		defer cleanupPID()
	}

	// Start the server
	logrus.Infof("Starting HMAC File Server on port %s...", conf.Server.ListenPort)
	if conf.Server.UnixSocket {
		listener, err := net.Listen("unix", conf.Server.ListenPort)
		if err != nil {
			logrus.Fatalf("Failed to listen on Unix socket: %v", err)
		}
		defer listener.Close()
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			logrus.Fatalf("Server error: %v", err)
		}
	} else {
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			logrus.Fatalf("Server error: %v", err)
		}
	}

	// Wait for context cancellation
	<-ctx.Done()

	// Close Redis client if initialized
	if redisClient != nil {
		if err := redisClient.Close(); err != nil {
			logrus.Errorf("Error closing Redis client: %v", err)
		} else {
			logrus.Info("Redis client closed successfully.")
		}
	}

	logrus.Info("Server shutdown complete.")
}

// Add the writePidToFile function
func writePidToFile(filename string, pid int) error {
	file, err := os.Create(filename)
	if (err != nil) {
		return err
	}
	defer file.Close()

	_, err = file.WriteString(strconv.Itoa(pid))
	if err != nil {
		return err
	}

	return nil
}

func initClamAV(socket string) (*clamd.Clamd, error) {
	client := clamd.NewClamd(socket)
	err := client.Ping()
	if err != nil {
		return nil, fmt.Errorf("error pinging ClamAV: %v", err)
	}
	return client, nil
}

func initRedis(ctx context.Context, conf config.RedisConfig) *redis.Client {
	rdb := redis.NewClient(&redis.Options{
		Addr:     conf.RedisAddr,
		Password: conf.RedisPassword,
		DB:       conf.RedisDBIndex,
	})
	_, err := rdb.Ping(ctx).Result()
	if err != nil {
		logrus.WithFields(logrus.Fields{
			"error": err,
		}).Error("Redis ping failed")
		return nil
	}
	return rdb
}