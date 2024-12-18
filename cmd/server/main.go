package main

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"time"

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"github.com/patrickmn/go-cache"
	"github.com/renz/hmac-file-server/config"
	"github.com/renz/hmac-file-server/handlers"
	"github.com/renz/hmac-file-server/utils"
	"github.com/sirupsen/logrus"
	"github.com/shirou/gopsutil/v3/cpu"     // Updated import
	"github.com/shirou/gopsutil/v3/mem"     // Updated import
	"github.com/shirou/gopsutil/v3/disk"    // Updated import
	"github.com/shirou/gopsutil/v3/host"    // Updated import
)

var (
	fileInfoCache *cache.Cache
	clamClient    *clamd.Clamd
	redisClient   *redis.Client
	redisCtx      = context.Background()
	versionString = "v1.0.0" // Define the version string
)

func main() {
	conf, err := config.LoadConfig("config.toml")
	if err != nil {
		logrus.Fatalf("Error loading configuration: %v", err)
	}
	logrus.Info("Configuration loaded successfully.")

	utils.SetupLogging(conf.Server.LogLevel, conf.Server.LogFile)

	utils.LogSystemInfo(versionString) // Log system information

	// Initialize HMAC worker pool
	numHMACWorkers := conf.Workers.NumWorkers
	if conf.Server.AutoAdjustWorkers {
		numHMACWorkers = utils.AutoAdjustWorkers()
		conf.Workers.NumWorkers = numHMACWorkers // Update config with adjusted value
	}
	handlers.HMACWorkerPool = make(chan struct{}, numHMACWorkers)
	logrus.Infof("HMAC worker pool initialized with %d workers.", numHMACWorkers)

	if conf.ClamAV.ClamAVEnabled {
		clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			logrus.Fatalf("Failed to initialize ClamAV: %v", err)
		}
		logrus.Info("ClamAV initialized successfully.")
		handlers.ClamAVClient = clamClient // Pass ClamAV client to handlers

		// Initialize ClamAV worker pool
		numClamAVWorkers := conf.ClamAV.NumScanWorkers
		if conf.Server.AutoAdjustWorkers {
			numClamAVWorkers = utils.AutoAdjustClamAVWorkers()
			conf.ClamAV.NumScanWorkers = numClamAVWorkers // Update config with adjusted value
		}
		handlers.ClamAVWorkerPool = make(chan struct{}, numClamAVWorkers)
		logrus.Infof("ClamAV worker pool initialized with %d workers.", numClamAVWorkers)
	}

	if conf.Redis.RedisEnabled {
		redisClient = initRedis(conf.Redis)
		if redisClient == nil {
			logrus.Fatalf("Failed to initialize Redis client.")
		}
		logrus.Info("Redis client initialized successfully.")
		handlers.RedisClient = redisClient // Pass Redis client to handlers
	} else {
		logrus.Info("Redis is not enabled. Using fallback mechanisms.")
		// Initialize in-memory cache or other fallback mechanisms
		handlers.InMemoryCache = cache.New(5*time.Minute, 10*time.Minute)
	}

	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)

	if conf.ISO.Enabled {
		logrus.Infof("ISO storage enabled. Mount point: %s, Charset: %s", conf.ISO.MountPoint, conf.ISO.Charset)
	}

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Start the cleanup routine
	go utils.CleanupExpiredFiles(ctx, conf)

	router := handlers.SetupRouter(conf)

	// Handle errors returned by ParseDuration when setting server timeouts
	readTimeout, err := utils.ParseDuration(conf.Timeouts.ReadTimeout)
	if err != nil {
		logrus.Fatalf("Invalid ReadTimeout: %v", err)
	}

	writeTimeout, err := utils.ParseDuration(conf.Timeouts.WriteTimeout)
	if err != nil {
		logrus.Fatalf("Invalid WriteTimeout: %v", err)
	}

	idleTimeout, err := utils.ParseDuration(conf.Timeouts.IdleTimeout)
	if err != nil {
		logrus.Fatalf("Invalid IdleTimeout: %v", err)
	}

	server := &http.Server{
		Addr:         ":" + conf.Server.ListenPort,
		Handler:      router,
		ReadTimeout:  readTimeout,
		WriteTimeout: writeTimeout,
		IdleTimeout:  idleTimeout,
	}

	utils.SetupGracefulShutdown(server, ctx, cancel)

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
}

func initClamAV(socket string) (*clamd.Clamd, error) {
	client := clamd.NewClamd(socket)
	response, err := client.Ping()
	if err != nil {
		return nil, fmt.Errorf("failed to ping ClamAV: %w", err)
	}
	select {
	case res := <-response:
		if res.Status != "PONG" {
			return nil, fmt.Errorf("unexpected ClamAV ping response: %s", res.Status)
		}
	case <-time.After(5 * time.Second):
		return nil, fmt.Errorf("timeout while pinging ClamAV")
	}
	return client, nil
}

func initRedis(conf config.RedisConfig) *redis.Client {
	rdb := redis.NewClient(&redis.Options{
		Addr:     conf.RedisAddr,
		Password: conf.RedisPassword,
		DB:       conf.RedisDBIndex,
	})
	_, err := rdb.Ping(redisCtx).Result()
	if err != nil {
		logrus.WithFields(logrus.Fields{
			"error": err,
		}).Error("Redis ping failed")
		return nil
	}
	return rdb
}