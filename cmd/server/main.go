package main

import (
	// Standard library imports
	"context"
	"net"
	"net/http"
	"time"

	// Third-party imports
	"github.com/dutchcoders/go-clamd"   // ClamAV integration
	"github.com/go-redis/redis/v8"      // Redis integration
	"github.com/patrickmn/go-cache"     // In-memory cache
	"github.com/renz/hmac-file-server/config"
	"github.com/renz/hmac-file-server/handlers"
	"github.com/renz/hmac-file-server/utils"
	"github.com/sirupsen/logrus"
	"github.com/shirou/gopsutil/cpu"      // System metrics
	"github.com/shirou/gopsutil/disk"     // System metrics
	"github.com/shirou/gopsutil/host"     // System metrics
	"github.com/shirou/gopsutil/mem"      // System metrics
)

var (
	fileInfoCache *cache.Cache
	clamClient    *clamd.Clamd
	redisClient   *redis.Client
	redisCtx      = context.Background()
)

func main() {
	// Load configuration
	conf, err := config.LoadConfig("config.toml")
	if err != nil {
		logrus.Fatalf("Error loading configuration: %v", err)
	}
	logrus.Info("Configuration loaded successfully.")

	// Setup logging
	utils.SetupLogging(conf.Server.LogLevel, conf.Server.LogFile)

	// Initialize other components (e.g., ClamAV, Redis) as needed
	if conf.ClamAV.ClamAVEnabled {
		clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			logrus.Fatalf("Failed to initialize ClamAV: %v", err)
		}
		logrus.Info("ClamAV initialized successfully.")
	}

	if conf.Redis.RedisEnabled {
		redisClient = initRedis(conf.Redis)
		if redisClient == nil {
			logrus.Fatalf("Failed to initialize Redis client.")
		}
		logrus.Info("Redis client initialized successfully.")
	}

	// Initialize in-memory cache
	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)

	// Initialize HTTP handlers
	router := handlers.SetupRouter(conf)

	// Create HTTP server
	server := &http.Server{
		Addr:         ":" + conf.Server.ListenPort,
		Handler:      router,
		ReadTimeout:  utils.ParseDuration(conf.Timeouts.ReadTimeout),
		WriteTimeout: utils.ParseDuration(conf.Timeouts.WriteTimeout),
		IdleTimeout:  utils.ParseDuration(conf.Timeouts.IdleTimeout),
	}

	// Setup graceful shutdown
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	utils.SetupGracefulShutdown(server, cancel)

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
}

// initClamAV initializes the ClamAV client.
func initClamAV(socket string) (*clamd.Clamd, error) {
	client := clamd.NewClamdSocket(socket)
	if err := client.Ping(); err != nil {
		return nil, err
	}
	return client, nil
}

// initRedis initializes the Redis client.
func initRedis(conf config.RedisConfig) *redis.Client {
	rdb := redis.NewClient(&redis.Options{
		Addr:     conf.RedisAddr,
		Password: conf.RedisPassword,
		DB:       conf.RedisDBIndex,
	})

	_, err := rdb.Ping(redisCtx).Result()
	if err != nil {
		logrus.Errorf("Redis ping failed: %v", err)
		return nil
	}
	return rdb
}