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
		handlers.redisClient = redisClient // Pass Redis client to handlers
	}

	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)

	if conf.Server.AutoAdjustWorkers {
		conf.Workers.NumWorkers, conf.Workers.UploadQueueSize = utils.AutoAdjustWorkers()
	}

	router := handlers.SetupRouter(conf)

	server := &http.Server{
		Addr:         ":" + conf.Server.ListenPort,
		Handler:      router,
		ReadTimeout:  utils.ParseDuration(conf.Timeouts.ReadTimeout),
		WriteTimeout: utils.ParseDuration(conf.Timeouts.WriteTimeout),
		IdleTimeout:  utils.ParseDuration(conf.Timeouts.IdleTimeout),
	}

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
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
	err := client.Ping()
	if err != nil {
		return nil, fmt.Errorf("failed to ping ClamAV: %w", err)
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
