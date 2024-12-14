// internal/redis/redis.go

package redis

import (
	"context"
	"fmt"
	"time"

	"your-project/internal/config"
	"your-project/internal/logging"

	"github.com/go-redis/redis/v8"
)

var (
	RedisClient     *redis.Client
	RedisConnected  bool
	mu              sync.RWMutex
)

// InitRedis initialisiert den Redis-Client
func InitRedis() {
	if !config.Conf.Redis.RedisEnabled {
		logging.Log.Info("Redis is disabled in configuration.")
		return
	}

	RedisClient = redis.NewClient(&redis.Options{
		Addr:     config.Conf.Redis.RedisAddr,
		Password: config.Conf.Redis.RedisPassword,
		DB:       config.Conf.Redis.RedisDBIndex,
	})

	// Test the Redis connection
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	_, err := RedisClient.Ping(ctx).Result()
	if err != nil {
		logging.Log.Fatalf("Failed to connect to Redis: %v", err)
	}
	logging.Log.Info("Connected to Redis successfully")

	// Set initial connection status
	mu.Lock()
	RedisConnected = true
	mu.Unlock()

	// Start monitoring Redis health
	go MonitorRedisHealth(context.Background(), RedisClient, ParseDuration(config.Conf.Redis.RedisHealthCheckInterval))
}

// MonitorRedisHealth überwacht periodisch die Redis-Verbindung
func MonitorRedisHealth(ctx context.Context, client *redis.Client, checkInterval time.Duration) {
	ticker := time.NewTicker(checkInterval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			logging.Log.Info("Stopping Redis health monitor.")
			return
		case <-ticker.C:
			err := client.Ping(ctx).Err()
			mu.Lock()
			if err != nil {
				if RedisConnected {
					logging.Log.Errorf("Redis health check failed: %v", err)
				}
				RedisConnected = false
			} else {
				if !RedisConnected {
					logging.Log.Info("Redis reconnected successfully")
				}
				RedisConnected = true
				logging.Log.Debug("Redis health check succeeded.")
			}
			mu.Unlock()
		}
	}
}

// ParseDuration konvertiert eine Dauerzeichenkette in eine time.Duration
func ParseDuration(durationStr string) time.Duration {
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		logging.Log.WithError(err).Warn("Invalid duration format, using default 30s")
		return 30 * time.Second
	}
	return duration
}
