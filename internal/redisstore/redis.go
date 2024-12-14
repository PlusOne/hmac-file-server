package redisstore

import (
	"context"
	"time"

	"github.com/go-redis/redis/v8"
	"github.com/sirupsen/logrus"
)

var (
	RedisClient    *redis.Client
	RedisConnected bool
)

func InitRedis(confAddr, confPassword string, dbIndex int, log *logrus.Logger) {
	RedisClient = redis.NewClient(&redis.Options{
		Addr:     confAddr,
		Password: confPassword,
		DB:       dbIndex,
	})

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	_, err := RedisClient.Ping(ctx).Result()
	if err != nil {
		log.Fatalf("Failed to connect to Redis: %v", err)
	}
	log.Info("Connected to Redis successfully")

	RedisConnected = true
}

func MonitorRedisHealth(ctx context.Context, client *redis.Client, checkInterval time.Duration, log *logrus.Logger) {
	ticker := time.NewTicker(checkInterval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping Redis health monitor.")
			return
		case <-ticker.C:
			err := client.Ping(ctx).Err()
			if err != nil {
				if RedisConnected {
					log.Errorf("Redis health check failed: %v", err)
				}
				RedisConnected = false
			} else {
				if !RedisConnected {
					log.Info("Redis reconnected successfully")
				}
				RedisConnected = true
				log.Debug("Redis health check succeeded.")
			}
		}
	}
}
