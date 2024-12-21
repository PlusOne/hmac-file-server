package redisutils

import (
	"context"
	"time"

	"github.com/go-redis/redis/v8"
	"github.com/renz/hmac-file-server/internal/config"
	"github.com/sirupsen/logrus"
)

func InitRedis(conf *config.Config) (*redis.Client, error) {
	if !conf.Redis.RedisEnabled {
		return nil, nil
	}

	client := redis.NewClient(&redis.Options{
		Addr:     conf.Redis.RedisAddr,
		Password: conf.Redis.RedisPassword,
		DB:       conf.Redis.RedisDBIndex,
	})

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if err := client.Ping(ctx).Err(); err != nil {
		logrus.Errorf("Failed to connect to Redis: %v", err)
		return nil, err
	}

	logrus.Info("Connected to Redis successfully.")
	return client, nil
}

// ...additional Redis utilities...
