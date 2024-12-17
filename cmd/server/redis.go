package main

import (
    "context"
    "fmt"
    "github.com/go-redis/redis/v8"
    "time"
)

var redisClient *redis.Client

// initRedis initializes the Redis client
func initRedis() {
    redisClient = redis.NewClient(&redis.Options{
        Addr:     conf.Redis.RedisAddr,
        Password: conf.Redis.RedisPassword,
        DB:       conf.Redis.RedisDBIndex,
    })

    ctx := context.Background()
    if _, err := redisClient.Ping(ctx).Result(); err != nil {
        log.Fatalf("Failed to connect to Redis: %v", err)
    }
    log.Info("Connected to Redis successfully")
}

// MonitorRedisHealth periodically checks Redis health
func MonitorRedisHealth(ctx context.Context, client *redis.Client, interval time.Duration) {
    ticker := time.NewTicker(interval)
    defer ticker.Stop()

    for {
        select {
        case <-ctx.Done():
            log.Info("Stopping Redis health monitoring")
            return
        case <-ticker.C:
            if _, err := client.Ping(ctx).Result(); err != nil {
                log.Errorf("Redis health check failed: %v", err)
            } else {
                log.Info("Redis health check successful")
            }
        }
    }
}

// SetRedisValue sets a key-value pair in Redis
func SetRedisValue(ctx context.Context, key string, value string, ttl time.Duration) error {
    err := redisClient.Set(ctx, key, value, ttl).Err()
    if err != nil {
        return fmt.Errorf("failed to set Redis key %s: %w", key, err)
    }
    log.Infof("Set Redis key: %s", key)
    return nil
}

// GetRedisValue retrieves a value from Redis
func GetRedisValue(ctx context.Context, key string) (string, error) {
    value, err := redisClient.Get(ctx, key).Result()
    if err == redis.Nil {
        log.Infof("Redis key not found: %s", key)
        return "", nil
    } else if err != nil {
        return "", fmt.Errorf("failed to get Redis key %s: %w", key, err)
    }
    log.Infof("Retrieved Redis key: %s", key)
    return value, nil
}

// DeleteRedisKey deletes a key from Redis
func DeleteRedisKey(ctx context.Context, key string) error {
    _, err := redisClient.Del(ctx, key).Result()
    if err != nil {
        return fmt.Errorf("failed to delete Redis key %s: %w", key, err)
    }
    log.Infof("Deleted Redis key: %s", key)
    return nil
}
