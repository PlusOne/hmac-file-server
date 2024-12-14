package redis
import ( 
        // TODO: Add required imports here 
)
func initRedis() {
	if !conf.Redis.RedisEnabled {
		log.Info("Redis is disabled in configuration.")
		return
	}

	redisClient = redis.NewClient(&redis.Options{
		Addr:     conf.Redis.RedisAddr,
		Password: conf.Redis.RedisPassword,
		DB:       conf.Redis.RedisDBIndex,
	})

	// Test the Redis connection
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	_, err := redisClient.Ping(ctx).Result()
	if err != nil {
		log.Fatalf("Failed to connect to Redis: %v", err)
	}
	log.Info("Connected to Redis successfully")

	// Set initial connection status
	mu.Lock()
	redisConnected = true
	mu.Unlock()

	// Start monitoring Redis health
	go MonitorRedisHealth(context.Background(), redisClient, parseDuration(conf.Redis.RedisHealthCheckInterval))
}
func MonitorRedisHealth(ctx context.Context, client *redis.Client, checkInterval time.Duration) {
	ticker := time.NewTicker(checkInterval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping Redis health monitor.")
			return
		case <-ticker.C:
			err := client.Ping(ctx).Err()
			mu.Lock()
			if err != nil {
				if redisConnected {
					log.Errorf("Redis health check failed: %v", err)
				}
				redisConnected = false
			} else {
				if !redisConnected {
					log.Info("Redis reconnected successfully")
				}
				redisConnected = true
				log.Debug("Redis health check succeeded.")
			}
			mu.Unlock()
		}
	}
}
