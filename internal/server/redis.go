func initRedis() {
	if !conf.Redis.RedisEnabled {
		log.Info("Redis is disabled in configuration.")
		return
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
