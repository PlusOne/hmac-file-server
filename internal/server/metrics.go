func initMetrics() {
	uploadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{Namespace: "hmac", Name: "file_server_upload_duration_seconds", Help: "Histogram of file upload duration in seconds.", Buckets: prometheus.DefBuckets})
func updateSystemMetrics(ctx context.Context) {
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			v, _ := mem.VirtualMemory()
			memoryUsage.Set(float64(v.Used) / float64(v.Total) * 100)
			c, _ := cpu.Percent(0, false)
			if len(c) > 0 {
				cpuUsage.Set(c[0])
			}
