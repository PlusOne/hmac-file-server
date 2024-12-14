func setupRouter() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRequest)
	if (conf.Server.MetricsEnabled) {
		mux.Handle("/metrics", promhttp.Handler())
	}
func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requestsTotal.WithLabelValues(r.Method, r.URL.Path).Inc()
		next.ServeHTTP(w, r)
	})
func recoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if rec := recover(); rec != nil {
				log.WithFields(logrus.Fields{
					"method": r.Method,
					"url":    r.URL.String(),
					"error":  rec,
				}).Error("Panic recovered in HTTP handler")
func corsMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        w.Header().Set("Access-Control-Allow-Origin", "*")
        w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
        w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
        if r.Method == "OPTIONS" {
            w.WriteHeader(http.StatusOK)
            return
        }
