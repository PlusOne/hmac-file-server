
package middleware

import (
	"net/http"
	"pkg/file"
)

func handleRequest(w http.ResponseWriter, r *http.Request) {
	file.HandleRequest(w, r)
}
package middleware

import (
	"net/http"

	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/sirupsen/logrus"
	"pkg/config"
	"pkg/metrics"
)

func SetupRouter() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRequest)
	if config.Conf.Server.MetricsEnabled {
		metrics.IncHTTPRequests(r.Method, r.URL.Path)
		next.ServeHTTP(w, r)
	})
}

func recoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if err := recover(); err != nil {
				logrus.Errorf("Recovered from panic: %v", err)
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
}

func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		if r.Method == http.MethodOptions {
			w.WriteHeader(http.StatusNoContent)
			return
		}
		next.ServeHTTP(w, r)
	})
}

// ...handleRequest function moved to file package...
		mux.Handle("/metrics", promhttp.Handler())

func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
	}
	handler := loggingMiddleware(mux)
	handler = recoveryMiddleware(handler)
	handler = corsMiddleware(handler)
	return handler
}