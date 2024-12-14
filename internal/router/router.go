// internal/router/router.go

package router

import (
	"net/http"
	"strings"

	"your-project/internal/config"
	"your-project/internal/logging"
	"your-project/internal/uploads"
	"your-project/internal/downloads"

	"github.com/prometheus/client_golang/prometheus/promhttp"
)

func SetupRouter() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRequest)

	// Start metrics server if enabled
	if config.Conf.Server.MetricsEnabled {
		mux.Handle("/metrics", promhttp.Handler())
	}

	// Apply middleware
	handler := loggingMiddleware(mux)
	handler = recoveryMiddleware(handler)
	handler = corsMiddleware(handler)
	return handler
}

// Middleware für Logging
func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requestsTotal.WithLabelValues(r.Method, r.URL.Path).Inc()
		next.ServeHTTP(w, r)
	})
}

// Middleware für Panic Recovery
func recoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if rec := recover(); rec != nil {
				logging.Log.WithFields(logrus.Fields{
					"method": r.Method,
					"url":    r.URL.String(),
					"error":  rec,
				}).Error("Panic recovered in HTTP handler")
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
}

// Middleware für CORS
func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}
		next.ServeHTTP(w, r)
	})
}

// handleRequest ist der Haupt-Handler für alle Anfragen
func handleRequest(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost && strings.Contains(r.Header.Get("Content-Type"), "multipart/form-data") {
		absFilename, err := storage.SanitizeFilePath(config.Conf.Server.StoragePath, strings.TrimPrefix(r.URL.Path, "/"))
		if err != nil {
			logging.Log.WithError(err).Error("Invalid file path")
			http.Error(w, "Invalid file path", http.StatusBadRequest)
			return
		}
		uploads.HandleUpload(w, r, absFilename)
		return
	}

	// Hier kannst du weitere Methoden wie GET, HEAD, etc. handhaben
}
