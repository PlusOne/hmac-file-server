package handlers

import (
	"net/http"

	"github.com/renz/hmac-file-server/config"   // Local project imports
	"github.com/renz/hmac-file-server/utils"    // Local project imports
	"github.com/sirupsen/logrus"                // Third-party imports
)

func SetupRouter(conf *config.Config) http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRequest(conf))
	if conf.Server.MetricsEnabled {
		mux.Handle("/metrics", utils.PrometheusHandler())
	}
	handler := LoggingMiddleware(mux)
	handler = RecoveryMiddleware(handler)
	handler = CORSMiddleware(handler)
	return handler
}

func handleRequest(conf *config.Config) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method == http.MethodPost && utils.IsMultipart(r.Header.Get("Content-Type")) {
			handleUpload(w, r, conf)
			return
		}

		// ...existing request handling code...

		logrus.WithFields(logrus.Fields{
			"method": r.Method,
			"url":    r.URL.String(),
			"remote": utils.GetClientIP(r),
		}).Info("Incoming request")

		// ...additional routing logic...
	}
}

func handleUpload(w http.ResponseWriter, r *http.Request, conf *config.Config) {
	// ...implementation...
}

func handleDownload(w http.ResponseWriter, r *http.Request, conf *config.Config) {
	// ...implementation...
}

// LoggingMiddleware logs each incoming HTTP request.
func LoggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logrus.WithFields(logrus.Fields{
			"method": r.Method,
			"url":    r.URL.String(),
			"remote": utils.GetClientIP(r),
		}).Info("Incoming request")
		next.ServeHTTP(w, r)
	})
}

// RecoveryMiddleware recovers from any panics and writes a 500 error.
func RecoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if rec := recover(); rec != nil {
				logrus.Errorf("Panic recovered: %v", rec)
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
}

// CORSMiddleware handles Cross-Origin Resource Sharing (CORS).
func CORSMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Adjust the allowed origins, methods, and headers as needed
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")

		// Handle preflight requests
		if r.Method == http.MethodOptions {
			return
		}

		next.ServeHTTP(w, r)
	})
}

// ...other handler functions...