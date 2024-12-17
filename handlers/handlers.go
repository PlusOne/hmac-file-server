package handlers

import (
	"net/http"
	"os"
	"path/filepath"
	"strings"

	"github.com/renz/hmac-file-server/config"
	"github.com/renz/hmac-file-server/utils"
	"github.com/sirupsen/logrus"
)

func SetupRouter(conf *config.Config) http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRequest(conf))
	mux.HandleFunc("/download", handleDownload(conf))
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

		 // ...additional routing logic...
	}
}

func handleUpload(w http.ResponseWriter, r *http.Request, conf *config.Config) {
	// ...implementation...
}

// handleDownload serves the requested file after validating the path.
func handleDownload(conf *config.Config) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		filePath := r.URL.Query().Get("file")
		if filePath == "" {
			http.Error(w, "File parameter is missing", http.StatusBadRequest)
			return
		}

		// Clean the filePath to prevent path traversal
		cleanFilePath := filepath.Clean(filePath)
		fullPath := filepath.Join(conf.Server.StoragePath, cleanFilePath)

		// Ensure that the fullPath is within the StoragePath
		if !strings.HasPrefix(fullPath, filepath.Clean(conf.Server.StoragePath)+string(os.PathSeparator)) {
			http.Error(w, "Invalid file path", http.StatusBadRequest)
			return
		}

		_, err := os.Stat(fullPath)
		if os.IsNotExist(err) {
			http.Error(w, "File not found", http.StatusNotFound)
			return
		} else if err != nil {
			http.Error(w, "Internal server error", http.StatusInternalServerError)
			logrus.Errorf("Error accessing file %s: %v", fullPath, err)
			return
		}

		http.ServeFile(w, r, fullPath)
	}
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
