package handlers

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"mime"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"time"
	"fmt"

	"github.com/go-redis/redis/v8"
	"github.com/renz/hmac-file-server/config"
	"github.com/renz/hmac-file-server/utils"
	"github.com/sirupsen/logrus"
)

var redisClient *redis.Client
var redisCtx = context.Background()

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

// handleUpload handles PUT requests for file uploads with HMAC validation.
func handleUpload(w http.ResponseWriter, r *http.Request, conf *config.Config) {
	absFilename := r.URL.Query().Get("file")
	if absFilename == "" {
		http.Error(w, "File parameter is missing", http.StatusBadRequest)
		return
	}

	fileStorePath := filepath.Join(conf.Server.StoragePath, absFilename)
	a := r.URL.Query()

	logrus.Infof("Using storage path: %s", conf.Server.StoragePath)

	// HMAC validation
	var protocolVersion string
	if a.Get("v2") != "" {
		protocolVersion = "v2"
	} else if a.Get("token") != "" {
		protocolVersion = "token"
	} else if a.Get("v") != "" {
		protocolVersion = "v"
	} else {
		logrus.Warn("No HMAC attached to URL.")
		http.Error(w, "No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC", http.StatusForbidden)
		return
	}

	mac := hmac.New(sha256.New, []byte(conf.Security.Secret))

	if protocolVersion == "v" {
		mac.Write([]byte(fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)))
	} else {
		contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
		if contentType == "" {
			contentType = "application/octet-stream"
		}
		mac.Write([]byte(fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10) + "\x00" + contentType))
	}

	calculatedMAC := mac.Sum(nil)

	providedMACHex := a.Get(protocolVersion)
	providedMAC, err := hex.DecodeString(providedMACHex)
	if err != nil {
		logrus.Warn("Invalid MAC encoding")
		http.Error(w, "Invalid MAC encoding", http.StatusForbidden)
		return
	}

	if !hmac.Equal(calculatedMAC, providedMAC) {
		logrus.Warn("Invalid MAC")
		http.Error(w, "Invalid MAC", http.StatusForbidden)
		return
	}

	if !isExtensionAllowed(fileStorePath) {
		logrus.Warn("Invalid file extension")
		http.Error(w, "Invalid file extension", http.StatusBadRequest)
		// uploadErrorsTotal.Inc() // Uncomment if using Prometheus metrics
		return
	}

	minFreeBytes, err := parseSize(conf.Server.MinFreeBytes)
	if err != nil {
		logrus.Fatalf("Invalid MinFreeBytes: %v", err)
	}
	err = checkStorageSpace(conf.Server.StoragePath, minFreeBytes)
	if err != nil {
		logrus.Warn("Not enough free space")
		http.Error(w, "Not enough free space", http.StatusInsufficientStorage)
		// uploadErrorsTotal.Inc() // Uncomment if using Prometheus metrics
		return
	}

	// ...existing file upload handling code...
}

// isExtensionAllowed checks if the file extension is allowed.
func isExtensionAllowed(filePath string) bool {
	allowedExtensions := []string{".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"}
	ext := filepath.Ext(filePath)
	for _, allowedExt := range allowedExtensions {
		if ext == allowedExt {
			return true
		}
	}
	return false
}

// parseSize parses a size string (e.g., "100MB") into bytes.
func parseSize(sizeStr string) (int64, error) {
	sizeStr = strings.ToUpper(sizeStr)
	var multiplier int64 = 1
	if strings.HasSuffix(sizeStr, "KB") {
		multiplier = 1024
		sizeStr = strings.TrimSuffix(sizeStr, "KB")
	} else if strings.HasSuffix(sizeStr, "MB") {
		multiplier = 1024 * 1024
		sizeStr = strings.TrimSuffix(sizeStr, "MB")
	} else if strings.HasSuffix(sizeStr, "GB") {
		multiplier = 1024 * 1024 * 1024
		sizeStr = strings.TrimSuffix(sizeStr, "GB")
	}
	size, err := strconv.ParseInt(sizeStr, 10, 64)
	if err != nil {
		return 0, err
	}
	return size * multiplier, nil
}

// checkStorageSpace checks if there is enough free space in the storage path.
func checkStorageSpace(storagePath string, minFreeBytes int64) error {
	var stat syscall.Statfs_t
	if err := syscall.Statfs(storagePath, &stat); err != nil {
		return err
	}
	freeBytes := stat.Bavail * uint64(stat.Bsize)
	if int64(freeBytes) < minFreeBytes {
		return fmt.Errorf("not enough free space: %d bytes available, %d bytes required", freeBytes, minFreeBytes)
	}
	return nil
}

// handleDownload serves the requested file after validating the path.
func handleDownload(conf *config.Config) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		filePath := r.URL.Query().Get("file")
		if filePath == "" {
			http.Error(w, "File parameter is missing", http.StatusBadRequest)
			return
		}

		// Check if the response is cached in Redis
		cachedResponse, err := redisClient.Get(redisCtx, filePath).Result()
		if err == redis.Nil {
			// Cache miss, proceed with file serving
			serveFile(w, r, conf, filePath)
		} else if err != nil {
			logrus.Errorf("Redis error: %v", err)
			http.Error(w, "Internal server error", http.StatusInternalServerError)
		} else {
			// Cache hit, return the cached response
			w.Header().Set("Content-Type", "application/json")
			w.Write([]byte(cachedResponse))
		}
	}
}

func serveFile(w http.ResponseWriter, r *http.Request, conf *config.Config, filePath string) {
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
		logrus.WithFields(logrus.Fields{
			"file":  fullPath,
			"error": err,
		}).Error("Error accessing file")
		return
	}

	http.ServeFile(w, r, fullPath)

	// Cache the response in Redis
	response := map[string]string{"filePath": filePath, "status": "served"}
	responseJSON, _ := json.Marshal(response)
	err = redisClient.Set(redisCtx, filePath, responseJSON, 10*time.Minute).Err()
	if err != nil {
		logrus.Errorf("Failed to cache response in Redis: %v", err)
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
				logrus.WithFields(logrus.Fields{
					"error": rec,
				}).Error("Panic recovered")
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
