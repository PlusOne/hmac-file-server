package handlers

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"mime"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"time"

	"github.com/go-redis/redis/v8"
	"github.com/renz/hmac-file-server/config"
	"github.com/renz/hmac-file-server/utils"
	"github.com/sirupsen/logrus"
	"github.com/dutchcoders/go-clamd"
	"github.com/patrickmn/go-cache"
	"github.com/shirou/gopsutil/v3/cpu"    // Updated import
	"github.com/shirou/gopsutil/v3/mem"    // Updated import
	"github.com/shirou/gopsutil/v3/disk"   // Updated import
	"github.com/shirou/gopsutil/v3/host"   // Updated import
)

var RedisClient *redis.Client   // Redis client
var InMemoryCache *cache.Cache  // In-memory cache for fallback
var redisCtx = context.Background()

// Add ClamAV client variable
var ClamAVClient *clamd.Clamd

// Add worker pools for HMAC and ClamAV processing
var HMACWorkerPool chan struct{}
var ClamAVWorkerPool chan struct{}

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
	// Acquire a worker from the HMAC pool
	HMACWorkerPool <- struct{}{}
	defer func() { <-HMACWorkerPool }()

	absFilename := r.URL.Query().Get("file")
	if absFilename == "" {
		http.Error(w, "File parameter is missing", http.StatusBadRequest)
		return
	}

	var fileStorePath string
	if conf.ISO.Enabled {
		fileStorePath = filepath.Join(conf.ISO.MountPoint, absFilename)
	} else {
		fileStorePath = filepath.Join(conf.Server.StoragePath, absFilename)
	}
	a := r.URL.Query()

	logrus.Infof("Using storage path: %s", fileStorePath)

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

	// Compute SHA-256 hash of the uploaded file
	hasher := sha256.New()
	tee := io.TeeReader(r.Body, hasher)

	// Save the uploaded file to a temporary location
	tempFilePath := fileStorePath + ".tmp"
	tempFile, err := os.Create(tempFilePath)
	if err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}
	defer tempFile.Close()
	_, err = io.Copy(tempFile, tee)
	if err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}

	sha256Hash := hex.EncodeToString(hasher.Sum(nil))

	// Check if deduplication is enabled
	if conf.Server.DeduplicationEnabled {
		exists, existingFilePath := checkFileExists(sha256Hash, conf)
		if exists {
			// File already exists, create a hard link
			err := os.Link(existingFilePath, fileStorePath)
			if err != nil {
				logrus.Errorf("Failed to create hard link: %v", err)
				http.Error(w, "Internal server error", http.StatusInternalServerError)
				return
			}
			logrus.Infof("Created hard link for duplicate file: %s", fileStorePath)
			// Remove the temporary file
			os.Remove(tempFilePath)
		} else {
			// Move temp file to final location
			err := os.Rename(tempFilePath, fileStorePath)
			if err != nil {
				http.Error(w, "Internal server error", http.StatusInternalServerError)
				return
			}
			// Store the hash and file path
			storeFileHash(sha256Hash, fileStorePath, conf)
		}
	}

	// ClamAV scanning
	if conf.ClamAV.ClamAVEnabled {
		ext := filepath.Ext(fileStorePath)
		// Check if the file extension should be scanned
		if shouldScanExtension(ext, conf.ClamAV.ScanFileExtensions) {
			logrus.Infof("Scanning file for viruses: %s", fileStorePath)

			// Acquire a worker from the ClamAV pool
			ClamAVWorkerPool <- struct{}{}
			defer func() { <-ClamAVWorkerPool }()

			scanResult, err := scanFileWithClamAV(fileStorePath)
			if err != nil {
				logrus.Errorf("Error scanning file with ClamAV: %v", err)
				http.Error(w, "Internal server error", http.StatusInternalServerError)
				return
			}
			if scanResult.Status == "FOUND" {
				logrus.Warnf("Uploaded file is infected: %s", scanResult.Description)
				http.Error(w, "Uploaded file is infected", http.StatusBadRequest)
				// Remove the infected file
				os.Remove(fileStorePath)
				return
			}
			logrus.Infof("File passed ClamAV scan: %s", fileStorePath)
		} else {
			logrus.Infof("Skipping ClamAV scan for extension: %s", ext)
		}
	}

	// Finalize the upload
	if conf.ISO.Enabled {
		go func(path string) {
			err := createISO(path, conf.ISO.Charset)
			if err != nil {
				logrus.Errorf("Failed to create ISO for %s: %v", path, err)
			} else {
				logrus.Infof("ISO created successfully for %s", path)
			}
		}(fileStorePath)
	}

	// ...existing code to finalize the upload...
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

		exists, cachedResponse := checkFileExists(filePath, conf)
		if exists {
			// Cache hit, serve the cached response
			serveCachedResponse(w, cachedResponse)
		} else {
			// Cache miss, proceed with file serving
			serveFile(w, r, conf, filePath)
		}
	}
}

// serveCachedResponse serves the cached response.
func serveCachedResponse(w http.ResponseWriter, cachedResponse string) {
	// Implement your logic to serve the cached response
	// For example, you might decode the JSON and write appropriate headers
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(cachedResponse))
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

	// If chunked downloads are enabled
	if conf.Downloads.ChunkedDownloadsEnabled {
		// Implement chunked download logic here
		// Example: use http.ServeContent with a custom ReadSeeker
	} else {
		http.ServeFile(w, r, fullPath)
	}

	// Cache the response in Redis
	response := map[string]string{"filePath": filePath, "status": "served"}
	responseJSON, _ := json.Marshal(response)
	storeFileHash(filePath, string(responseJSON), conf)
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

// shouldScanExtension checks if the file extension should be scanned.
func shouldScanExtension(ext string, scanExtensions []string) bool {
	for _, scanExt := range scanExtensions {
		if strings.EqualFold(ext, scanExt) {
			return true
		}
	}
	return false
}

// Add a timeout to scanFileWithClamAV
func scanFileWithClamAV(filePath string) (*clamd.ScanResult, error) {
	scanChannel, err := ClamAVClient.ScanFile(filePath)
	if err != nil {
		return nil, err
	}

	select {
	case scanResult, ok := <-scanChannel:
		if !ok {
			return nil, fmt.Errorf("ClamAV scan channel closed unexpectedly")
		}
		return scanResult, nil
	case <-time.After(60 * time.Second):
		return nil, fmt.Errorf("ClamAV scan timed out for file: %s", filePath)
	}
}

// calculateFileHash computes the SHA-256 hash of the file.

// checkFileExists checks if a file with the given hash exists.
func checkFileExists(hash string, conf *config.Config) (bool, string) {
	if conf.Redis.RedisEnabled && RedisClient != nil {
		// Check in Redis
		existingFilePath, err := RedisClient.Get(redisCtx, hash).Result()
		if err == redis.Nil {
			return false, ""
		} else if err != nil {
			logrus.Errorf("Redis error: %v", err)
			return false, ""
		}
		return true, existingFilePath
	} else {
		// Check in in-memory cache
		if data, found := InMemoryCache.Get(hash); found {
			return true, data.(string)
		}
		return false, ""
	}
}

// storeFileHash stores the file hash and path.
func storeFileHash(hash string, filePath string, conf *config.Config) {
	if conf.Redis.RedisEnabled && RedisClient != nil {
		// Store in Redis
		err := RedisClient.Set(redisCtx, hash, filePath, 0).Err()
		if err != nil {
			logrus.Errorf("Failed to store hash in Redis: %v", err)
		}
	} else {
		// Store in in-memory cache
		InMemoryCache.Set(hash, filePath, cache.DefaultExpiration)
	}
}

// createISO creates an ISO file with the specified charset.
func createISO(filePath, charset string) error {
	cmd := exec.Command("mkisofs", "-o", filePath, "-input-charset", charset, filepath.Dir(filePath))
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("mkisofs error: %v, output: %s", err, string(output))
	}
	return nil
}

// ...other handler functions...