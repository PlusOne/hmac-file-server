package handlers

import (
	"context"
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"github.com/gorilla/mux"
	"github.com/patrickmn/go-cache"
	"github.com/renz/hmac-file-server/config"
	"github.com/renz/hmac-file-server/utils"
	"github.com/sirupsen/logrus"
)

func handleUpload(w http.ResponseWriter, r *http.Request, conf *config.Config) {
	// Use helper for worker pool handling
	acquireWorker(HMACWorkerPool)
	defer releaseWorker(HMACWorkerPool)

	queryParams := r.URL.Query()
	absFilename := queryParams.Get("file")
	if absFilename == "" {
		http.Error(w, "File parameter is missing", http.StatusBadRequest)
		return
	}

	storagePath := getStoragePath(absFilename, conf)
	logrus.Infof("Using storage path: %s", storagePath)

	// Extract HMAC Validation
	protocolVersion, err := getHMACProtocol(queryParams)
	if err != nil {
		http.Error(w, err.Error(), http.StatusForbidden)
		return
	}
	if !validateHMAC(r, storagePath, protocolVersion, conf) {
		http.Error(w, "Invalid HMAC", http.StatusForbidden)
		return
	}

	if !isExtensionAllowed(storagePath) {
		http.Error(w, "Invalid file extension", http.StatusBadRequest)
		return
	}

	// Handle storage space check
	minFreeBytes, err := parseSize(conf.Server.MinFreeBytes)
	if err != nil {
		logrus.Fatalf("Invalid MinFreeBytes: %v", err)
	}
	if err := checkStorageSpace(conf.Server.StoragePath, minFreeBytes); err != nil {
		http.Error(w, "Not enough free space", http.StatusInsufficientStorage)
		return
	}

	// Save uploaded file and calculate hash
	sha256Hash, tempFilePath, err := saveUploadedFile(r, storagePath, conf)
	if err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}
	logrus.Infof("File uploaded and saved to temp path: %s", tempFilePath)

	// Handle Deduplication
	if conf.Server.DeduplicationEnabled {
		if handleDeduplication(storagePath, sha256Hash, conf) {
			return
		}
	}

	// Move temp file to final location
	if err := os.Rename(tempFilePath, storagePath); err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}

	// Handle ClamAV scanning
	if conf.ClamAV.ClamAVEnabled {
		ext := filepath.Ext(storagePath)
		if shouldScanExtension(ext, conf.ClamAV.ScanFileExtensions) {
			if !scanFile(storagePath) {
				http.Error(w, "Uploaded file is infected", http.StatusBadRequest)
				os.Remove(storagePath)
				return
			}
			logrus.Infof("ClamAV scan passed for file: %s", storagePath)
		}
	}

	// Handle ISO creation
	if conf.ISO.Enabled {
		go func() {
			if err := createISO(storagePath, conf.ISO.Charset); err != nil {
				logrus.Errorf("Failed to create ISO: %v", err)
			}
		}()
	}

	// Final response for successful upload
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("File uploaded successfully"))
}

func handleDeduplication(storagePath, sha256Hash string, conf *config.Config) bool {
	// Check if the file already exists
	exists, existingPath := checkFileExists(sha256Hash, conf)
	if exists {
		// File already exists, remove the temp file
		os.Remove(storagePath)
		// Optionally, link to existing file
		os.Link(existingPath, storagePath)
		logrus.Infof("Deduplication: linked to existing file %s", existingPath)
		return true
	}
	return false
}

func acquireWorker(workerPool chan struct{}) {
	workerPool <- struct{}{}
}

func releaseWorker(workerPool chan struct{}) {
	<-workerPool
}

var (
	RedisClient      *redis.Client // Redis client
	InMemoryCache    *cache.Cache  // In-memory cache for fallback
	ClamAVClient     *clamd.Clamd  // ClamAV client variable
	HMACWorkerPool   chan struct{} // Worker pool for HMAC processing
	ClamAVWorkerPool chan struct{} // Worker pool for ClamAV processing
	mu               sync.Mutex    // Added mutex for synchronization
)

func SetupRouter(conf *config.Config) *mux.Router {
	router := mux.NewRouter()

	// Define routes
	router.HandleFunc("/upload", func(w http.ResponseWriter, r *http.Request) {
		handleFileUpload(w, r, conf)
	}).Methods("POST")

	router.HandleFunc("/download/{filename}", func(w http.ResponseWriter, r *http.Request) {
		handleFileDownload(w, r, conf)
	}).Methods("GET")

	router.HandleFunc("/delete/{filename}", func(w http.ResponseWriter, r *http.Request) {
		handleFileDeletion(w, r, conf)
	}).Methods("DELETE")

	// Add more routes as needed
	// ...existing code...

	return router
}

// handleFileUpload handles the file upload process
func handleFileUpload(w http.ResponseWriter, r *http.Request, conf *config.Config) {
	// ...existing code...

	// Ensure the file is within the allowed size limit
	maxFileSize, err := utils.ParseSize(conf.Uploads.ChunkSize)
	if err != nil {
		logrus.WithError(err).Error("Invalid chunk size")
		http.Error(w, "Invalid chunk size", http.StatusBadRequest)
		return
	}

	// Ensure the uploaded file does not exceed the maximum file size
	r.Body = http.MaxBytesReader(w, r.Body, maxFileSize)

	// ...existing code...
}

// handleFileDownload handles the file download process
func handleFileDownload(w http.ResponseWriter, r *http.Request) {
	// ...existing code...

	// Ensure the file exists and is accessible
	filePath := mux.Vars(r)["filename"]
	if !utils.FileExists(filePath) {
		http.Error(w, "File not found", http.StatusNotFound)
		return
	}

	// ...existing code...
}

// handleFileDeletion handles the file deletion process
func handleFileDeletion(w http.ResponseWriter, r *http.Request, conf *config.Config) {
	// ...existing code...

	// Ensure the file exists and is accessible
	filePath := mux.Vars(r)["filename"]
	if !utils.FileExists(filePath) {
		http.Error(w, "File not found", http.StatusNotFound)
		return
	}

	// ...existing code...
}

// handleRequest handles incoming HTTP requests.
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
	sizeStr = strings.TrimSpace(sizeStr)
	multiplier := int64(1)
	switch {
	case strings.HasSuffix(sizeStr, "KB"):
		multiplier = 1 << 10
		sizeStr = strings.TrimSuffix(sizeStr, "KB")
	case strings.HasSuffix(sizeStr, "MB"):
		multiplier = 1 << 20
		sizeStr = strings.TrimSuffix(sizeStr, "MB")
	case strings.HasSuffix(sizeStr, "GB"):
		multiplier = 1 << 30
		sizeStr = strings.TrimSuffix(sizeStr, "GB")
	case strings.HasSuffix(sizeStr, "TB"):
		multiplier = 1 << 40
		sizeStr = strings.TrimSuffix(sizeStr, "TB")
	}
	value, err := strconv.ParseFloat(sizeStr, 64)
	if err != nil {
		return 0, err
	}
	return int64(value * float64(multiplier)), nil
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

// scanFile scans the file using ClamAV.
func scanFile(filePath string) bool {
	scanResult, err := scanFileWithClamAV(filePath)
	if err != nil {
		logrus.Errorf("ClamAV scan error: %v", err)
		return false
	}
	return scanResult.Status == clamd.RES_OK
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

// validateHMAC validates the HMAC of the request.
func validateHMAC(r *http.Request, storagePath, protocolVersion string, conf *config.Config) bool {
	// Implement HMAC validation logic here
	// This is a placeholder implementation
	return true
}

// saveUploadedFile saves the uploaded file to a temporary location and returns its SHA-256 hash.
func saveUploadedFile(r *http.Request, storagePath string, conf *config.Config) (string, string, error) {
	file, _, err := r.FormFile("file")
	if err != nil {
		return "", "", err
	}
	defer file.Close()

	tempFile, err := ioutil.TempFile(conf.Server.TempPath, "upload-*.tmp")
	if err != nil {
		return "", "", err
	}
	defer tempFile.Close()

	hash := sha256.New()
	if _, err := io.Copy(io.MultiWriter(tempFile, hash), file); err != nil {
		return "", "", err
	}

	return fmt.Sprintf("%x", hash.Sum(nil)), tempFile.Name(), nil
}

// calculateFileHash computes the SHA-256 hash of the file.

// checkFileExists checks if a file with the given hash exists.
func checkFileExists(hash string, conf *config.Config) (bool, string) {
	mu.Lock()
	defer mu.Unlock()
	redisCtx := context.Background()
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
	mu.Lock()
	defer mu.Unlock()
	if conf.Redis.RedisEnabled && RedisClient != nil {
		// Store in Redis
		redisCtx := context.Background()
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

// getStoragePath constructs the storage path for the given filename.
func getStoragePath(filename string, conf *config.Config) string {
	return filepath.Join(conf.Server.StoragePath, filename)
}

// getHMACProtocol extracts the HMAC protocol version from the query parameters.
func getHMACProtocol(queryParams url.Values) (string, error) {
	protocolVersion := queryParams.Get("protocol")
	if protocolVersion == "" {
		return "", fmt.Errorf("protocol parameter is missing")
	}
	return protocolVersion, nil
}

// ...other handler functions...

// Make sure to implement your HTTP handlers here if this file is intended for that purpose.
