package handlers

import (
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"net/url"

	"github.com/PlusOne/hmac-file-server/metrics"
	"github.com/sirupsen/logrus"
)

type UploadConfig struct {
	Server struct {
		StoragePath          string
		DeduplicationEnabled bool
		MinFreeBytes         string // Ensure this field is present
		Redis struct {
			RedisEnabled            bool
			RedisAddr               string
			RedisDBIndex            int
			RedisHealthCheckInterval string
			Uploads struct {
				ChunkedUploadsEnabled bool
				AllowedExtensions     []string
				ChunkSize             string
			} `json:"uploads"`
		}
	}
	File struct {
		FileRevision int
	}
}

var conf *UploadConfig

func UploadHandler(w http.ResponseWriter, r *http.Request) {
	if !conf.Server.Redis.Uploads.ChunkedUploadsEnabled {
		http.Error(w, "Chunked uploads are not enabled", http.StatusForbidden)
		return
	}

	if conf.Server.DeduplicationEnabled {
		logrus.Info("Deduplication is enabled.")
		// Add your deduplication logic here
	} else {
		logrus.Info("Deduplication is not enabled.")
	}

	if strings.ToUpper(conf.Server.MinFreeBytes) == "100GB" {
		logrus.Info("MinFreeBytes is set to 100GB.")
		// Add your logic here
	} else {
		logrus.Infof("MinFreeBytes is set to %s.", conf.Server.MinFreeBytes)
	}

	if conf.File.FileRevision == 1 {
		logrus.Info("FileRevision is set to 1.")
		// Add your logic here
	} else {
		logrus.Infof("FileRevision is set to %d.", conf.File.FileRevision)
	}

	if !conf.Server.Redis.RedisEnabled {
		logrus.Info("Redis is disabled.")
	} else {
		logrus.Infof("Redis is enabled. Addr: %s, DBIndex: %d, HealthCheckInterval: %s",
			conf.Server.Redis.RedisAddr, conf.Server.Redis.RedisDBIndex, conf.Server.Redis.RedisHealthCheckInterval)
	}

	file, header, err := r.FormFile("file")
	if err != nil {
		http.Error(w, "Failed to get file from request", http.StatusBadRequest)
		return
	}
	defer file.Close()

	// Check if the file extension is allowed
	ext := strings.ToLower(filepath.Ext(header.Filename))
	allowed := false
	for _, allowedExt := range conf.Server.Redis.Uploads.AllowedExtensions {
		if ext == allowedExt {
			allowed = true
			break
		}
	}
	if !allowed {
		http.Error(w, "File extension not allowed", http.StatusForbidden)
		return
	}

	chunkSize, err := parseChunkSize(conf.Server.Redis.Uploads.ChunkSize)
	if err != nil {
		http.Error(w, "Invalid chunk size", http.StatusInternalServerError)
		return
	}

	filePath := filepath.Join(conf.Server.StoragePath, header.Filename)
	outFile, err := os.Create(filePath)
	if err != nil {
		http.Error(w, "Failed to create file", http.StatusInternalServerError)
		return
	}
	defer outFile.Close()

	buf := make([]byte, chunkSize)
	for {
		n, err := file.Read(buf)
		if err != nil && err != io.EOF {
			http.Error(w, "Failed to read file", http.StatusInternalServerError)
			return
		}
		if n == 0 {
			break
		}

		if _, err := outFile.Write(buf[:n]); err != nil {
			http.Error(w, "Failed to write file", http.StatusInternalServerError)
			return
		}
	}

	logrus.Infof("File %s uploaded successfully", header.Filename)
	w.WriteHeader(http.StatusOK)

	// Increment the centralized upload errors metric
	metrics.UploadErrorsTotal.Inc()
}

func parseChunkSize(sizeStr string) (int64, error) {
	sizeStr = sizeStr[:len(sizeStr)-2] // Remove the "MB" suffix
	size, err := strconv.ParseInt(sizeStr, 10, 64)
	if err != nil {
		return 0, err
	}
	return size * 1024 * 1024, nil // Convert MB to bytes
}

// Wrapper for handleUpload to match handler signature
func handleUploadWrapper(w http.ResponseWriter, r *http.Request) {
    absFilename := r.URL.Query().Get("filename") // Retrieve absolute filename as needed
    fileStorePath := filepath.Join(conf.Server.StoragePath, absFilename) // Determine file store path
    queryParams := r.URL.Query()
    handleUpload(w, r, fileStorePath, queryParams)
}

// Handle file uploads with extension restrictions, HMAC validation, and deduplication
func handleUpload(w http.ResponseWriter, r *http.Request, fileStorePath string, a url.Values) {
    // Existing upload logic...
}
