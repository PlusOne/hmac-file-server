package handlers

import (
	"context"
	"crypto/sha256"

	// "encoding/json"
	"fmt"
	"io"
	"net/http"
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

var (
    RedisClient      *redis.Client
    InMemoryCache    *cache.Cache
    ClamAVClient     *clamd.Clamd
    HMACWorkerPool   chan struct{}
    ClamAVWorkerPool chan struct{}
    mu               sync.Mutex
)

// HTTP Handlers

func handleUpload(w http.ResponseWriter, r *http.Request, conf *config.Config) {
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

    if !validateHMAC() {
        http.Error(w, "Invalid HMAC", http.StatusForbidden)
        return
    }

    if !isExtensionAllowed(storagePath) {
        http.Error(w, "Invalid file extension", http.StatusBadRequest)
        return
    }

    minFreeBytes, err := parseSize(conf.Server.MinFreeBytes)
    if err != nil {
        logrus.Fatalf("Invalid MinFreeBytes: %v", err)
    }
    if err := checkStorageSpace(conf.Server.StoragePath, minFreeBytes); err != nil {
        http.Error(w, "Not enough free space", http.StatusInsufficientStorage)
        return
    }

    sha256Hash, tempFilePath, err := saveUploadedFile(r, conf)
    if err != nil {
        http.Error(w, "Internal server error", http.StatusInternalServerError)
        return
    }
    logrus.Infof("File uploaded and saved to temp path: %s", tempFilePath)

    if conf.Server.DeduplicationEnabled {
        if handleDeduplication(storagePath, sha256Hash, conf) {
            http.Error(w, "File already exists", http.StatusConflict)
            return
        }
    }

    if err := os.Rename(tempFilePath, storagePath); err != nil {
        http.Error(w, "Internal server error", http.StatusInternalServerError)
        return
    }

    storeFileHash(sha256Hash, storagePath, conf)

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

    if conf.ISO.Enabled {
        go func() {
            if err := createISO(storagePath, conf.ISO.Charset); err != nil {
                logrus.Errorf("Failed to create ISO: %v", err)
            }
        }()
    }

    w.WriteHeader(http.StatusOK)
    w.Write([]byte("File uploaded successfully"))
}

func handleDeduplication(storagePath, sha256Hash string, conf *config.Config) bool {
    exists, existingPath := checkFileExists(sha256Hash, conf)
    if exists {
        os.Remove(storagePath)
        os.Link(existingPath, storagePath)
        logrus.Infof("Deduplication: linked to existing file %s", existingPath)
        return true
    }
    return false
}

// Helper Functions

func acquireWorker(workerPool chan struct{}) {
    workerPool <- struct{}{}
}

func releaseWorker(workerPool chan struct{}) {
    <-workerPool
}

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

func shouldScanExtension(ext string, scanExtensions []string) bool {
    for _, scanExt := range scanExtensions {
        if strings.EqualFold(ext, scanExt) {
            return true
        }
    }
    return false
}

func scanFile(filePath string) bool {
    scanResult, err := scanFileWithClamAV(filePath)
    if err != nil {
        logrus.Errorf("ClamAV scan error: %v", err)
        return false
    }
    return scanResult.Status == clamd.RES_OK
}

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

func validateHMAC() bool {
    // Placeholder implementation of HMAC validation
    return true
}

func saveUploadedFile(r *http.Request, conf *config.Config) (string, string, error) {
    file, _, err := r.FormFile("file")
    if err != nil {
        return "", "", err
    }
    defer file.Close()

	tempFile, err := os.CreateTemp(conf.Server.TempPath, "upload-*.tmp")
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

func checkFileExists(hash string, conf *config.Config) (bool, string) {
    mu.Lock()
    defer mu.Unlock()
    redisCtx := context.Background()
    if conf.Redis.RedisEnabled && RedisClient != nil {
        existingFilePath, err := RedisClient.Get(redisCtx, hash).Result()
        if err == redis.Nil {
            return false, ""
        } else if err != nil {
            logrus.Errorf("Redis error: %v", err)
            return false, ""
        }
        return true, existingFilePath
    } else {
        if data, found := InMemoryCache.Get(hash); found {
            return true, data.(string)
        }
        return false, ""
    }
}

func storeFileHash(hash string, filePath string, conf *config.Config) {
    mu.Lock()
    defer mu.Unlock()
    if conf.Redis.RedisEnabled && RedisClient != nil {
        redisCtx := context.Background()
        err := RedisClient.Set(redisCtx, hash, filePath, 0).Err()
        if err != nil {
            logrus.Errorf("Failed to store hash in Redis: %v", err)
        }
    } else {
        InMemoryCache.Set(hash, filePath, cache.DefaultExpiration)
    }
}

func createISO(filePath, charset string) error {
    cmd := exec.Command("mkisofs", "-o", filePath, "-input-charset", charset, filepath.Dir(filePath))
    output, err := cmd.CombinedOutput()
    if err != nil {
        return fmt.Errorf("mkisofs error: %v, output: %s", err, string(output))
    }
    return nil
}

func getStoragePath(filename string, conf *config.Config) string {
    return filepath.Join(conf.Server.StoragePath, filename)
}


func SetupRouter(conf *config.Config) *mux.Router {
    router := mux.NewRouter()

    router.HandleFunc("/upload", func(w http.ResponseWriter, r *http.Request) {
        handleUpload(w, r, conf)
    }).Methods("POST")

    router.Use(LoggingMiddleware, RecoveryMiddleware, CORSMiddleware)

    return router
}

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

func CORSMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        w.Header().Set("Access-Control-Allow-Origin", "*")
        w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
        w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")

        if r.Method == http.MethodOptions {
            return
        }

        next.ServeHTTP(w, r)
    })
}
