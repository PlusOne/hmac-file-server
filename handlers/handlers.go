package handlers

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"mime"

	// "encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"sync"

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"github.com/gorilla/mux"
	"github.com/patrickmn/go-cache"
	"github.com/renz/hmac-file-server/config"   // Corrected import path
	"github.com/renz/hmac-file-server/utils"    // Corrected import path
	"github.com/sirupsen/logrus"
)

var (
    mu sync.Mutex
)

// Update the Config struct to include necessary dependencies
type HandlerDependencies struct {
	RedisClient      *redis.Client
	InMemoryCache    *cache.Cache
	ClamAVClient     *clamd.Clamd
	HMACWorkerPool   chan struct{}
	ClamAVWorkerPool chan struct{}
}

// HTTP Handlers

func handleUpload(w http.ResponseWriter, r *http.Request, conf *config.Config, deps *HandlerDependencies) {
	acquireWorker(deps.HMACWorkerPool)
	defer releaseWorker(deps.HMACWorkerPool)

	queryParams := r.URL.Query()
	absFilename := queryParams.Get("file")
	if absFilename == "" {
		http.Error(w, "File parameter is missing", http.StatusBadRequest)
		return
	}

	storagePath := getStoragePath(absFilename, conf)
	logrus.Infof("Using storage path: %s", storagePath)

	// Determine protocol version
	var protocolVersion string
	if queryParams.Get("v2") != "" {
		protocolVersion = "v2"
	} else if queryParams.Get("token") != "" {
		protocolVersion = "token"
	} else if queryParams.Get("v") != "" {
		protocolVersion = "v"
	} else {
		logrus.Warn("No HMAC attached to URL.")
		http.Error(w, "No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC", http.StatusForbidden)
		return
	}

	// Extract provided HMAC
	providedMACHex := queryParams.Get(protocolVersion)
	if providedMACHex == "" {
		logrus.Warnf("Missing HMAC parameter for protocol version: %s", protocolVersion)
		http.Error(w, "Missing HMAC parameter.", http.StatusForbidden)
		return
	}

	// Determine content type
	contentType := mime.TypeByExtension(filepath.Ext(storagePath))
	if contentType == "" {
		contentType = "application/octet-stream"
	}

	// Validate HMAC
	isValid := validateHMAC(protocolVersion, storagePath, r.ContentLength, contentType, providedMACHex, conf.Security.Secret)
	if !isValid {
		http.Error(w, "Invalid HMAC signature.", http.StatusForbidden)
		return
	}

	if !isExtensionAllowed(storagePath) {
		http.Error(w, "Invalid file extension", http.StatusBadRequest)
		return
	}

	minFreeBytes, err := utils.ParseSize(conf.Server.MinFreeBytes)
	if err != nil {
		logrus.Fatalf("Invalid MinFreeBytes: %v", err)
	}
	if err := utils.CheckStorageSpace(conf.Server.StoragePath, minFreeBytes); err != nil {
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

	storeFileHash(sha256Hash, storagePath, conf, deps)

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

// validateHMAC validates the HMAC signature based on the protocol version.
func validateHMAC(protocolVersion, fileStorePath string, contentLength int64, contentType, providedMACHex string, secret string) bool {
	mac := hmac.New(sha256.New, []byte(secret))

	// Calculate HMAC according to protocol
	if protocolVersion == "v" {
		mac.Write([]byte(fileStorePath + "\x20" + strconv.FormatInt(contentLength, 10)))
	} else if protocolVersion == "v2" || protocolVersion == "token" {
		if contentType == "" {
			contentType = "application/octet-stream"
		}
		mac.Write([]byte(fileStorePath + "\x00" + strconv.FormatInt(contentLength, 10) + "\x00" + contentType))
	} else {
		logrus.Warnf("Unsupported protocol version: %s", protocolVersion)
		return false
	}

	calculatedMAC := mac.Sum(nil)

	providedMAC, err := hex.DecodeString(providedMACHex)
	if err != nil {
		logrus.Warnf("Error decoding provided MAC: %v", err)
		return false
	}

	if !hmac.Equal(calculatedMAC, providedMAC) {
		logrus.Warn("Invalid HMAC signature.")
		return false
	}

	return true
}

func scanFile(storagePath string) bool {
	panic("unimplemented")
}

func shouldScanExtension(ext string, s []string) bool {
	panic("unimplemented")
}

func handleDeduplication(storagePath, sha256Hash string, conf *config.Config) bool {
	var deps *HandlerDependencies
	exists, existingPath := checkFileExists(sha256Hash, conf, deps)
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

func checkFileExists(hash string, conf *config.Config, deps *HandlerDependencies) (bool, string) {
    mu.Lock()
    defer mu.Unlock()
    redisCtx := context.Background()
	RedisClient := deps.RedisClient
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
		if data, found := deps.InMemoryCache.Get(hash); found {
            return true, data.(string)
        }
        return false, ""
    }
}

func storeFileHash(hash string, filePath string, conf *config.Config, deps *HandlerDependencies) {
    mu.Lock()
    defer mu.Unlock()
	if conf.Redis.RedisEnabled && deps.RedisClient != nil {
        redisCtx := context.Background()
		err := deps.RedisClient.Set(redisCtx, hash, filePath, 0).Err()
        if err != nil {
            logrus.Errorf("Failed to store hash in Redis: %v", err)
        }
    } else {
		deps.InMemoryCache.Set(hash, filePath, cache.DefaultExpiration)
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


func SetupRouter(conf *config.Config, deps *HandlerDependencies) *mux.Router {
    router := mux.NewRouter()

    router.HandleFunc("/upload", func(w http.ResponseWriter, r *http.Request) {
        handleUpload(w, r, conf, deps)
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
