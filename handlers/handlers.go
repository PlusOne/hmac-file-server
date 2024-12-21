package handlers

import (
	"bytes"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"log"
	"mime"

	// "encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"sync"

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"github.com/gorilla/mux"
	"github.com/patrickmn/go-cache"
	"github.com/renz/hmac-file-server/config" // Corrected import path
	"github.com/renz/hmac-file-server/utils"  // Corrected import path
	"github.com/sirupsen/logrus"
)


// bufferPool is a pool of byte buffers to reduce memory allocations
var bufferPool = sync.Pool{
    New: func() interface{} {
        return new(bytes.Buffer)
    },
}

var mu sync.Mutex

// Update the Config struct to include necessary dependencies
type HandlerDependencies struct {
	RedisClient      *redis.Client
	InMemoryCache    *cache.Cache
	ClamAVClient     *clamd.Clamd
	HMACWorkerPool   chan struct{}
	ClamAVWorkerPool chan struct{}
}

func SetupHandlerDependencies(redisClient *redis.Client, inMemoryCache *cache.Cache, clamAVClient *clamd.Clamd, hmacWorkerPool chan struct{}, clamAVWorkerPool chan struct{}) *HandlerDependencies {
    return &HandlerDependencies{
        RedisClient:     redisClient,
        InMemoryCache:   inMemoryCache,
        ClamAVClient:    clamAVClient,
        HMACWorkerPool:  hmacWorkerPool,
        ClamAVWorkerPool: clamAVWorkerPool,
    }
}

type Handler struct {
	cfg *config.Config
	// ...other fields...
}

func NewHandler(cfg *config.Config) *Handler {
	return &Handler{
		cfg: cfg,
		// ...initialize other fields...
	}
}

// HTTP Handlers

func (h *Handler) handleUpload(w http.ResponseWriter, r *http.Request, deps *HandlerDependencies) {
	logrus.Debugf("handleUpload called with method=%s, path=%s, contentLength=%d", r.Method, r.URL.Path, r.ContentLength)
	logrus.Info("Handling file upload request")
	acquireWorker(deps.HMACWorkerPool)
	defer releaseWorker(deps.HMACWorkerPool)

	queryParams := r.URL.Query()
	logrus.Debugf("Query params: %v", queryParams)
	absFilename := queryParams.Get("file")
	if absFilename == "" {
		logrus.Warn("File parameter is missing in upload request")
		http.Error(w, "File parameter is missing", http.StatusBadRequest)
		return
	}

	storagePath := getStoragePath(absFilename, h.cfg)
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
		logrus.Warn("No HMAC attached to upload URL")
		http.Error(w, "No HMAC attached to upload URL. Expecting 'v', 'v2', or 'token' parameter as MAC", http.StatusForbidden)
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
	logrus.Debugf("Determined content type: %s", contentType)

	// Validate HMAC
	isValid := validateHMAC(protocolVersion, storagePath, r.ContentLength, contentType, providedMACHex, h.cfg.Security.Secret)
	if !isValid {
		logrus.Warn("HMAC validation failed for upload request")
		http.Error(w, "Invalid HMAC signature.", http.StatusForbidden)
		return
	}
	logrus.Info("HMAC validation succeeded for upload request")

	if (!isExtensionAllowed(storagePath)) {
		logrus.Warnf("Invalid file extension for file: %s", storagePath)
		http.Error(w, "Invalid file extension", http.StatusBadRequest)
		return
	}
	logrus.Infof("File extension is allowed for file: %s", storagePath)

	minFreeBytes, err := utils.ParseSize(h.cfg.Server.MinFreeBytes)
	if err != nil {
		logrus.Fatalf("Invalid MinFreeBytes: %v", err)
	}
	if err := utils.CheckStorageSpace(h.cfg.Server.StoragePath, minFreeBytes); err != nil {
		logrus.Errorf("Insufficient storage space: %v", err)
		http.Error(w, "Insufficient storage space", http.StatusInsufficientStorage)
		return
	}

	sha256Hash, tempFilePath, err := saveUploadedFile(r, h.cfg)
	if err != nil {
		logrus.Errorf("Error saving uploaded file: %v", err)
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}
	logrus.Infof("File uploaded and saved to temp path: %s", tempFilePath)

	if h.cfg.Server.DeduplicationEnabled {
		if handleDeduplication(storagePath, sha256Hash, h.cfg, deps) {
			logrus.Info("Deduplication successful")
			// Optionally, respond or take additional actions
		} else {
			logrus.Warn("Deduplication failed or file is unique")
		}
	}

	if err := os.Rename(tempFilePath, storagePath); err != nil {
		logrus.Errorf("Error moving file from %s to %s: %v", tempFilePath, storagePath, err)
		http.Error(w, "Failed to store the uploaded file", http.StatusInternalServerError)
		return
	}

	storeFileHash(sha256Hash, storagePath, h.cfg, deps)

	if h.cfg.ClamAV.ClamAVEnabled {
		ext := filepath.Ext(storagePath)
		if shouldScanExtension(ext, h.cfg.ClamAV.ScanFileExtensions) {
			if !scanFile(storagePath) {
				logrus.Warnf("File %s failed ClamAV scan", storagePath)
				http.Error(w, "File failed virus scan", http.StatusForbidden)
				return
			}
		}
	}

	if h.cfg.ISO.Enabled {
		go func() {
			if err := createISO(storagePath, h.cfg.ISO.Charset); err != nil {
				logrus.Errorf("Error creating ISO for %s: %v", storagePath, err)
			} else {
				logrus.Infof("ISO created for %s", storagePath)
			}
		}()
	}

	w.WriteHeader(http.StatusOK)
	w.Write([]byte("File uploaded successfully"))
	logrus.Info("File upload handled successfully")
}

func storeFileHash(sha256Hash, storagePath string, config *config.Config, deps *HandlerDependencies) {
	panic("unimplemented")
}

func isExtensionAllowed(storagePath string) bool {
	panic("unimplemented")
}

func createISO(storagePath, s string) any {
	panic("unimplemented")
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

	logrus.Debugf("Provided MAC (hex): %s", providedMACHex)
	logrus.Debugf("Calculated MAC (hex): %s", hex.EncodeToString(calculatedMAC))

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

func handleDeduplication(storagePath, sha256Hash string, conf *config.Config, deps *HandlerDependencies) bool {
    exists, existingPath := checkFileExists(sha256Hash, conf, deps)
	if exists.(bool) {
        os.Remove(storagePath)
		os.Link(existingPath.(string), storagePath)
        logrus.Infof("Deduplication: linked to existing file %s", existingPath)
        return true
    }
    return false
}

func checkFileExists(sha256Hash string, conf *config.Config, deps *HandlerDependencies) (any, any) {
	panic("unimplemented")
}

// Helper Functions

func acquireWorker(workerPool chan struct{}) {
    workerPool <- struct{}{}
}

func releaseWorker(workerPool chan struct{}) {
    <-workerPool
}


func saveUploadedFile(r *http.Request, conf *config.Config) (string, string, error) {
    file, _, err := r.FormFile("file")
    if (err != nil) {
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

// nolint:unused

// Removed unused function storeFileHash


func getStoragePath(filename string, conf *config.Config) string {
    return filepath.Join(conf.Server.StoragePath, filename)
}

func createFile(tempFilename string, r *http.Request) error {
    file, err := os.Create(tempFilename)
    if err != nil {
        return err
    }
    defer file.Close()

    // Example: Copying the request body to the file
    _, err = io.Copy(file, r.Body)
    if err != nil {
        return err
    }

    return nil
}

func SetupRouter(conf *config.Config, deps *HandlerDependencies) *mux.Router {
	logrus.Debug("Initializing router and registering routes")
    router := mux.NewRouter()

    router.HandleFunc("/upload", func(w http.ResponseWriter, r *http.Request) {
		logrus.Debugf("Route matched: /upload with method %s", r.Method)
        handleUpload(w, r, conf, deps)
    }).Methods("POST")

	router.HandleFunc("/uploads/{filename}", func(w http.ResponseWriter, r *http.Request) {
		UploadFileHandler(w, r, conf)
	}).Methods("PUT")

	router.HandleFunc("/download/{filename}", func(w http.ResponseWriter, r *http.Request) {
		vars := mux.Vars(r)
		filename := vars["filename"]
		storagePath := getStoragePath(filename, conf)
		handleDownload(w, r, filename, storagePath)
	}).Methods("GET")

    router.Use(LoggingMiddleware, RecoveryMiddleware, CORSMiddleware)

	logrus.Debug("Router setup completed")
    return router
}

func handleUpload(w http.ResponseWriter, r *http.Request, conf *config.Config, deps *HandlerDependencies) {
	panic("unimplemented")
}



func LoggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logrus.WithFields(logrus.Fields{
			"method":  r.Method,
			"url":     r.URL.String(),
			"remote":  utils.GetClientIP(r),
			"headers": r.Header,
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

// CORSMiddleware adds CORS headers to each response
func CORSMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logrus.Debugf("Handling CORS for request: %s %s", r.Method, r.URL.Path)
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusNoContent)
			return
		}
		next.ServeHTTP(w, r)
	})
}

func YourHandler(w http.ResponseWriter, r *http.Request) {
    // Get a buffer from the pool
    buf := bufferPool.Get().(*bytes.Buffer)
    buf.Reset() // Clear the buffer before use

    // Use the buffer for your operations
    // Example: writing some data
    buf.WriteString("Hello, World!")

    // Write buffer contents to the ResponseWriter
    w.Write(buf.Bytes())

    // Put the buffer back into the pool
    bufferPool.Put(buf)
}

func UploadHandler(w http.ResponseWriter, r *http.Request) {
    tempFilename := "temp_upload_file.txt"

    // Call createFile to handle the file creation
    err := createFile(tempFilename, r)
    if err != nil {
        http.Error(w, "Failed to create file", http.StatusInternalServerError)
        log.Printf("Error creating file: %v", err)
        return
    }

    // Respond to the client
    w.WriteHeader(http.StatusOK)
    w.Write([]byte("File uploaded successfully"))
}

func UploadFileHandler(w http.ResponseWriter, r *http.Request, conf *config.Config) {
	if r.Method != http.MethodPut {
		http.Error(w, "Method Not Allowed", http.StatusMethodNotAllowed)
		return
	}

	authHeader := r.Header.Get("Authorization")
	if authHeader == "" {
		http.Error(w, "Missing Authorization header", http.StatusUnauthorized)
		return
	}

	err := r.ParseMultipartForm(10 << 20) // 10MB
	if err != nil {
		logrus.Errorf("Error parsing form: %v", err)
		http.Error(w, "Invalid form data", http.StatusBadRequest)
		return
	}

	file, handler, err := r.FormFile("file")
	if err != nil {
		logrus.Errorf("File not provided: %v", err)
		http.Error(w, "File not provided", http.StatusBadRequest)
		return
	}
	defer file.Close()

	filename := mux.Vars(r)["filename"]

	// Use configured upload path instead of hardcoding
	storagePath := filepath.Join(conf.Server.StoragePath, filename)
	logrus.Debugf("Using configured storage path: %s", storagePath)

	out, err := os.Create(storagePath)
	if err != nil {
		logrus.Errorf("Error saving file: %v", err)
		http.Error(w, "File save error", http.StatusInternalServerError)
		return
	}
	defer out.Close()

	_, err = io.Copy(out, file)
	if err != nil {
		logrus.Errorf("Error writing file: %v", err)
		http.Error(w, "File write error", http.StatusInternalServerError)
		return
	}

	logrus.Infof("File '%s' uploaded successfully as '%s'", handler.Filename, filename)
	w.WriteHeader(http.StatusCreated)
	w.Write([]byte("File uploaded successfully"))
}

// Removed unused function handleRequest

func handleDownload(w http.ResponseWriter, r *http.Request, absFilename any, fileStorePath string) {
	panic("unimplemented")
}

