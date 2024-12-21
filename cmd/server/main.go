package main

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"mime"
	"net"
	"net/http"
	"net/url"
	"os"
	"strconv"
	"strings"
	"time"

	// "strconv" // Removed unused import
	"path/filepath"

	// "strings" // Removed unused import
	// "net/url" // Removed unused import

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"github.com/patrickmn/go-cache" // Added import for in-memory cache
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/renz/hmac-file-server/config"   // Corrected import path
	"github.com/renz/hmac-file-server/handlers" // Corrected import path
	"github.com/renz/hmac-file-server/utils"    // Corrected import path
	"github.com/sirupsen/logrus"
)

var (
	versionString = "v1.0.0" // Define the version string
)

func main() {
	// Add debugging statements at the start
	logrus.Debug("Starting HMAC File Server...")
	logrus.Debugf("Process ID (PID): %d", os.Getpid())

	// Define configuration file paths in order of priority
	configPaths := []string{
		"/etc/hmac-file-server/config.toml",
	}

	// Attempt to get the parent directory of the executable
	execPath, err := os.Executable()
	if err == nil {
		execDir := filepath.Dir(execPath)
		parentDir := filepath.Dir(execDir)
		fallbackConfig := filepath.Join(parentDir, "config.toml")
		configPaths = append(configPaths, fallbackConfig)
	}

	var configFile string
	for _, path := range configPaths {
		if _, err := os.Stat(path); err == nil {
			configFile = path
			break
		}
	}

	if configFile == "" {
		logrus.Fatalf("Error loading configuration: no config.toml found in /etc/hmac-file-server or parent directory of the executable")
	}

	logrus.Debugf("Loading configuration from: %s", configFile)

	// Load configuration
	logrus.Debug("Attempting to load configuration...")
	conf, err := config.LoadConfig(configFile)
	if err != nil {
		logrus.Fatalf("Error loading configuration from %s: %v", configFile, err)
	}

	logrus.Debug("Configuration successfully loaded.")
	logrus.Info("Configuration loaded successfully.")

	utils.SetupLogging(conf.Server.LogLevel, conf.Server.LogFile, conf.Server.LoggingJSON)

	utils.LogSystemInfo(versionString) // Log system information

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()


	logrus.Debug("Initializing worker pools and services...")

	// Initialize HMAC worker pool without using global variables
	logrus.Infof("HMAC worker pool initialized with %d workers.", conf.Workers.NumWorkers)

	// Initialize HMAC worker pool
	HMACWorkerPool := make(chan struct{}, conf.Workers.NumWorkers) // Added initialization
	for i := 0; i < conf.Workers.NumWorkers; i++ {
		HMACWorkerPool <- struct{}{}
	}

	// Initialize ClamAV worker pool
	clamAVWorkers := utils.AutoAdjustClamAVWorkers()
	ClamAVWorkerPool := make(chan struct{}, clamAVWorkers) // Added initialization
	for i := 0; i < clamAVWorkers; i++ {
		ClamAVWorkerPool <- struct{}{}
	}

	logrus.Infof("Initialized %d upload workers: [0 1 2 ... %d]", conf.Workers.NumWorkers, conf.Workers.NumWorkers-1)
	logrus.Infof("Initialized %d scan workers: [0 1 2 ... %d]", clamAVWorkers, clamAVWorkers-1)

	// Initialize ClamAV and Redis clients as needed
	var redisClient *redis.Client

	if conf.ClamAV.ClamAVEnabled {
		if _, err := initClamAV(conf.ClamAV.ClamAVSocket); err != nil {
			logrus.WithError(err).Fatalf("Failed to initialize ClamAV")
		}
		logrus.Info("ClamAV initialized successfully.")
	}

	if conf.Redis.RedisEnabled {
		redisClient = initRedis(ctx, conf.Redis)
		if redisClient == nil {
			logrus.Fatalf("Failed to initialize Redis client.")
		}
		logrus.Info("Redis client initialized successfully.")
	} else {
		logrus.Info("Redis is not enabled. Using fallback mechanisms.")
	}

	// Initialize caches
	var inMemoryCache *cache.Cache
	if !conf.Redis.RedisEnabled {
		inMemoryCache = cache.New(cache.DefaultExpiration, cache.DefaultExpiration)
	}

	// Setup Handler Dependencies
	var ClamAVClient *clamd.Clamd
	if conf.ClamAV.ClamAVEnabled {
		client, err := initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			logrus.Fatalf("Failed to initialize ClamAV: %v", err)
		}
		ClamAVClient = client
	}

	// Initialize HandlerDependencies
	handlerDeps := handlers.SetupHandlerDependencies(redisClient, inMemoryCache, ClamAVClient, HMACWorkerPool, ClamAVWorkerPool)

	router := handlers.SetupRouter(conf, handlerDeps)
	router.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		handleRequest(w, r, *conf)
	})

	// Start the cleanup routine with error handling
	go utils.CleanupExpiredFiles(ctx, conf.Server.StoragePath, conf.Server.FileTTL)

	// Remove the redundant /upload route defined using http.HandleFunc
	// http.HandleFunc("/upload", func(w http.ResponseWriter, r *http.Request) {
	//     // Example values for absFilename and a
	//     absFilename := "example.txt"
	//     a := r.URL.Query()
	//     handleUpload(w, r, absFilename, a, *conf)
	// })

	// Apply CORS middleware
	router.Use(handlers.CORSMiddleware)

	// Configure server timeouts
	server := &http.Server{
		Addr:         ":" + conf.Server.ListenPort,
		Handler:      router,
		ReadTimeout:  utils.ParseDurationOrDefault(conf.Timeouts.ReadTimeout, 5*time.Second),
		WriteTimeout: utils.ParseDurationOrDefault(conf.Timeouts.WriteTimeout, 10*time.Second),
		IdleTimeout:  utils.ParseDurationOrDefault(conf.Timeouts.IdleTimeout, 120*time.Second),
	}

	utils.SetupGracefulShutdown(server, ctx, cancel)

	// PID handling
	cleanupPID, err := utils.ManagePID(conf.Server.PidFilePath)
	if err != nil {
		logrus.Fatalf("PID management failed: %v", err)
	}
	if conf.Server.CleanupOnExit && cleanupPID != nil {
		defer cleanupPID()
	}

	logrus.Debug("Starting HTTP server...")

	go func() {
		logrus.Infof("Metrics server listening on :8081")
		http.Handle("/metrics", promhttp.Handler())
		if err := http.ListenAndServe(":8081", nil); err != nil {
			logrus.Errorf("Metrics server error: %v", err)
		}
	}()

	// Start the server
	logrus.Infof("Starting HMAC File Server on port %s...", conf.Server.ListenPort)
	if conf.Server.UnixSocket {
		listener, err := net.Listen("unix", conf.Server.ListenPort)
		if err != nil {
			logrus.Fatalf("Failed to listen on Unix socket: %v", err)
		}
		defer listener.Close()
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			logrus.Fatalf("Server error: %v", err)
		}
	} else {
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			logrus.Fatalf("Server error: %v", err)
		}
	}

	// Wait for context cancellation
	<-ctx.Done()

	// Close Redis client if initialized
	if redisClient != nil {
		if err := redisClient.Close(); err != nil {
			logrus.Errorf("Error closing Redis client: %v", err)
		} else {
			logrus.Info("Redis client closed successfully.")
		}
	}

	logrus.Info("Server shutdown complete.")
}

func initClamAV(socket string) (*clamd.Clamd, error) {
	client := clamd.NewClamd(socket)
	err := client.Ping()
	if err != nil {
		return nil, fmt.Errorf("error pinging ClamAV: %v", err)
	}
	return client, nil
}

func initRedis(ctx context.Context, conf config.RedisConfig) *redis.Client {
	rdb := redis.NewClient(&redis.Options{
		Addr:     conf.RedisAddr,
		Password: conf.RedisPassword,
		DB:       conf.RedisDBIndex,
	})
	_, err := rdb.Ping(ctx).Result()
	if err != nil {
		logrus.WithFields(logrus.Fields{
			"error": err,
		}).Error("Redis ping failed")
		return nil
	}
	return rdb
}

func handleUpload(w http.ResponseWriter, r *http.Request, a url.Values, conf config.Config) {
    // ...existing code...

    // Enhanced HMAC validation
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
        fileStorePath := ""
        mac.Write([]byte(fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)))
    } else {
        fileStorePath := ""
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

    // ...existing code...
}

// ...existing code...

func handleRequest(w http.ResponseWriter, r *http.Request, conf config.Config) {
	// ...existing code...

	p := ""
	fileStorePath := strings.TrimPrefix(p, "/")
	if fileStorePath == "" || fileStorePath == "/" {
		logrus.Warn("Access to root directory is forbidden")
		http.Error(w, "Forbidden", http.StatusForbidden)
		return
	} else if fileStorePath[0] == '/' {
		fileStorePath = fileStorePath[1:]
	}

	absFilename, err := sanitizeFilePath(conf.Server.StoragePath, fileStorePath)
	if err != nil {
		logrus.WithFields(logrus.Fields{"file": fileStorePath, "error": err}).Warn("Invalid file path")
		http.Error(w, "Invalid file path", http.StatusBadRequest)
		return
	}

	switch r.Method {
	case http.MethodPut:
		handleUpload(w, r, url.Values{}, conf)
	case http.MethodHead, http.MethodGet:
		handleDownload(w, r, absFilename, fileStorePath)
	case http.MethodOptions:
		w.Header().Set("Allow", "OPTIONS, GET, PUT, HEAD")
				return
			default:
				logrus.WithField("method", r.Method).Warn("Invalid HTTP method for upload directory")
				http.Error(w, "Method Not Allowed", http.StatusMethodNotAllowed)
				return
			}
		}

func handleDownload(w http.ResponseWriter, r *http.Request, absFilename any, fileStorePath string) {
	panic("unimplemented")
}

func sanitizeFilePath(s, fileStorePath string) (any, any) {
	panic("unimplemented")
}
		// ...existing code...