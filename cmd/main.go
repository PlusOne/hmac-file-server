package main

import (
    "context"
    "errors"
    "flag"
    "fmt"
    "net"
    "net/http"
    "os"
    "os/signal"
    "path/filepath"
    "strings"
    "syscall"
    "time"

    "github.com/PlusOne/hmac-file-server/config"
    "github.com/PlusOne/hmac-file-server/handlers"
    "github.com/PlusOne/hmac-file-server/metrics"
    "github.com/PlusOne/hmac-file-server/workers"
    "github.com/patrickmn/go-cache"
    "github.com/prometheus/client_golang/prometheus/promhttp"
    "github.com/sirupsen/logrus"
)

var (
    fileInfoCache *cache.Cache
    uploadQueue   chan workers.UploadTask
    scanQueue     chan workers.ScanTask
    conf          config.Config
    clamClient    *workers.ClamAVClient
    networkEvents chan NetworkEvent
    versionString = "1.0.0"
)

type NetworkEvent struct {
    EventType string
    Timestamp time.Time
}

func initClamAV(socket string) (*workers.ClamAVClient, error) {
    if socket == "" {
        return nil, errors.New("ClamAV socket path is empty")
    }

    logrus.Printf("Initializing ClamAV client with socket: %s", socket)
    return &workers.ClamAVClient{Socket: socket}, nil
}

func setupRouter() *http.ServeMux {
    router := http.NewServeMux()
    router.HandleFunc("/upload", handlers.UploadHandler)
    return router
}

func setDefaults() {
    conf.Server.ListenPort = "8080"
    conf.Server.StoragePath = "./storage"
    conf.Server.FileTTL = "24h"
    conf.Server.MetricsEnabled = true
    conf.Server.MetricsPort = "2112"
    conf.Server.UnixSocket = false

    conf.ISO.Enabled = false

    conf.Timeouts.ReadTimeout = "10s"
    conf.Timeouts.WriteTimeout = "10s"
    conf.Timeouts.IdleTimeout = "60s"

    conf.Workers.UploadQueueSize = 10

    conf.ClamAV.ClamAVEnabled = false
    conf.ClamAV.ClamAVSocket = "/var/run/clamav/clamd.ctl"

    conf.Redis.RedisEnabled = false
    conf.Redis.RedisHealthCheckInterval = "30s"
}

func readConfig(configFile string, conf *config.Config) error {
    return config.LoadConfig(configFile)
}

func monitorNetwork(ctx context.Context) {
    for {
        select {
        case <-ctx.Done():
            return
        default:
            networkEvents <- NetworkEvent{
                EventType: "NETWORK_CHANGE",
                Timestamp: time.Now(),
            }
            time.Sleep(10 * time.Second)
        }
    }
}

func handleNetworkEvents(ctx context.Context) {
    for {
        select {
        case <-ctx.Done():
            return
        case event := <-networkEvents:
            logrus.Infof("Handling network event: %s at %s", event.EventType, event.Timestamp)
        }
    }
}

func runFileCleaner(ctx context.Context, storagePath string, ttl time.Duration) {
    ticker := time.NewTicker(ttl)
    defer ticker.Stop()

    for {
        select {
        case <-ctx.Done():
            return
        case <-ticker.C:
            cleanupOldFiles(storagePath, ttl)
        }
    }
}

func cleanupOldFiles(storagePath string, ttl time.Duration) {
    files, err := os.ReadDir(storagePath)
    if err != nil {
        logrus.Errorf("Error reading storage directory: %v", err)
        return
    }
    cutoff := time.Now().Add(-ttl)
    for _, file := range files {
        filePath := filepath.Join(storagePath, file.Name())
        info, err := os.Stat(filePath)
        if err != nil {
            logrus.Errorf("Error stating file %s: %v", filePath, err)
            continue
        }
        if info.ModTime().Before(cutoff) {
            err := os.Remove(filePath)
            if err != nil {
                logrus.Errorf("Error removing file %s: %v", filePath, err)
            } else {
                logrus.Infof("Removed old file: %s", filePath)
            }
        }
    }
}

func checkFreeSpaceWithRetry(path string, retries int, delay time.Duration) error {
    for i := 0; i < retries; i++ {
        if checkFreeSpace(path) {
            return nil
        }
        time.Sleep(delay)
    }
    return fmt.Errorf("insufficient free space after %d retries", retries)
}

func checkFreeSpace(path string) bool {
    var stat syscall.Statfs_t
    if err := syscall.Statfs(path, &stat); err != nil {
        logrus.Errorf("Error getting filesystem stats: %v", err)
        return false
    }
    freeSpace := stat.Bavail * uint64(stat.Bsize)
    requiredSpace := uint64(1 << 30)
    return freeSpace > requiredSpace
}

func parseDuration(durationStr string) time.Duration {
    duration, err := time.ParseDuration(durationStr)
    if err != nil {
        logrus.Warnf("Invalid duration '%s', defaulting to 30s", durationStr)
        return 30 * time.Second
    }
    return duration
}

func logSystemInfo() {
    logrus.Info("Logging system information...")
}

func setupLogging() {
    logrus.SetFormatter(&logrus.TextFormatter{
        FullTimestamp: true,
    })
    logrus.SetOutput(os.Stdout)
    logrus.SetLevel(logrus.InfoLevel)
}

func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
    c := make(chan os.Signal, 1)
    signal.Notify(c, os.Interrupt)
    go func() {
        <-c
        logrus.Info("Shutting down server...")
        if err := server.Shutdown(context.Background()); err != nil {
            logrus.Fatalf("Server Shutdown Failed:%+v", err)
        }
        cancel()
    }()
}

func verifyAndCreateISOContainer() error {
    return nil
}

func main() {
    setDefaults()

    var configFile string
    flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
    flag.Parse()

    err := readConfig(configFile, &conf)
    if err != nil {
        logrus.Fatalf("Error reading config: %v", err)
    }
    logrus.Info("Configuration loaded successfully.")

    if conf.ISO.Enabled {
        err = verifyAndCreateISOContainer()
        if err != nil {
            logrus.Fatalf("ISO container verification failed: %v", err)
        }
    }

    fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)

    err = os.MkdirAll(conf.Server.StoragePath, os.ModePerm)
    if err != nil {
        logrus.Fatalf("Error creating store directory: %v", err)
    }
    logrus.WithField("directory", conf.Server.StoragePath).Info("Store directory is ready")

    err = checkFreeSpaceWithRetry(conf.Server.StoragePath, 3, 5*time.Second)
    if err != nil {
        logrus.Fatalf("Insufficient free space: %v", err)
    }

    setupLogging()

    logSystemInfo()

    metrics.InitMetrics()
    logrus.Info("Prometheus metrics initialized.")

    uploadQueue = make(chan workers.UploadTask, conf.Workers.UploadQueueSize)
    scanQueue = make(chan workers.ScanTask, conf.Workers.UploadQueueSize)
    networkEvents = make(chan NetworkEvent, 100)
    logrus.Info("Upload, scan, and network event channels initialized.")

    handlers.InitHandlers(uploadQueue, scanQueue, &conf)

    ctx, cancel := context.WithCancel(context.Background())
    defer cancel()

    go monitorNetwork(ctx)
    go handleNetworkEvents(ctx)

    go metrics.UpdateMetrics()

    if conf.ClamAV.ClamAVEnabled {
        clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
        if err != nil {
            logrus.WithFields(logrus.Fields{
                "error": err.Error(),
            }).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
        } else {
            logrus.Info("ClamAV client initialized successfully.")
        }
    }

    if conf.Redis.RedisEnabled {
        logrus.Info("Redis client initialization is enabled.")
    }

    workers.InitializeUploadWorkerPool(ctx, uploadQueue, conf.Workers.UploadQueueSize)

    if conf.ClamAV.ClamAVEnabled && clamClient != nil {
        workers.InitializeScanWorkerPool(ctx, clamClient, scanQueue, conf.Workers.UploadQueueSize)
    }

    if conf.Redis.RedisEnabled {
    }

    router := setupRouter()

    fileTTL, err := time.ParseDuration(conf.Server.FileTTL)
    if err != nil {
        logrus.Fatalf("Invalid FileTTL: %v", err)
    }
    go runFileCleaner(ctx, conf.Server.StoragePath, fileTTL)

    readTimeout, err := time.ParseDuration(conf.Timeouts.ReadTimeout)
    if err != nil {
        logrus.Fatalf("Invalid ReadTimeout: %v", err)
    }

    writeTimeout, err := time.ParseDuration(conf.Timeouts.WriteTimeout)
    if err != nil {
        logrus.Fatalf("Invalid WriteTimeout: %v", err)
    }

    idleTimeout, err := time.ParseDuration(conf.Timeouts.IdleTimeout)
    if err != nil {
        logrus.Fatalf("Invalid IdleTimeout: %v", err)
    }

    server := &http.Server{
        Addr:         ":" + conf.Server.ListenPort,
        Handler:      router,
        ReadTimeout:  readTimeout,
        WriteTimeout: writeTimeout,
        IdleTimeout:  idleTimeout,
    }

    if conf.Server.MetricsEnabled {
        go func() {
            http.Handle("/metrics", promhttp.Handler())
            logrus.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
            if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
                logrus.Fatalf("Metrics server failed: %v", err)
            }
        }()
    }

    setupGracefulShutdown(server, cancel)

    logrus.Infof("Starting HMAC file server %s...", versionString)
    if conf.Server.UnixSocket {
        if err := os.RemoveAll(conf.Server.ListenPort); err != nil {
            logrus.Fatalf("Failed to remove existing Unix socket: %v", err)
        }
        listener, err := net.Listen("unix", conf.Server.ListenPort)
        if err != nil {
            logrus.Fatalf("Failed to listen on Unix socket %s: %v", conf.Server.ListenPort, err)
        }
        defer listener.Close()
        if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
            logrus.Fatalf("Server failed: %v", err)
        }
    } else {
        if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
            logrus.Fatalf("Server failed: %v", err)
        }
    }
}
