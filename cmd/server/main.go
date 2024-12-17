package main

import (
    "context"
    "flag"
    "net"
    "net/http"
    "os"
    "os/signal"
    "syscall"
    "time"

    "github.com/prometheus/client_golang/prometheus/promhttp"
)

func main() {
    setDefaults()

    var configFile string
    flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
    flag.Parse()

    err := readConfig(configFile, &conf)
    if err != nil {
        log.Fatalf("Error reading config: %v", err)
    }
    log.Info("Configuration loaded successfully.")

    initializeWorkerSettings(&conf.Server, &conf.Workers, &conf.ClamAV)

    if conf.ISO.Enabled {
        err = verifyAndCreateISOContainer()
        if err != nil {
            log.Fatalf("ISO container verification failed: %v", err)
        }
    }

    err = os.MkdirAll(conf.Server.StoragePath, os.ModePerm)
    if err != nil {
        log.Fatalf("Error creating store directory: %v", err)
    }
    log.WithField("directory", conf.Server.StoragePath).Info("Store directory is ready")

    err = checkFreeSpaceWithRetry(conf.Server.StoragePath, 3, 5*time.Second)
    if err != nil {
        log.Fatalf("Insufficient free space: %v", err)
    }

    setupLogging()
    logSystemInfo()
    initMetrics()
    log.Info("Prometheus metrics initialized.")

    ctx, cancel := context.WithCancel(context.Background())
    defer cancel()

    go monitorNetwork(ctx)
    go handleNetworkEvents(ctx)
    go updateSystemMetrics(ctx)

    if conf.ClamAV.ClamAVEnabled {
        clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
        if err != nil {
            log.WithError(err).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
        } else {
            log.Info("ClamAV client initialized successfully.")
        }
    }

    if conf.Redis.RedisEnabled {
        initRedis()
    }

    initializeUploadWorkerPool(ctx, &conf.Workers)
    if conf.ClamAV.ClamAVEnabled && clamClient != nil {
        initializeScanWorkerPool(ctx)
    }

    if conf.Redis.RedisEnabled && redisClient != nil {
        go MonitorRedisHealth(ctx, redisClient, parseDuration(conf.Redis.RedisHealthCheckInterval))
    }

    router := setupRouter()

    fileTTL, err := parseTTL(conf.Server.FileTTL)
    if err != nil {
        log.Fatalf("Invalid FileTTL: %v", err)
    }
    go runFileCleaner(ctx, conf.Server.StoragePath, fileTTL)

    readTimeout, err := time.ParseDuration(conf.Timeouts.ReadTimeout)
    if err != nil {
        log.Fatalf("Invalid ReadTimeout: %v", err)
    }

    writeTimeout, err := time.ParseDuration(conf.Timeouts.WriteTimeout)
    if err != nil {
        log.Fatalf("Invalid WriteTimeout: %v", err)
    }

    idleTimeout, err := time.ParseDuration(conf.Timeouts.IdleTimeout)
    if err != nil {
        log.Fatalf("Invalid IdleTimeout: %v", err)
    }

    server := &http.Server{
        Addr:           ":" + conf.Server.ListenPort,
        Handler:        router,
        ReadTimeout:    readTimeout,
        WriteTimeout:   writeTimeout,
        IdleTimeout:    idleTimeout,
        MaxHeaderBytes: 1 << 20, // 1 MB
    }

    if conf.Server.MetricsEnabled {
        go func() {
            http.Handle("/metrics", promhttp.Handler())
            log.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
            if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
                log.Fatalf("Metrics server failed: %v", err)
            }
        }()
    }

    setupGracefulShutdown(server, cancel)

    if conf.Server.AutoAdjustWorkers {
        go monitorWorkerPerformance(ctx, &conf.Server, &conf.Workers, &conf.ClamAV)
    }

    log.Infof("Starting HMAC file server %s...", versionString)
    if conf.Server.UnixSocket {
        if err := os.RemoveAll(conf.Server.ListenPort); err != nil {
            log.Fatalf("Failed to remove existing Unix socket: %v", err)
        }
        listener, err := net.Listen("unix", conf.Server.ListenPort)
        if err != nil {
            log.Fatalf("Failed to listen on Unix socket %s: %v", conf.Server.ListenPort, err)
        }
        defer listener.Close()
        if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
            log.Fatalf("Server failed: %v", err)
        }
    } else {
        if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
            log.Fatalf("Server failed: %v", err)
        }
    }
}
