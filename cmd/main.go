package main

import (
    "context"
    "net"
    "net/http"
    "os"
    "os/signal"
    "syscall"
    "time"

    "github.com/PlusOne/hmac-file-server/config"
    "github.com/PlusOne/hmac-file-server/handlers"
    "github.com/PlusOne/hmac-file-server/middleware"
    "github.com/PlusOne/hmac-file-server/metrics"
    "github.com/PlusOne/hmac-file-server/workers"

    "github.com/prometheus/client_golang/prometheus/promhttp"
    "github.com/sirupsen/logrus"
)

func main() {
    // Initialize logger
    logrus.SetFormatter(&logrus.TextFormatter{FullTimestamp: true})
    logrus.SetLevel(logrus.InfoLevel)

    // Load configuration
    err := config.LoadConfig("./config.toml")
    if err != nil {
        logrus.Fatalf("Failed to load config: %v", err)
    }

    // Initialize metrics
    metrics.InitMetrics()

    // Initialize workers
    ctx, cancel := context.WithCancel(context.Background())
    defer cancel()
    workers.InitializeWorkers(ctx)

    // Setup router
    router := http.NewServeMux()
    router.HandleFunc("/", handlers.HandleRequest)
    if config.Conf.Server.MetricsEnabled {
        router.Handle("/metrics", promhttp.Handler())
    }

    // Apply middleware
    handler := middleware.LoggingMiddleware(router)
    handler = middleware.RecoveryMiddleware(handler)
    handler = middleware.CORSMiddleware(handler)

    // Create HTTP server
    server := &http.Server{
        Addr:         ":" + config.Conf.Server.ListenPort,
        Handler:      handler,
        ReadTimeout:  parseDuration(config.Conf.Timeouts.ReadTimeout),
        WriteTimeout: parseDuration(config.Conf.Timeouts.WriteTimeout),
        IdleTimeout:  parseDuration(config.Conf.Timeouts.IdleTimeout),
    }

    // Start server in a goroutine
    go func() {
        if config.Conf.Server.UnixSocket {
            if err := os.RemoveAll(config.Conf.Server.ListenPort); err != nil {
                logrus.Fatalf("Failed to remove Unix socket: %v", err)
            }
            listener, err := net.Listen("unix", config.Conf.Server.ListenPort)
            if err != nil {
                logrus.Fatalf("Failed to listen on Unix socket: %v", err)
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
    }()

    logrus.Infof("Server started on %s", server.Addr)

    // Graceful shutdown
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
    <-quit
    logrus.Info("Shutting down server...")

    ctxShutdown, cancelShutdown := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancelShutdown()

    if err := server.Shutdown(ctxShutdown); err != nil {
        logrus.Fatalf("Server forced to shutdown: %v", err)
    }

    // Cancel workers
    cancel()

    logrus.Info("Server exiting")
}

func parseDuration(durationStr string) time.Duration {
    duration, err := time.ParseDuration(durationStr)
    if err != nil {
        logrus.Warnf("Invalid duration '%s', defaulting to 30s", durationStr)
        return 30 * time.Second
    }
    return duration
}