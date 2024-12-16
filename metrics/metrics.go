package metrics

import (
    "sync"

    "github.com/prometheus/client_golang/prometheus"
    "github.com/sirupsen/logrus"
)

var (
    once sync.Once

    // Define your metrics
    UploadErrorsTotal = prometheus.NewCounter(
        prometheus.CounterOpts{
            Name: "upload_errors_total",
            Help: "Total number of upload errors",
        },
    )

    // Add other metric definitions here
    // Example:
    UploadCounter = prometheus.NewCounter(
        prometheus.CounterOpts{
            Name: "upload_total",
            Help: "Total number of uploads",
        },
    )
)

// InitMetrics initializes and registers Prometheus metrics.
// It uses sync.Once to ensure metrics are registered only once.
func InitMetrics() {
    once.Do(func() {
        logrus.Info("Registering Prometheus metrics...")
        err := prometheus.Register(UploadErrorsTotal)
        if err != nil {
            logrus.Fatalf("Failed to register UploadErrorsTotal: %v", err)
        }

        err = prometheus.Register(UploadCounter)
        if err != nil {
            logrus.Fatalf("Failed to register UploadCounter: %v", err)
        }

        // Register other metrics here

        logrus.Info("Prometheus metrics initialized successfully.")
    })
}
