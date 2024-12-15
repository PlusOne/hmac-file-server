package metrics

import (
    "sync"

    "github.com/prometheus/client_golang/prometheus"
    "github.com/prometheus/client_golang/prometheus/promauto"
    "github.com/sirupsen/logrus"
)

var (
    opsProcessed = promauto.NewCounter(prometheus.CounterOpts{
        Name: "myapp_processed_ops_total",
        Help: "The total number of processed events",
    })
    once sync.Once
    uploadErrorsTotal = prometheus.NewCounter(
        prometheus.CounterOpts{
            Name: "upload_errors_total",
            Help: "Total number of upload errors",
        },
    )
    // Use sync.Once to ensure metrics are registered only once
    initOnce sync.Once

    // Define your metrics
    UploadCounter = prometheus.NewCounter(
        prometheus.CounterOpts{
            Name: "upload_counter",
            Help: "Number of file uploads",
        },
    )
    // ... other metrics ...
)

func InitMetrics() {
    once.Do(func() {
        // Register Prometheus metrics
        prometheus.MustRegister(uploadErrorsTotal)
        logrus.Info("Prometheus metrics initialized.")
    })
    initOnce.Do(func() {
        // Register metrics
        prometheus.MustRegister(UploadCounter)
        // Register other metrics
    })
}

func UpdateMetrics() {
    // Update your metrics here
    opsProcessed.Inc()
}
