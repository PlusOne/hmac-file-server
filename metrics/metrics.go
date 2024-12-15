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
    // ... other metrics ...
)

func InitMetrics() {
    once.Do(func() {
        // Register metrics
        prometheus.MustRegister(UploadErrorsTotal)
        // Register other metrics
        logrus.Info("Prometheus metrics initialized.")
    })
}
