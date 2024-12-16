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
    // ... define other metrics here ...
)

func InitMetrics() {
    once.Do(func() {
        // Register metrics
        prometheus.MustRegister(UploadErrorsTotal)
        // Register other metrics here
        logrus.Info("Prometheus metrics initialized.")
    })
}
