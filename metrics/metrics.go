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
)

func InitMetrics() {
    once.Do(func() {
        // Register Prometheus metrics
        prometheus.MustRegister(uploadErrorsTotal)
        logrus.Info("Prometheus metrics initialized.")
    })
}

func UpdateMetrics() {
    // Update your metrics here
    opsProcessed.Inc()
}
