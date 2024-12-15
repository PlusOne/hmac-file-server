package metrics

import (
    "github.com/prometheus/client_golang/prometheus"
    "github.com/prometheus/client_golang/prometheus/promauto"
)

var (
    opsProcessed = promauto.NewCounter(prometheus.CounterOpts{
        Name: "myapp_processed_ops_total",
        Help: "The total number of processed events",
    })
)

func InitMetrics() {
    // Initialize your metrics here
}

func UpdateMetrics() {
    // Update your metrics here
    opsProcessed.Inc()
}
