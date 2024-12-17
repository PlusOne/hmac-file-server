package main

import (
    "github.com/prometheus/client_golang/prometheus"
    "github.com/prometheus/client_golang/prometheus/promhttp"
    "net/http"
)

var (
    requestCount = prometheus.NewCounter(prometheus.CounterOpts{
        Name: "http_requests_total",
        Help: "Total number of HTTP requests",
    })
)

func initMetrics() {
    prometheus.MustRegister(requestCount)
}

func startMetricsServer() {
    http.Handle("/metrics", promhttp.Handler())
    go func() {
        log.Infof("Starting metrics server on %s", conf.Server.MetricsPort)
        if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
            log.Fatalf("Metrics server failed: %v", err)
        }
    }()
}
