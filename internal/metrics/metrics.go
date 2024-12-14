// internal/metrics/metrics.go

package metrics

import (
	"context"
	"runtime"
	"time"

	"github.com/PlusOne/hmac-file-server/internal/config"

	"github.com/prometheus/client_golang/prometheus"
)

var (
	uploadDuration      prometheus.Histogram
	uploadErrorsTotal   prometheus.Counter
	uploadsTotal        prometheus.Counter
	downloadDuration    prometheus.Histogram
	downloadsTotal      prometheus.Counter
	downloadErrorsTotal prometheus.Counter
	memoryUsage         prometheus.Gauge
	cpuUsage            prometheus.Gauge
	activeConnections   prometheus.Gauge
	requestsTotal       *prometheus.CounterVec
	goroutines          prometheus.Gauge
	uploadSizeBytes     prometheus.Histogram
	downloadSizeBytes   prometheus.Histogram
)

// InitMetrics initialisiert und registriert die Prometheus-Metriken
func InitMetrics() {
	uploadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_upload_duration_seconds",
		Help:      "Histogram of file upload duration in seconds.",
		Buckets:   prometheus.DefBuckets,
	})
	uploadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "file_server_upload_errors_total",
		Help:      "Total number of file upload errors.",
	})
	uploadsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "file_server_uploads_total",
		Help:      "Total number of successful file uploads.",
	})
	downloadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_download_duration_seconds",
		Help:      "Histogram of file download duration in seconds.",
		Buckets:   prometheus.DefBuckets,
	})
	downloadsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "file_server_downloads_total",
		Help:      "Total number of successful file downloads.",
	})
	downloadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "file_server_download_errors_total",
		Help:      "Total number of file download errors.",
	})
	memoryUsage = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "memory_usage_percent",
		Help:      "Current memory usage as a percentage.",
	})
	cpuUsage = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "cpu_usage_percent",
		Help:      "Current CPU usage as a percentage.",
	})
	activeConnections = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "active_connections_total",
		Help:      "Total number of active connections.",
	})
	requestsTotal = prometheus.NewCounterVec(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "http_requests_total",
		Help:      "Total number of HTTP requests received, labeled by method and path.",
	}, []string{"method", "path"})
	goroutines = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "goroutines_count",
		Help:      "Current number of goroutines.",
	})
	uploadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_upload_size_bytes",
		Help:      "Histogram of uploaded file sizes in bytes.",
		Buckets:   prometheus.ExponentialBuckets(100, 10, 8),
	})
	downloadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_download_size_bytes",
		Help:      "Histogram of downloaded file sizes in bytes.",
		Buckets:   prometheus.ExponentialBuckets(100, 10, 8),
	})

	if config.Conf.Server.MetricsEnabled {
		prometheus.MustRegister(uploadDuration, uploadErrorsTotal, uploadsTotal)
		prometheus.MustRegister(downloadDuration, downloadsTotal, downloadErrorsTotal)
		prometheus.MustRegister(memoryUsage, cpuUsage, activeConnections, requestsTotal, goroutines)
		prometheus.MustRegister(uploadSizeBytes, downloadSizeBytes)
	}
}

// UpdateSystemMetrics aktualisiert die Systemmetriken periodisch
func UpdateSystemMetrics(ctx context.Context) {
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			v, err := getMemoryUsage()
			if err == nil {
				memoryUsage.Set(v)
			}

			cpuPerc, err := getCPUUsage()
			if err == nil {
				cpuUsage.Set(cpuPerc)
			}

			goroutines.Set(float64(runtime.NumGoroutine()))
		}
	}
}

// Placeholder-Funktionen für Memory und CPU Usage
func getMemoryUsage() (float64, error) {
	// Implementiere die tatsächliche Logik zur Ermittlung der Speicherbenutzung
	return 50.0, nil
}

func getCPUUsage() (float64, error) {
	// Implementiere die tatsächliche Logik zur Ermittlung der CPU-Auslastung
	return 20.0, nil
}
