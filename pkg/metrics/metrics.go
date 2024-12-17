
package metrics

import (
	"github.com/prometheus/client_golang/prometheus"
)

var (
	// Define all metrics variables here
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

func InitMetrics(enabled bool) {
	if !enabled {
		return
	}

	uploadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_upload_duration_seconds",
		Help:      "Histogram of file upload duration.",
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
		Help:      "Histogram of file download duration.",
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
		Name:      "memory_usage_bytes",
		Help:      "Current memory usage in bytes.",
	})
	cpuUsage = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "cpu_usage_percent",
		Help:      "CPU usage as a percentage.",
	})
	activeConnections = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "active_connections_total",
		Help:      "Total number of active connections.",
	})
	requestsTotal = prometheus.NewCounterVec(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "http_requests_total",
		Help:      "Total HTTP requests.",
	}, []string{"method", "path"})
	goroutines = prometheus.NewGauge(prometheus.GaugeOpts{
		Namespace: "hmac",
		Name:      "goroutines_count",
		Help:      "Number of goroutines.",
	})
	uploadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_upload_size_bytes",
		Help:      "Histogram of uploaded file sizes.",
	})
	downloadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Namespace: "hmac",
		Name:      "file_server_download_size_bytes",
		Help:      "Histogram of downloaded file sizes.",
	})

	prometheus.MustRegister(uploadDuration, uploadErrorsTotal, uploadsTotal)
	prometheus.MustRegister(downloadDuration, downloadsTotal, downloadErrorsTotal)
	prometheus.MustRegister(memoryUsage, cpuUsage, activeConnections, requestsTotal, goroutines, uploadSizeBytes, downloadSizeBytes)
}

func ObserveUploadDuration(seconds float64) {
	uploadDuration.Observe(seconds)
}

func IncUploadErrorsTotal() {
	uploadErrorsTotal.Inc()
}

func IncUploadsTotal() {
	uploadsTotal.Inc()
}

func ObserveDownloadDuration(seconds float64) {
	downloadDuration.Observe(seconds)
}

func IncDownloadsTotal() {
	downloadsTotal.Inc()
}

func IncDownloadErrorsTotal() {
	downloadErrorsTotal.Inc()
}

func SetMemoryUsage(bytes float64) {
	memoryUsage.Set(bytes)
}

func SetCPUUsage(percent float64) {
	cpuUsage.Set(percent)
}

func SetActiveConnections(total float64) {
	activeConnections.Set(total)
}

func IncRequestsTotal(method, path string) {
	requestsTotal.WithLabelValues(method, path).Inc()
}

func SetGoroutines(count float64) {
	goroutines.Set(count)
}

func ObserveUploadSizeBytes(bytes float64) {
	uploadSizeBytes.Observe(bytes)
}

func ObserveDownloadSizeBytes(bytes float64) {
	downloadSizeBytes.Observe(bytes)
}