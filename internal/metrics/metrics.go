package metrics

import (
	"net/http"
	"runtime"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/mem"
	"github.com/sirupsen/logrus"
)

var (
	UploadDuration      prometheus.Histogram
	UploadErrorsTotal   prometheus.Counter
	UploadsTotal        prometheus.Counter
	DownloadDuration    prometheus.Histogram
	DownloadsTotal      prometheus.Counter
	DownloadErrorsTotal prometheus.Counter
	MemoryUsage         prometheus.Gauge
	CPUUsage            prometheus.Gauge
	ActiveConnections   prometheus.Gauge
	RequestsTotal       *prometheus.CounterVec
	Goroutines          prometheus.Gauge
	UploadSizeBytes     prometheus.Histogram
	DownloadSizeBytes   prometheus.Histogram
)

func InitMetrics(metricsEnabled bool, log *logrus.Logger) {
	UploadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{Name: "file_server_upload_duration_seconds"})
	UploadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{Name: "file_server_upload_errors_total"})
	UploadsTotal = prometheus.NewCounter(prometheus.CounterOpts{Name: "file_server_uploads_total"})
	DownloadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{Name: "file_server_download_duration_seconds"})
	DownloadsTotal = prometheus.NewCounter(prometheus.CounterOpts{Name: "file_server_downloads_total"})
	DownloadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{Name: "file_server_download_errors_total"})
	MemoryUsage = prometheus.NewGauge(prometheus.GaugeOpts{Name: "memory_usage_bytes"})
	CPUUsage = prometheus.NewGauge(prometheus.GaugeOpts{Name: "cpu_usage_percent"})
	ActiveConnections = prometheus.NewGauge(prometheus.GaugeOpts{Name: "active_connections_total"})
	RequestsTotal = prometheus.NewCounterVec(prometheus.CounterOpts{Name: "http_requests_total"}, []string{"method", "path"})
	Goroutines = prometheus.NewGauge(prometheus.GaugeOpts{Name: "goroutines_count"})
	UploadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{Name: "file_server_upload_size_bytes"})
	DownloadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{Name: "file_server_download_size_bytes"})

	if !metricsEnabled {
		log.Info("Prometheus metrics are disabled.")
		return
	}

	if metricsEnabled {
		prometheus.MustRegister(UploadDuration, UploadErrorsTotal, UploadsTotal)
		prometheus.MustRegister(DownloadDuration, DownloadsTotal, DownloadErrorsTotal)
		prometheus.MustRegister(MemoryUsage, CPUUsage, ActiveConnections, RequestsTotal, Goroutines, UploadSizeBytes, DownloadSizeBytes)
	}

	// Start Prometheus metrics server
	go func() {
		http.Handle("/metrics", promhttp.Handler())
		log.Infof("Prometheus metrics server is starting on %s", ":2112")
		if err := http.ListenAndServe(":2112", nil); err != nil {
			log.Fatalf("Failed to start Prometheus metrics server: %v", err)
		}
	}()
}

// Example system metric updates
func UpdateSystemMetrics() {
	v, _ := mem.VirtualMemory()
	MemoryUsage.Set(float64(v.Used) / float64(v.Total) * 100)
	c, _ := cpu.Percent(0, false)
	if len(c) > 0 {
		CPUUsage.Set(c[0])
	}
	Goroutines.Set(float64(runtime.NumGoroutine()))
}
