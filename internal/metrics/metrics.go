// Package metrics handles Prometheus metrics initialization and system monitoring.
package metrics

import (
	"runtime"
	"time"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/shirou/gopsutil/cpu"
	"github.com/sirupsen/logrus"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// Prometheus metrics - exported for use by other packages.
var (
	UploadDuration      prometheus.Histogram
	UploadErrorsTotal   prometheus.Counter
	UploadsTotal        prometheus.Counter
	DownloadDuration    prometheus.Histogram
	DownloadsTotal      prometheus.Counter
	DownloadErrorsTotal prometheus.Counter
	MemoryUsage         prometheus.Gauge
	CpuUsage            prometheus.Gauge
	ActiveConnections   prometheus.Gauge
	RequestsTotal       *prometheus.CounterVec
	Goroutines          prometheus.Gauge
	UploadSizeBytes     prometheus.Histogram
	DownloadSizeBytes   prometheus.Histogram

	FilesDeduplicatedTotal    prometheus.Counter
	DeduplicationErrorsTotal  prometheus.Counter
	IsoContainersCreatedTotal prometheus.Counter
	IsoCreationErrorsTotal    prometheus.Counter
	IsoContainersMountedTotal prometheus.Counter
	IsoMountErrorsTotal       prometheus.Counter

	WorkerAdjustmentsTotal   prometheus.Counter
	WorkerReAdjustmentsTotal prometheus.Counter
)

// InitMetrics registers all Prometheus metrics.
func InitMetrics() {
	UploadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name:    "upload_duration_seconds",
		Help:    "Duration of file uploads in seconds.",
		Buckets: prometheus.DefBuckets,
	})
	UploadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "upload_errors_total",
		Help: "Total number of upload errors.",
	})
	UploadsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "uploads_total",
		Help: "Total number of successful uploads.",
	})
	DownloadDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name:    "download_duration_seconds",
		Help:    "Duration of file downloads in seconds.",
		Buckets: prometheus.DefBuckets,
	})
	DownloadsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "downloads_total",
		Help: "Total number of successful downloads.",
	})
	DownloadErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "download_errors_total",
		Help: "Total number of download errors.",
	})
	MemoryUsage = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "memory_usage_bytes",
		Help: "Current memory usage in bytes.",
	})
	CpuUsage = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "cpu_usage_percent",
		Help: "Current CPU usage percentage.",
	})
	ActiveConnections = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "active_connections",
		Help: "Number of active connections.",
	})
	RequestsTotal = prometheus.NewCounterVec(prometheus.CounterOpts{
		Name: "requests_total",
		Help: "Total number of HTTP requests.",
	}, []string{"method", "path"})
	Goroutines = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "goroutines",
		Help: "Number of running goroutines.",
	})
	UploadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name:    "upload_size_bytes",
		Help:    "Size of uploaded files in bytes.",
		Buckets: prometheus.ExponentialBuckets(1024, 2, 20),
	})
	DownloadSizeBytes = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name:    "download_size_bytes",
		Help:    "Size of downloaded files in bytes.",
		Buckets: prometheus.ExponentialBuckets(1024, 2, 20),
	})
	FilesDeduplicatedTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "files_deduplicated_total",
		Help: "Total number of files deduplicated.",
	})
	DeduplicationErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "deduplication_errors_total",
		Help: "Total number of deduplication errors.",
	})
	IsoContainersCreatedTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "iso_containers_created_total",
		Help: "Total number of ISO containers created.",
	})
	IsoCreationErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "iso_creation_errors_total",
		Help: "Total number of ISO creation errors.",
	})
	IsoContainersMountedTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "iso_containers_mounted_total",
		Help: "Total number of ISO containers mounted.",
	})
	IsoMountErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "iso_mount_errors_total",
		Help: "Total number of ISO mount errors.",
	})
	WorkerAdjustmentsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "worker_adjustments_total",
		Help: "Total number of worker pool adjustments.",
	})
	WorkerReAdjustmentsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Name: "worker_readjustments_total",
		Help: "Total number of worker pool readjustments.",
	})

	prometheus.MustRegister(
		UploadDuration,
		UploadErrorsTotal,
		UploadsTotal,
		DownloadDuration,
		DownloadsTotal,
		DownloadErrorsTotal,
		MemoryUsage,
		CpuUsage,
		ActiveConnections,
		RequestsTotal,
		Goroutines,
		UploadSizeBytes,
		DownloadSizeBytes,
		FilesDeduplicatedTotal,
		DeduplicationErrorsTotal,
		IsoContainersCreatedTotal,
		IsoCreationErrorsTotal,
		IsoContainersMountedTotal,
		IsoMountErrorsTotal,
		WorkerAdjustmentsTotal,
		WorkerReAdjustmentsTotal,
	)

	log.Info("Prometheus metrics initialized")
}

// UpdateSystemMetrics updates memory, CPU, and goroutine metrics.
func UpdateSystemMetrics() {
	var m runtime.MemStats
	runtime.ReadMemStats(&m)
	MemoryUsage.Set(float64(m.Alloc))
	Goroutines.Set(float64(runtime.NumGoroutine()))

	cpuPercent, err := cpu.Percent(time.Second, false)
	if err == nil && len(cpuPercent) > 0 {
		CpuUsage.Set(cpuPercent[0])
	}
}
