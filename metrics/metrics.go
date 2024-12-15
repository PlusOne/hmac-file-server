package metrics

import (
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
)

var (
	UploadsTotal         prometheus.Counter
	UploadsErrorsTotal   prometheus.Counter
	DownloadsTotal       prometheus.Counter
	DownloadsErrorsTotal prometheus.Counter
	HttpRequestsTotal    *prometheus.CounterVec
	opsProcessed         = promauto.NewCounter(prometheus.CounterOpts{
		Name: "myapp_processed_ops_total",
		Help: "The total number of processed events",
	})
)

func InitMetrics() {
	UploadsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "uploads_total",
		Help:      "Total number of uploads",
	})
	UploadsErrorsTotal = prometheus.NewCounter(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "upload_errors_total",
		Help:      "Total upload errors",
	})
	// Initialize other metrics...

	HttpRequestsTotal = prometheus.NewCounterVec(prometheus.CounterOpts{
		Namespace: "hmac",
		Name:      "http_requests_total",
		Help:      "Total HTTP requests",
	}, []string{"method", "path"})

	prometheus.MustRegister(UploadsTotal, UploadsErrorsTotal, DownloadsTotal, DownloadsErrorsTotal, HttpRequestsTotal)
}

func IncRequest(method, path string) {
	HttpRequestsTotal.WithLabelValues(method, path).Inc()
}

func UpdateMetrics() {
	opsProcessed.Inc()
}
