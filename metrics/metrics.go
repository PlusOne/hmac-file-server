package metrics

import (
	"context"
	"net/http"
	"sync"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/sirupsen/logrus"
)

var (
	once          sync.Once
	metricsServer *http.Server

	// UploadErrorsTotal tracks the total number of upload errors.
	UploadErrorsTotal = prometheus.NewCounter(
		prometheus.CounterOpts{
			Name: "upload_errors_total",
			Help: "Total number of upload errors",
		},
	)

	// UploadTotal tracks the total number of uploads.
	UploadTotal = prometheus.NewCounter(
		prometheus.CounterOpts{
			Name: "upload_total",
			Help: "Total number of uploads",
		},
	)
)

// InitMetrics initializes and registers Prometheus metrics.
func InitMetrics() {
	once.Do(func() {
		logrus.Info("Registering Prometheus metrics...")

		// Register metrics
		if err := prometheus.Register(UploadErrorsTotal); err != nil {
			logrus.Fatalf("Failed to register UploadErrorsTotal: %v", err)
		}

		if err := prometheus.Register(UploadTotal); err != nil {
			logrus.Fatalf("Failed to register UploadTotal: %v", err)
		}

		// Register other metrics here

		logrus.Info("Prometheus metrics initialized successfully.")
	})
}

// StartMetricsServer starts the Prometheus metrics HTTP server.
func StartMetricsServer(port string) {
	once.Do(func() {
		logrus.Infof("Metrics server starting on port %s", port)

		mux := http.NewServeMux()
		mux.Handle("/metrics", promhttp.Handler())

		metricsServer = &http.Server{
			Addr:    ":" + port,
			Handler: mux,
		}

		go func() {
			if err := metricsServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
				logrus.Fatalf("Metrics server failed: %v", err)
			}
		}()

		logrus.Infof("Metrics server started on port %s", port)
	})
}

// ShutdownMetricsServer gracefully shuts down the metrics server.
func ShutdownMetricsServer(ctx context.Context) error {
	if metricsServer != nil {
		logrus.Info("Shutting down metrics server...")
		return metricsServer.Shutdown(ctx)
	}
	return nil
}

