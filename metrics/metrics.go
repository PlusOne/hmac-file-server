package metrics

import (
	"context"
	"fmt"
	"net/http"
	"sync"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/sirupsen/logrus"
)

var (
	metricsOnce sync.Once
	serverOnce  sync.Once

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
	metricsOnce.Do(func() {
		logrus.Info("Registering Prometheus metrics...")
		logrus.SetLevel(logrus.DebugLevel)

		// Register metrics
		prometheus.MustRegister(UploadErrorsTotal)
		if err := prometheus.Register(UploadTotal); err != nil {
			logrus.Fatalf("Failed to register UploadTotal: %v", err)
		}

		// Register other metrics here

		logrus.Info("Prometheus metrics initialized successfully.")
	})
}

// StartMetricsServer starts the Prometheus metrics HTTP server.
func StartMetricsServer(port string) {
	http.Handle("/metrics", promhttp.Handler())
	logrus.Infof("Metrics server started on port %s", port)
	if err := http.ListenAndServe(":"+port, nil); err != nil {
		logrus.Fatalf("Metrics server failed: %v", err)
	}
}

// ShutdownMetricsServer gracefully shuts down the metrics server.
func ShutdownMetricsServer(ctx context.Context) error {
	if metricsServer != nil {
		logrus.Info("Shutting down metrics server...")
		return metricsServer.Shutdown(ctx)
	}
	return nil
}

type Config struct {
	Server struct {
		MetricsPort string
	}
}

func validateConfig(conf *Config) error {
	if conf.Server.MetricsPort == "" {
		return fmt.Errorf("metrics port is not set in configuration")
	}
	// Add more validation as needed
	return nil
}
