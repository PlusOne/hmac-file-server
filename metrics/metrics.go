package metrics

import (
	"sync"
	"testing"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/sirupsen/logrus"
)

var (
	once sync.Once

	// Define your metrics
	UploadErrorsTotal = prometheus.NewCounter(
		prometheus.CounterOpts{
			Name: "upload_errors_total",
			Help: "Total number of upload errors",
		},
	)

	// Example of another metric
	UploadTotal = prometheus.NewCounter(
		prometheus.CounterOpts{
			Name: "upload_total",
			Help: "Total number of uploads",
		},
	)
)

// InitMetrics initializes and registers Prometheus metrics.
// It uses sync.Once to ensure metrics are registered only once.
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

func TestInitMetrics(t *testing.T) {
	InitMetrics()

	// Attempt to register the metrics again
	// This should not cause a panic due to sync.Once
	defer func() {
		if r := recover(); r != nil {
			t.Errorf("InitMetrics caused a panic: %v", r)
		}
	}()

	InitMetrics()
}

func TestMetricsAreRegistered(t *testing.T) {
	InitMetrics()

	// Check if the metrics are registered
	uploader := prometheus.NewRegistry()
	uploader.MustRegister(UploadErrorsTotal)

	_, err := uploader.Gather()
	if err != nil {
		t.Errorf("Failed to gather metrics: %v", err)
	}
}
