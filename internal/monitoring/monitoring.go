package monitoring

import (
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/renz/hmac-file-server/internal/config"
	"github.com/sirupsen/logrus"
	"net/http"
	"os"
	"io"
)

func Setup(conf *config.Config) {
	if conf.Server.MetricsEnabled {
		http.Handle("/metrics", promhttp.Handler())
		// Start Prometheus metrics server
		go func() {
			logrus.Infof("Prometheus metrics available at :%s/metrics", conf.Server.MetricsPort)
			if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
				logrus.Fatalf("Failed to start metrics server: %v", err)
			}
		}()
	}
}

func InitializeLogging(conf *config.Config) {
	level, err := logrus.ParseLevel(conf.Server.LogLevel)
	if err != nil {
		logrus.Fatalf("Invalid log level: %s", conf.Server.LogLevel)
	}
	logrus.SetLevel(level)

	if conf.Server.LogFile != "" {
		logFile, err := os.OpenFile(conf.Server.LogFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			logrus.Fatalf("Failed to open log file: %v", err)
		}
		logrus.SetOutput(io.MultiWriter(os.Stdout, logFile))
	} else {
		logrus.SetOutput(os.Stdout)
	}

	logrus.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})
}

// ...additional monitoring setups...
