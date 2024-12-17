package utils

import (
	"context" // Standard library
	"io"
	"net"
	"net/http"
	"os"
	"strings"
	"syscall"
	"time"

	"github.com/prometheus/client_golang/prometheus/promhttp" // Third-party imports
	"github.com/sirupsen/logrus"                             // Third-party imports
)

func SetupLogging(logLevel string, logFile string) {
	level, err := logrus.ParseLevel(logLevel)
	if err != nil {
		logrus.Fatalf("Invalid log level: %s", logLevel)
	}
	logrus.SetLevel(level)

	if logFile != "" {
		file, err := os.OpenFile(logFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			logrus.Fatalf("Failed to open log file: %v", err)
		}
		logrus.SetOutput(io.MultiWriter(os.Stdout, file))
	} else {
		logrus.SetOutput(os.Stdout)
	}

	logrus.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})
}

func PrometheusHandler() http.Handler {
	return promhttp.Handler()
}

func GetClientIP(r *http.Request) string {
	clientIP := r.Header.Get("X-Real-IP")
	if clientIP == "" {
		clientIP, _, _ = net.SplitHostPort(r.RemoteAddr)
	}
	return clientIP
}

func IsMultipart(contentType string) bool {
	return strings.Contains(contentType, "multipart/form-data")
}

func SetupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-quit
		logrus.Infof("Received signal %s. Initiating shutdown...", sig)

		ctxShutdown, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()

		if err := server.Shutdown(ctxShutdown); err != nil {
			logrus.Errorf("Error during server shutdown: %v", err)
		}
		cancel()

		logrus.Info("Shutdown process completed. Exiting application.")
		os.Exit(0)
	}()
}

func parseDuration(durationStr string) time.Duration {
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		logrus.Fatalf("Invalid duration: %s", durationStr)
	}
	return duration
}