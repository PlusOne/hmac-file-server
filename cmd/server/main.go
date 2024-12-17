package main

import (
	"context" // Added missing "context" package
	"net"
	"net/http"
	"time"

	"github.com/renz/hmac-file-server/config"
	"github.com/renz/hmac-file-server/handlers"
	"github.com/renz/hmac-file-server/utils"
	"github.com/sirupsen/logrus"
)

func main() {
	// Load configuration
	conf, err := config.LoadConfig("config.toml")
	if err != nil {
		logrus.Fatalf("Error loading configuration: %v", err)
	}
	logrus.Info("Configuration loaded successfully.")

	// Setup logging
	utils.SetupLogging(conf.Server.LogLevel, conf.Server.LogFile)

	// Initialize other components (e.g., ClamAV, Redis) as needed
	// ...existing initialization code...

	// Initialize HTTP handlers
	router := handlers.SetupRouter(conf)

	// Create HTTP server
	server := &http.Server{
		Addr:         ":" + conf.Server.ListenPort,
		Handler:      router,
		ReadTimeout:  timeParseDuration(conf.Timeouts.ReadTimeout),
		WriteTimeout: timeParseDuration(conf.Timeouts.WriteTimeout),
		IdleTimeout:  timeParseDuration(conf.Timeouts.IdleTimeout),
	}

	// Setup graceful shutdown
	_, cancelFunc := context.WithCancel(context.Background())
	defer cancelFunc()
	utils.SetupGracefulShutdown(server, cancelFunc)

	// Start the server
	logrus.Infof("Starting HMAC File Server on port %s...", conf.Server.ListenPort)
	if conf.Server.UnixSocket {
		listener, err := net.Listen("unix", conf.Server.ListenPort)
		if err != nil {
			logrus.Fatalf("Failed to listen on Unix socket: %v", err)
		}
		defer listener.Close()
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			logrus.Fatalf("Server error: %v", err)
		}
	} else {
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			logrus.Fatalf("Server error: %v", err)
		}
	}
}

// timeParseDuration parses a duration string and handles the error.
func timeParseDuration(durationStr string) time.Duration {
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		logrus.Fatalf("Invalid duration: %s", durationStr)
	}
	return duration
}