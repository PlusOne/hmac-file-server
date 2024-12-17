package main

import (
	"context"
	"net"
	"net/http"
	"os"
	"os/signal"
	"syscall"
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
		ReadTimeout:  parseDuration(conf.Timeouts.ReadTimeout),
		WriteTimeout: parseDuration(conf.Timeouts.WriteTimeout),
		IdleTimeout:  parseDuration(conf.Timeouts.IdleTimeout),
	}

	// Setup graceful shutdown
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	utils.SetupGracefulShutdown(server, cancel)

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