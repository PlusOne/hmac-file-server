package main

import (
	"context"
	"flag"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"
)

func main() {
	// Set default configuration values
	setDefaults()

	// Flags for configuration file
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	// Load configuration
	err := loadConfig()
	if err != nil {
		log.Fatalf("Error reading config: %v", err) // Fatal: application cannot proceed
	}

	// Create and start server
	server := &http.Server{Addr: ":8080"}

	// Context to control shutdown
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Channel to listen for OS signals
	quit := make(chan os.Signal, 1)
	setupGracefulShutdown(server, cancel, quit)

	// Start the server
	if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		log.Fatalf("Could not listen on %s: %v", server.Addr, err)
	}

	<-ctx.Done() // Wait until context is canceled
	log.Println("Shutting down...")
}

func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc, quit chan os.Signal) {
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		sig := <-quit
		log.Printf("Received signal %s. Initiating shutdown...", sig)

		ctxShutdown, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()

		if err := server.Shutdown(ctxShutdown); err != nil {
			log.Printf("Server shutdown failed: %v", err)
		} else {
			log.Println("Server gracefully stopped.")
		}

		// Additional cleanup if needed
		cancel()
	}()
}

func setDefaults() {
	// Set default values or configurations here
}

func loadConfig() error {
	// Load configuration from the given path
	// TODO: Implement actual config loading
	return nil
}
