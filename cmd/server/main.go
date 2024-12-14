package main

import (
	"context"
	"flag"
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/sirupsen/logrus"
)

// Config structure for demonstration
type Config struct {
	ListenPort string `mapstructure:"listen_port"`
	LogLevel   string `mapstructure:"log_level"`
}

var (
	log    = logrus.New()
	conf   Config
	ctx, cancel = context.WithCancel(context.Background())
)

func main() {
	// Flags for configuration file
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	// Set defaults and then load config
	setDefaults()
	if err := loadConfig(configFile); err != nil {
		log.Fatalf("Error reading config: %v", err)
	}

	// Set log level
	level, err := logrus.ParseLevel(conf.LogLevel)
	if err != nil {
		log.Fatalf("Invalid log level: %v", err)
	}
	log.SetLevel(level)

	// Validate configuration
	if err := validateConfig(&conf); err != nil {
		log.Fatalf("Configuration validation failed: %v", err)
	}

	// Setup HTTP server with sample route
	mux := http.NewServeMux()
	mux.HandleFunc("/", helloHandler)

	server := &http.Server{
		Addr:    ":" + conf.ListenPort,
		Handler: mux,
	}

	// Setup graceful shutdown
	quit := make(chan os.Signal, 1)
	setupGracefulShutdown(server, cancel, quit)

	// Start the server
	log.Infof("Starting server on port %s...", conf.ListenPort)
	if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		log.Fatalf("Could not listen on %s: %v", server.Addr, err)
	}

	<-ctx.Done() // Wait until context is canceled
	log.Println("Shutting down...")
}

func setDefaults() {
	// Set default configuration values
	conf.ListenPort = "8080"
	conf.LogLevel = "info"
}

func loadConfig(configFile string) error {
	// Here you would actually parse your config file (e.g., using viper)
	// For demonstration, we'll just print that we're "loading" it.
	log.Infof("Loading configuration from: %s", configFile)

	// Assume the configuration was loaded successfully and stored into 'c'.
	// TODO: Implement real configuration loading.

	return nil
}

func validateConfig(c *Config) error {
	if c.ListenPort == "" {
		return fmt.Errorf("listen_port must be set")
	}
	if c.LogLevel == "" {
		return fmt.Errorf("log_level must be set")
	}
	return nil
}

func helloHandler(w http.ResponseWriter, r *http.Request) {
	log.Info("Received request for /")
	w.Write([]byte("Hello, world!"))
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
