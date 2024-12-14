package server

import (
	"context"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/sirupsen/logrus"
)

// Start initializes and starts the HTTP server with graceful shutdown.
func Start(ctx context.Context, server *http.Server, log *logrus.Logger) {
	// Channel to listen for interrupt or terminate signals
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)

	// Goroutine to handle graceful shutdown
	go func() {
		sig := <-quit
		log.Infof("Received signal %s. Initiating graceful shutdown...", sig)

		ctxShutdown, cancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer cancel()

		if err := server.Shutdown(ctxShutdown); err != nil {
			log.Fatalf("Server Shutdown Failed:%+v", err)
		}
		log.Info("Server gracefully stopped")
	}()

	// Start the server
	log.Infof("Starting server on %s", server.Addr)
	if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		log.Fatalf("Server failed: %v", err)
	}

	<-ctx.Done()
}
