// Package server provides HTTP server setup and lifecycle management.
package server

import (
	"context"
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/sirupsen/logrus"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// New creates a configured HTTP server.
func New(addr string, handler http.Handler, readTimeout, writeTimeout, idleTimeout time.Duration) *http.Server {
	return &http.Server{
		Addr:           addr,
		Handler:        handler,
		ReadTimeout:    readTimeout,
		WriteTimeout:   writeTimeout,
		IdleTimeout:    idleTimeout,
		MaxHeaderBytes: 1 << 20,
	}
}

// Start starts the HTTP server and blocks until shutdown.
func Start(server *http.Server) error {
	log.Infof("Starting HMAC File Server on %s", server.Addr)
	return server.ListenAndServe()
}

// SetupGracefulShutdown configures graceful shutdown on SIGINT/SIGTERM.
func SetupGracefulShutdown(srv *http.Server, cancel context.CancelFunc, cleanupFn func()) {
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		sig := <-sigChan
		log.Infof("Received signal %v, initiating graceful shutdown...", sig)

		cancel()

		ctx, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()

		if err := srv.Shutdown(ctx); err != nil {
			log.Errorf("Server shutdown error: %v", err)
		} else {
			log.Info("Server shutdown completed")
		}

		if cleanupFn != nil {
			cleanupFn()
		}

		os.Exit(0)
	}()
}

// PrintStartupBanner prints the server startup banner.
func PrintStartupBanner(version string, listenAddr string) {
	fmt.Println("╔══════════════════════════════════════════════╗")
	fmt.Println("║         HMAC File Server                    ║")
	fmt.Printf("║         Version: %-27s║\n", version)
	fmt.Printf("║         Listen:  %-27s║\n", listenAddr)
	fmt.Println("╚══════════════════════════════════════════════╝")
}
