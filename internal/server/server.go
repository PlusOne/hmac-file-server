package server

import (
	"context"
	"net"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/sirupsen/logrus"
)

func StartServer(server *http.Server, unixSocket bool, listenPort string, log *logrus.Logger, cancel context.CancelFunc, uploadQueue chan interface{}, scanQueue chan interface{}, networkEvents chan interface{}) {
	setupGracefulShutdown(server, cancel, uploadQueue, scanQueue, networkEvents, log)
	log.Infof("Starting HMAC file server ...")

	if unixSocket {
		if err := os.RemoveAll(listenPort); err != nil {
			log.Fatalf("Failed to remove existing Unix socket: %v", err)
		}
		listener, err := net.Listen("unix", listenPort)
		if err != nil {
			log.Fatalf("Failed to listen on Unix socket %s: %v", listenPort, err)
		}
		defer listener.Close()
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	} else {
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	}
}

func setupGracefulShutdown(server *http.Server, cancel context.CancelFunc, uploadQueue, scanQueue, networkEvents chan interface{}, log *logrus.Logger) {
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-quit
		log.Infof("Received signal %s. Initiating shutdown...", sig)

		ctxShutdown, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()

		if err := server.Shutdown(ctxShutdown); err != nil {
			log.Errorf("Server shutdown failed: %v", err)
		} else {
			log.Info("Server shutdown gracefully.")
		}

		cancel()

		close(uploadQueue)
		log.Info("Upload queue closed.")
		close(scanQueue)
		log.Info("Scan queue closed.")
		close(networkEvents)
		log.Info("Network events channel closed.")

		log.Info("Shutdown process completed. Exiting application.")
		os.Exit(0)
	}()
}
