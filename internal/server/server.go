package server

import (
	"context"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/renz/source/hmac-file-server/internal/config"
	"github.com/sirupsen/logrus"
)

type Server struct {
	httpServer *http.Server
}

func NewServer(conf *config.Config) *Server {
	mux := SetupRouter()
	if conf.Server.MetricsEnabled {
		mux.Handle("/metrics", promhttp.Handler())
	}

	srv := &http.Server{
		Addr:         ":" + conf.Server.ListenPort,
		Handler:      mux,
		ReadTimeout:  parseDuration(conf.Timeouts.ReadTimeout),
		WriteTimeout: parseDuration(conf.Timeouts.WriteTimeout),
		IdleTimeout:  parseDuration(conf.Timeouts.IdleTimeout),
	}

	return &Server{httpServer: srv}
}

func parseDuration(durationStr string) time.Duration {
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		logrus.Fatalf("Invalid duration string: %v", err)
	}
	return duration
}

func (s *Server) Start(ctx context.Context, log *logrus.Logger) {
	// Channel to listen for interrupt or terminate signals
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)

	// Goroutine to handle graceful shutdown
	go func() {
		sig := <-quit
		log.Infof("Received signal %s. Initiating graceful shutdown...", sig)

		ctxShutdown, cancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer cancel()

		if err := s.httpServer.Shutdown(ctxShutdown); err != nil {
			log.Fatalf("Server Shutdown Failed:%+v", err)
		}
		log.Info("Server gracefully stopped")
	}()

	// Start the server
	log.Infof("Starting server on %s", s.httpServer.Addr)
	if err := s.httpServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		log.Fatalf("Server failed: %v", err)
	}

	<-ctx.Done()
}

// Start starts the HTTP server and handles graceful shutdown
func Start(ctx context.Context, srv *http.Server, log *logrus.Logger) {
	go func() {
		log.Infof("Server is listening on %s", srv.Addr)
		if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("ListenAndServe(): %v", err)
		}
	}()

	<-ctx.Done()

	log.Info("Shutting down server...")

	ctxShutDown, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if err := srv.Shutdown(ctxShutDown); err != nil {
		log.Fatalf("Server Shutdown Failed:%+v", err)
	}

	log.Info("Server exited properly")
}

// SetupRouter sets up the HTTP routes
func SetupRouter() *http.ServeMux {
	mux := http.NewServeMux()
	mux.HandleFunc("/upload", handleUpload)
	mux.HandleFunc("/download", handleDownload)
	return mux
}

// handleUpload handles file upload requests
func handleUpload(w http.ResponseWriter, r *http.Request) {
	// Your upload handling logic
	w.WriteHeader(http.StatusCreated)
	w.Write([]byte("Upload successful"))
}

// handleDownload handles file download requests
func handleDownload(w http.ResponseWriter, r *http.Request) {
	// Your download handling logic
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("Download successful"))
}
