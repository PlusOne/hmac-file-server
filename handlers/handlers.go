package handlers

import (
	"net/http"

	"github.com/renz/hmac-file-server/config"
	"github.com/renz/hmac-file-server/utils"
	"github.com/sirupsen/logrus"
)

func SetupRouter(conf *config.Config) http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", handleRequest(conf))
	if conf.Server.MetricsEnabled {
		mux.Handle("/metrics", utils.PrometheusHandler())
	}
	handler := LoggingMiddleware(mux)
	handler = RecoveryMiddleware(handler)
	handler = CORSMiddleware(handler)
	return handler
}

func handleRequest(conf *config.Config) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method == http.MethodPost && utils.IsMultipart(r.Header.Get("Content-Type")) {
			handleUpload(w, r, conf)
			return
		}

		// ...existing request handling code...

		logrus.WithFields(logrus.Fields{
			"method":  r.Method,
			"url":     r.URL.String(),
			"remote":  utils.GetClientIP(r),
		}).Info("Incoming request")

		// ...additional routing logic...
	}
}

func handleUpload(w http.ResponseWriter, r *http.Request, conf *config.Config) {
	// ...implementation...
}

func handleDownload(w http.ResponseWriter, r *http.Request, conf *config.Config) {
	// ...implementation...
}

// ...other handler functions...