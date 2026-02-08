// Package handlers provides HTTP handler utilities and common response patterns.
package handlers

import (
	"encoding/json"
	"net/http"

	"github.com/sirupsen/logrus"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// WriteJSONResponse writes a JSON response with the given status code.
func WriteJSONResponse(w http.ResponseWriter, statusCode int, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(statusCode)
	if err := json.NewEncoder(w).Encode(data); err != nil {
		log.Errorf("Failed to encode JSON response: %v", err)
	}
}

// WriteJSONError writes a JSON error response.
func WriteJSONError(w http.ResponseWriter, statusCode int, message string) {
	WriteJSONResponse(w, statusCode, map[string]string{"error": message})
}

// HealthHandler returns the health check handler.
func HealthHandler() http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte("OK"))
	}
}

// CORSWrapper wraps a handler with CORS headers.
func CORSWrapper(allowedOrigins string, handler http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		origin := allowedOrigins
		if origin == "" {
			origin = "*"
		}

		w.Header().Set("Access-Control-Allow-Origin", origin)
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS, HEAD")
		w.Header().Set("Access-Control-Allow-Headers", "Authorization, Content-Type, Content-Length, X-Requested-With, X-Upload-ID, X-Session-Token, X-File-Name, X-File-Size, Range, Content-Range, X-Session-ID, X-Upload-Session-ID, X-Challenge-Response")
		w.Header().Set("Access-Control-Expose-Headers", "Content-Length, Content-Range, X-Upload-Status, X-Session-ID, Location, ETag, X-Network-Resilience, X-Upload-Context-ID")
		w.Header().Set("Access-Control-Max-Age", "86400")
		w.Header().Set("Access-Control-Allow-Credentials", "false")

		if r.Method == http.MethodOptions {
			w.WriteHeader(http.StatusOK)
			return
		}

		handler(w, r)
	}
}
