package routes

import (
	"net/http"

	"github.com/prometheus/client_golang/prometheus/promhttp"
)

func SetupRouter(metricsEnabled bool, handler http.Handler) http.Handler {
	mux := http.NewServeMux()
	mux.Handle("/", handler)
	if metricsEnabled {
		mux.Handle("/metrics", promhttp.Handler())
	}
	return mux
}
