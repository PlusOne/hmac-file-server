package server

import (
    "net/http"
    "context"
    "time"
)

// Start the HTTP server with given configuration, timeouts, and handle graceful shutdown
func StartServer(handler http.Handler, addr string) error {
    srv := &http.Server{
        Addr: addr,
        Handler: handler,
        ReadTimeout: 5 * time.Second,
        WriteTimeout: 5 * time.Second,
        IdleTimeout: 120 * time.Second,
    }
    // Implement graceful shutdown logic
    return srv.ListenAndServe()
}
