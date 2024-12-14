package routes

import "net/http"

// Setup HTTP routes, middleware, and integrate handlers from fileops etc.
func SetupRouter() http.Handler {
    // return mux or router with all routes registered
    return http.NewServeMux()
}
