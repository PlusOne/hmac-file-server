// rate_limiter.go — Per-IP and per-JID rate limiting for upload/download abuse prevention.

package main

import (
	"fmt"
	"net/http"
	"sync"
	"time"
)

// RateLimiterConfig holds rate limiting configuration.
type RateLimiterConfig struct {
	Enabled         bool     `toml:"enabled" mapstructure:"enabled"`
	RequestsPerMin  int      `toml:"requests_per_minute" mapstructure:"requests_per_minute"`
	BurstSize       int      `toml:"burst_size" mapstructure:"burst_size"`
	CleanupInterval string   `toml:"cleanup_interval" mapstructure:"cleanup_interval"`
	ByJID           bool     `toml:"by_jid" mapstructure:"by_jid"` // Rate limit per JID
	ByIP            bool     `toml:"by_ip" mapstructure:"by_ip"`   // Rate limit per IP
	WhitelistedIPs  []string `toml:"whitelisted_ips" mapstructure:"whitelisted_ips"`
	WhitelistedJIDs []string `toml:"whitelisted_jids" mapstructure:"whitelisted_jids"`
}

// tokenBucket implements a simple token bucket rate limiter per key.
type tokenBucket struct {
	tokens     float64
	maxTokens  float64
	refillRate float64 // tokens per second
	lastRefill time.Time
}

// RateLimiter manages per-key rate limiting using token buckets.
type RateLimiter struct {
	config       *RateLimiterConfig
	buckets      sync.Map // map[string]*tokenBucket
	whitelist    map[string]bool
	jidWhitelist map[string]bool
}

var (
	rateLimiter     *RateLimiter
	rateLimiterOnce sync.Once
)

// InitRateLimiter initializes the global rate limiter.
func InitRateLimiter(cfg *RateLimiterConfig) {
	rateLimiterOnce.Do(func() {
		if cfg == nil || !cfg.Enabled {
			log.Info("Rate limiter is disabled")
			return
		}

		if cfg.RequestsPerMin <= 0 {
			cfg.RequestsPerMin = 60 // default: 60 req/min
		}
		if cfg.BurstSize <= 0 {
			cfg.BurstSize = cfg.RequestsPerMin / 2
			if cfg.BurstSize < 5 {
				cfg.BurstSize = 5
			}
		}

		wl := make(map[string]bool, len(cfg.WhitelistedIPs))
		for _, ip := range cfg.WhitelistedIPs {
			wl[ip] = true
		}
		// Always whitelist localhost
		wl["127.0.0.1"] = true
		wl["::1"] = true

		jidWl := make(map[string]bool, len(cfg.WhitelistedJIDs))
		for _, jid := range cfg.WhitelistedJIDs {
			jidWl[jid] = true
		}

		rateLimiter = &RateLimiter{
			config:       cfg,
			whitelist:    wl,
			jidWhitelist: jidWl,
		}

		// Start cleanup goroutine to evict stale buckets
		cleanupInterval := 5 * time.Minute
		if cfg.CleanupInterval != "" {
			if d, err := parseTTL(cfg.CleanupInterval); err == nil {
				cleanupInterval = d
			}
		}
		go rateLimiter.cleanupLoop(cleanupInterval)

		log.Infof("Rate limiter initialized: %d req/min, burst=%d, by_ip=%v, by_jid=%v",
			cfg.RequestsPerMin, cfg.BurstSize, cfg.ByIP, cfg.ByJID)
	})
}

// GetRateLimiter returns the global rate limiter (may be nil if disabled).
func GetRateLimiter() *RateLimiter {
	return rateLimiter
}

// Allow checks if a request should be allowed based on rate limits.
// Returns true if the request is allowed, false if rate limited.
func (rl *RateLimiter) Allow(r *http.Request) bool {
	if rl == nil || !rl.config.Enabled {
		return true
	}

	clientIP := getClientIP(r)

	// Check IP whitelist
	if rl.whitelist[clientIP] {
		return true
	}

	// Check JID whitelist
	jid := extractJIDFromRequest(r)
	if jid != "" && rl.jidWhitelist[jid] {
		return true
	}

	// Rate limit by IP
	if rl.config.ByIP {
		if !rl.allowKey("ip:" + clientIP) {
			return false
		}
	}

	// Rate limit by JID
	if rl.config.ByJID && jid != "" {
		if !rl.allowKey("jid:" + jid) {
			return false
		}
	}

	return true
}

// allowKey checks the token bucket for a given key.
func (rl *RateLimiter) allowKey(key string) bool {
	now := time.Now()

	val, loaded := rl.buckets.Load(key)
	if !loaded {
		// Create new bucket
		bucket := &tokenBucket{
			tokens:     float64(rl.config.BurstSize),
			maxTokens:  float64(rl.config.BurstSize),
			refillRate: float64(rl.config.RequestsPerMin) / 60.0, // tokens per second
			lastRefill: now,
		}
		val, _ = rl.buckets.LoadOrStore(key, bucket)
	}

	bucket := val.(*tokenBucket)

	// Refill tokens based on elapsed time
	elapsed := now.Sub(bucket.lastRefill).Seconds()
	bucket.tokens += elapsed * bucket.refillRate
	if bucket.tokens > bucket.maxTokens {
		bucket.tokens = bucket.maxTokens
	}
	bucket.lastRefill = now

	// Try to consume a token
	if bucket.tokens >= 1.0 {
		bucket.tokens -= 1.0
		return true
	}

	return false
}

// extractJIDFromRequest attempts to extract a JID from the request.
func extractJIDFromRequest(r *http.Request) string {
	// Check X-User-JID header (set during auth)
	if jid := r.Header.Get("X-User-JID"); jid != "" {
		return jid
	}
	// Check X-Authenticated-User header
	if jid := r.Header.Get("X-Authenticated-User"); jid != "" {
		return jid
	}
	// Check query parameter
	if user := r.URL.Query().Get("user"); user != "" {
		return user
	}
	return ""
}

// cleanupLoop periodically removes stale token buckets.
func (rl *RateLimiter) cleanupLoop(interval time.Duration) {
	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for range ticker.C {
		staleThreshold := time.Now().Add(-10 * time.Minute)
		var deleted int

		rl.buckets.Range(func(key, val interface{}) bool {
			bucket := val.(*tokenBucket)
			if bucket.lastRefill.Before(staleThreshold) {
				rl.buckets.Delete(key)
				deleted++
			}
			return true
		})

		if deleted > 0 {
			log.Debugf("Rate limiter: cleaned up %d stale buckets", deleted)
		}
	}
}

// RateLimitMiddleware returns an HTTP middleware that enforces rate limits.
func RateLimitMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		rl := GetRateLimiter()
		if rl == nil || !rl.config.Enabled {
			next.ServeHTTP(w, r)
			return
		}

		// Only rate-limit upload methods (PUT, POST)
		if r.Method != http.MethodPut && r.Method != http.MethodPost {
			next.ServeHTTP(w, r)
			return
		}

		if !rl.Allow(r) {
			clientIP := getClientIP(r)
			jid := extractJIDFromRequest(r)

			log.Warnf("⛔ Rate limit exceeded: ip=%s, jid=%s, path=%s", clientIP, jid, r.URL.Path)
			AuditRateLimited(r, jid, "upload_rate_exceeded")

			w.Header().Set("Retry-After", "60")
			w.Header().Set("X-RateLimit-Limit", formatIntHeader(rl.config.RequestsPerMin))
			w.Header().Set("X-RateLimit-Reset", formatIntHeader(int(time.Now().Add(60*time.Second).Unix())))
			http.Error(w, "Rate limit exceeded. Please try again later.", http.StatusTooManyRequests)
			return
		}

		next.ServeHTTP(w, r)
	})
}

// formatIntHeader formats an int as a string for HTTP headers.
func formatIntHeader(v int) string {
	return fmt.Sprintf("%d", v)
}

// DefaultRateLimiterConfig returns sensible defaults for rate limiting.
func DefaultRateLimiterConfig() RateLimiterConfig {
	return RateLimiterConfig{
		Enabled:        false,
		RequestsPerMin: 60,
		BurstSize:      10,
		ByIP:           true,
		ByJID:          true,
	}
}
