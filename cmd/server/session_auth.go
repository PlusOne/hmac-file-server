// session_auth.go — Session management, authentication, and HMAC validation functions.
// CRITICAL: Auth/HMAC logic is copied EXACTLY from main.go — DO NOT modify.
package main

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/go-redis/redis/v8"
	jwt "github.com/golang-jwt/jwt/v5"
	"github.com/patrickmn/go-cache"
	"github.com/spf13/viper"
)

// NetworkResilientSession represents a persistent session for network switching
type NetworkResilientSession struct {
	SessionID          string         `json:"session_id"`
	UserJID            string         `json:"user_jid"`
	OriginalToken      string         `json:"original_token"`
	CreatedAt          time.Time      `json:"created_at"`
	LastSeen           time.Time      `json:"last_seen"`
	NetworkHistory     []NetworkEvent `json:"network_history"`
	UploadContext      *UploadContext `json:"upload_context,omitempty"`
	RefreshCount       int            `json:"refresh_count"`
	MaxRefreshes       int            `json:"max_refreshes"`
	LastIP             string         `json:"last_ip"`
	UserAgent          string         `json:"user_agent"`
	SecurityLevel      int            `json:"security_level"` // 1=normal, 2=challenge, 3=reauth
	LastSecurityCheck  time.Time      `json:"last_security_check"`
	NetworkChangeCount int            `json:"network_change_count"`
	StandbyDetected    bool           `json:"standby_detected"`
	LastActivity       time.Time      `json:"last_activity"`
}

// contextKey is a custom type for context keys to avoid collisions
type contextKey string

// Context keys
const (
	responseWriterKey contextKey = "responseWriter"
)

// BearerTokenClaims represents the claims extracted from a Bearer token
type BearerTokenClaims struct {
	User     string
	Filename string
	Size     int64
	Expiry   int64
}

// SessionStore manages persistent sessions for network resilience
type SessionStore struct {
	storage       map[string]*NetworkResilientSession
	mutex         sync.RWMutex
	cleanupTicker *time.Ticker
	redisClient   *redis.Client
	memoryCache   *cache.Cache
	enabled       bool
}

// Global session store
var sessionStore *SessionStore

// Session storage methods
func (s *SessionStore) GetSession(sessionID string) *NetworkResilientSession {
	if !s.enabled || sessionID == "" {
		return nil
	}

	s.mutex.RLock()
	defer s.mutex.RUnlock()

	// Try Redis first if available
	if s.redisClient != nil {
		ctx := context.Background()
		sessionData, err := s.redisClient.Get(ctx, "session:"+sessionID).Result()
		if err == nil {
			var session NetworkResilientSession
			if json.Unmarshal([]byte(sessionData), &session) == nil {
				log.Debugf("📊 Session retrieved from Redis: %s", sessionID)
				return &session
			}
		}
	}

	// Fallback to memory cache
	if s.memoryCache != nil {
		if sessionData, found := s.memoryCache.Get(sessionID); found {
			if session, ok := sessionData.(*NetworkResilientSession); ok {
				log.Debugf("📊 Session retrieved from memory: %s", sessionID)
				return session
			}
		}
	}

	// Fallback to in-memory map
	if session, exists := s.storage[sessionID]; exists {
		if time.Since(session.LastSeen) < 72*time.Hour {
			log.Debugf("📊 Session retrieved from storage: %s", sessionID)
			return session
		}
	}

	return nil
}

func (s *SessionStore) StoreSession(sessionID string, session *NetworkResilientSession) {
	if !s.enabled || sessionID == "" || session == nil {
		return
	}

	s.mutex.Lock()
	defer s.mutex.Unlock()

	session.LastSeen = time.Now()

	// Store in Redis if available
	if s.redisClient != nil {
		ctx := context.Background()
		sessionData, err := json.Marshal(session)
		if err == nil {
			s.redisClient.Set(ctx, "session:"+sessionID, sessionData, 72*time.Hour)
			log.Debugf("📊 Session stored in Redis: %s", sessionID)
		}
	}

	// Store in memory cache
	if s.memoryCache != nil {
		s.memoryCache.Set(sessionID, session, 72*time.Hour)
		log.Debugf("📊 Session stored in memory: %s", sessionID)
	}

	// Store in local map as final fallback
	s.storage[sessionID] = session
	log.Debugf("📊 Session stored in local storage: %s", sessionID)
}

func (s *SessionStore) DeleteSession(sessionID string) {
	if !s.enabled || sessionID == "" {
		return
	}

	s.mutex.Lock()
	defer s.mutex.Unlock()

	// Remove from Redis
	if s.redisClient != nil {
		ctx := context.Background()
		s.redisClient.Del(ctx, "session:"+sessionID)
	}

	// Remove from memory cache
	if s.memoryCache != nil {
		s.memoryCache.Delete(sessionID)
	}

	// Remove from local storage
	delete(s.storage, sessionID)
	log.Debugf("📊 Session deleted: %s", sessionID)
}

func (s *SessionStore) cleanupRoutine() {
	if !s.enabled {
		return
	}

	for range s.cleanupTicker.C {
		s.mutex.Lock()
		for sessionID, session := range s.storage {
			if time.Since(session.LastSeen) > 72*time.Hour {
				delete(s.storage, sessionID)
				log.Debugf("🧹 Cleaned up expired session: %s", sessionID)
			}
		}
		s.mutex.Unlock()
	}
}

// Initialize session store
func initializeSessionStore() {
	enabled := viper.GetBool("session_store.enabled")
	if !enabled {
		log.Infof("📊 Session store disabled in configuration")
		sessionStore = &SessionStore{enabled: false}
		return
	}

	sessionStore = &SessionStore{
		storage:       make(map[string]*NetworkResilientSession),
		cleanupTicker: time.NewTicker(30 * time.Minute),
		enabled:       true,
	}

	// Initialize memory cache
	sessionStore.memoryCache = cache.New(72*time.Hour, 1*time.Hour)

	// Optional Redis backend
	if redisURL := viper.GetString("session_store.redis_url"); redisURL != "" {
		opt, err := redis.ParseURL(redisURL)
		if err == nil {
			sessionStore.redisClient = redis.NewClient(opt)

			// Test Redis connection
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()

			if err := sessionStore.redisClient.Ping(ctx).Err(); err == nil {
				log.Infof("📊 Session store: Redis backend initialized (%s)", redisURL)
			} else {
				log.Warnf("📊 Session store: Redis connection failed, using memory backend: %v", err)
				sessionStore.redisClient = nil
			}
		} else {
			log.Warnf("📊 Session store: Invalid Redis URL, using memory backend: %v", err)
		}
	}

	if sessionStore.redisClient == nil {
		log.Infof("📊 Session store: Memory backend initialized")
	}

	// Start cleanup routine
	go sessionStore.cleanupRoutine()
}

// Detect network context for intelligent switching
// nolint:unused
func detectNetworkContext(r *http.Request) string {
	clientIP := getClientIP(r)
	userAgent := r.Header.Get("User-Agent")
	xForwardedFor := r.Header.Get("X-Forwarded-For")

	// Detect network type based on IP ranges and headers
	if strings.Contains(xForwardedFor, "10.") || strings.Contains(clientIP, "10.") {
		return "cellular_lte"
	} else if strings.Contains(clientIP, "192.168.") || strings.Contains(clientIP, "172.") {
		return "wifi_private"
	} else if strings.Contains(userAgent, "Mobile") || strings.Contains(userAgent, "Android") {
		return "mobile_network"
	} else if strings.Contains(clientIP, "127.0.0.1") || strings.Contains(clientIP, "::1") {
		return "localhost"
	}

	return "external_network"
}

// Add session response headers for client tracking
func setSessionHeaders(w http.ResponseWriter, sessionID string) {
	w.Header().Set("X-Session-ID", sessionID)
	w.Header().Set("X-Session-Timeout", "259200") // 72 hours in seconds
	w.Header().Set("X-Network-Resilience", "enabled")
}

// Extract session ID from request
func getSessionIDFromRequest(r *http.Request) string {
	// Try header first
	if sessionID := r.Header.Get("X-Session-ID"); sessionID != "" {
		return sessionID
	}

	// Try query parameter
	if sessionID := r.URL.Query().Get("session_id"); sessionID != "" {
		return sessionID
	}

	// Try from Authorization header (for some XMPP clients)
	if auth := r.Header.Get("Authorization"); strings.HasPrefix(auth, "Bearer ") {
		token := strings.TrimPrefix(auth, "Bearer ")
		// Generate consistent session ID from token
		h := sha256.New()
		h.Write([]byte(token))
		return fmt.Sprintf("auth_%s", hex.EncodeToString(h.Sum(nil))[:16])
	}

	return ""
}

// validateJWTFromRequest extracts and validates a JWT from the request.
func validateJWTFromRequest(r *http.Request, secret string) (*jwt.Token, error) {
	authHeader := r.Header.Get("Authorization")
	tokenString := ""

	if authHeader != "" {
		splitToken := strings.Split(authHeader, "Bearer ")
		if len(splitToken) == 2 {
			tokenString = splitToken[1]
		} else {
			return nil, errors.New("invalid Authorization header format")
		}
	} else {
		// Fallback to checking 'token' query parameter
		tokenString = r.URL.Query().Get("token")
		if tokenString == "" {
			return nil, errors.New("missing JWT in Authorization header or 'token' query parameter")
		}
	}

	token, err := jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
		if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("unexpected signing method: %v", token.Header["alg"])
		}
		return []byte(secret), nil
	})

	if err != nil {
		return nil, fmt.Errorf("JWT validation failed: %w", err)
	}

	if !token.Valid {
		return nil, errors.New("invalid JWT")
	}

	return token, nil
}

// validateBearerToken validates Bearer token authentication from ejabberd module
// ENHANCED FOR 100% WIFI ↔ LTE SWITCHING AND STANDBY RECOVERY RELIABILITY
func validateBearerToken(r *http.Request, secret string) (*BearerTokenClaims, error) {
	authHeader := r.Header.Get("Authorization")
	if authHeader == "" {
		return nil, errors.New("missing Authorization header")
	}

	// Check for Bearer token format
	if !strings.HasPrefix(authHeader, "Bearer ") {
		return nil, errors.New("invalid Authorization header format")
	}

	token := strings.TrimPrefix(authHeader, "Bearer ")
	if token == "" {
		return nil, errors.New("empty Bearer token")
	}

	// Decode base64 token
	tokenBytes, err := base64.StdEncoding.DecodeString(token)
	if err != nil {
		return nil, fmt.Errorf("invalid base64 token: %v", err)
	}

	// Extract claims from URL parameters
	query := r.URL.Query()
	user := query.Get("user")
	expiryStr := query.Get("expiry")

	if user == "" {
		return nil, errors.New("missing user parameter")
	}

	if expiryStr == "" {
		return nil, errors.New("missing expiry parameter")
	}

	expiry, err := strconv.ParseInt(expiryStr, 10, 64)
	if err != nil {
		return nil, fmt.Errorf("invalid expiry parameter: %v", err)
	}

	// ULTRA-FLEXIBLE GRACE PERIODS FOR NETWORK SWITCHING AND STANDBY SCENARIOS
	now := time.Now().Unix()

	// Base grace period: 8 hours (increased from 4 hours for better WiFi ↔ LTE reliability)
	gracePeriod := int64(28800) // 8 hours base grace period for all scenarios

	// Detect mobile XMPP clients and apply enhanced grace periods
	userAgent := r.Header.Get("User-Agent")
	isMobileXMPP := strings.Contains(strings.ToLower(userAgent), "conversations") ||
		strings.Contains(strings.ToLower(userAgent), "dino") ||
		strings.Contains(strings.ToLower(userAgent), "gajim") ||
		strings.Contains(strings.ToLower(userAgent), "android") ||
		strings.Contains(strings.ToLower(userAgent), "mobile") ||
		strings.Contains(strings.ToLower(userAgent), "xmpp") ||
		strings.Contains(strings.ToLower(userAgent), "client") ||
		strings.Contains(strings.ToLower(userAgent), "bot")

	// Enhanced XMPP client detection and grace period management
	// Desktop XMPP clients (Dino, Gajim) need extended grace for session restoration after restart
	isDesktopXMPP := strings.Contains(strings.ToLower(userAgent), "dino") ||
		strings.Contains(strings.ToLower(userAgent), "gajim")

	if isMobileXMPP || isDesktopXMPP {
		if isDesktopXMPP {
			gracePeriod = int64(86400) // 24 hours for desktop XMPP clients (session restoration)
			log.Infof("🖥️  Desktop XMPP client detected (%s), using 24-hour grace period for session restoration", userAgent)
		} else {
			gracePeriod = int64(43200) // 12 hours for mobile XMPP clients
			log.Infof("📱 Mobile XMPP client detected (%s), using extended 12-hour grace period", userAgent)
		}
	}

	// Network resilience parameters for session recovery
	sessionId := query.Get("session_id")
	networkResilience := query.Get("network_resilience")
	resumeAllowed := query.Get("resume_allowed")

	// Maximum grace period for network resilience scenarios
	if sessionId != "" || networkResilience == "true" || resumeAllowed == "true" {
		gracePeriod = int64(86400) // 24 hours for explicit network resilience scenarios
		log.Infof("🌐 Network resilience mode activated (session_id: %s, network_resilience: %s), using 24-hour grace period",
			sessionId, networkResilience)
	}

	// Detect potential network switching scenarios
	clientIP := getClientIP(r)
	xForwardedFor := r.Header.Get("X-Forwarded-For")
	xRealIP := r.Header.Get("X-Real-IP")

	// Check for client IP change indicators (WiFi ↔ LTE switching detection)
	if xForwardedFor != "" || xRealIP != "" {
		// Client is behind proxy/NAT - likely mobile switching between networks
		gracePeriod = int64(86400) // 24 hours for proxy/NAT scenarios
		log.Infof("📱 Network switching detected (client IP: %s, X-Forwarded-For: %s, X-Real-IP: %s), using 24-hour grace period",
			clientIP, xForwardedFor, xRealIP)
	}

	// Check Content-Length to identify large uploads that need extra time
	contentLength := r.Header.Get("Content-Length")
	var size int64 = 0
	if contentLength != "" {
		size, _ = strconv.ParseInt(contentLength, 10, 64)
		// For large files (>10MB), add extra grace time for mobile uploads
		if size > 10*1024*1024 {
			additionalTime := (size / (10 * 1024 * 1024)) * 3600 // 1 hour per 10MB
			gracePeriod += additionalTime
			log.Infof("📁 Large file detected (%d bytes), extending grace period by %d seconds", size, additionalTime)
		}
	}

	// ABSOLUTE MAXIMUM: 48 hours for extreme scenarios
	maxAbsoluteGrace := int64(172800) // 48 hours absolute maximum
	if gracePeriod > maxAbsoluteGrace {
		gracePeriod = maxAbsoluteGrace
		log.Infof("⚠️  Grace period capped at 48 hours maximum")
	}

	// STANDBY RECOVERY: Special handling for device standby scenarios
	isLikelyStandbyRecovery := false
	standbyGraceExtension := int64(86400) // Additional 24 hours for standby recovery

	if now > expiry {
		expiredTime := now - expiry

		// If token expired more than grace period but less than standby window, allow standby recovery
		if expiredTime > gracePeriod && expiredTime < (gracePeriod+standbyGraceExtension) {
			isLikelyStandbyRecovery = true
			log.Infof("💤 STANDBY RECOVERY: Token expired %d seconds ago, within standby recovery window", expiredTime)
		}

		// Apply grace period check
		if expiredTime > gracePeriod && !isLikelyStandbyRecovery {
			// DESKTOP XMPP CLIENT SESSION RESTORATION: Special handling for Dino/Gajim restart scenarios
			isDesktopSessionRestore := false
			if isDesktopXMPP && expiredTime < int64(172800) { // 48 hours for desktop session restore
				isDesktopSessionRestore = true
				log.Infof("🖥️  DESKTOP SESSION RESTORE: %s token expired %d seconds ago, allowing within 48-hour desktop restoration window", userAgent, expiredTime)
			}

			// Still apply ultra-generous final check for mobile scenarios
			ultraMaxGrace := int64(259200) // 72 hours ultra-maximum for critical mobile scenarios
			if (isMobileXMPP && expiredTime < ultraMaxGrace) || isDesktopSessionRestore {
				if isMobileXMPP {
					log.Warnf("⚡ ULTRA-GRACE: Mobile XMPP client token expired %d seconds ago, allowing within 72-hour ultra-grace window", expiredTime)
				}
			} else {
				log.Warnf("❌ Bearer token expired beyond all grace periods: now=%d, expiry=%d, expired_for=%d seconds, grace_period=%d, user_agent=%s",
					now, expiry, expiredTime, gracePeriod, userAgent)
				return nil, fmt.Errorf("token has expired beyond grace period (expired %d seconds ago, grace period: %d seconds)",
					expiredTime, gracePeriod)
			}
		} else if isLikelyStandbyRecovery {
			log.Infof("✅ STANDBY RECOVERY successful: allowing token within extended standby window")
		} else {
			log.Infof("✅ Bearer token expired but within grace period: %d seconds remaining", gracePeriod-expiredTime)
		}
	} else {
		log.Debugf("✅ Bearer token still valid: %d seconds until expiry", expiry-now)
	}

	// Extract filename and size from request with enhanced path parsing
	pathParts := strings.Split(strings.Trim(r.URL.Path, "/"), "/")
	if len(pathParts) < 1 {
		return nil, errors.New("invalid upload path format")
	}

	// Handle different path formats from various ejabberd modules
	filename := ""
	if len(pathParts) >= 3 {
		filename = pathParts[len(pathParts)-1] // Standard format: /upload/uuid/filename
	} else if len(pathParts) >= 1 {
		filename = pathParts[len(pathParts)-1] // Simplified format: /filename
	}

	if filename == "" {
		filename = "upload" // Fallback filename
	}

	// ENHANCED HMAC VALIDATION: Try multiple payload formats for maximum compatibility
	var validPayload bool
	var payloadFormat string

	// Format 1: Network-resilient payload (mod_http_upload_hmac_network_resilient)
	extendedPayload := fmt.Sprintf("%s\x00%s\x00%d\x00%d\x00%d\x00network_resilient",
		user, filename, size, expiry-86400, expiry)
	h1 := hmac.New(sha256.New, []byte(secret))
	h1.Write([]byte(extendedPayload))
	expectedMAC1 := h1.Sum(nil)

	if hmac.Equal(tokenBytes, expectedMAC1) {
		validPayload = true
		payloadFormat = "network_resilient"
	}

	// Format 2: Extended payload with session support
	if !validPayload {
		sessionPayload := fmt.Sprintf("%s\x00%s\x00%d\x00%d\x00%s", user, filename, size, expiry, sessionId)
		h2 := hmac.New(sha256.New, []byte(secret))
		h2.Write([]byte(sessionPayload))
		expectedMAC2 := h2.Sum(nil)

		if hmac.Equal(tokenBytes, expectedMAC2) {
			validPayload = true
			payloadFormat = "session_based"
		}
	}

	// Format 3: Standard payload (original mod_http_upload_hmac)
	if !validPayload {
		standardPayload := fmt.Sprintf("%s\x00%s\x00%d\x00%d", user, filename, size, expiry-3600)
		h3 := hmac.New(sha256.New, []byte(secret))
		h3.Write([]byte(standardPayload))
		expectedMAC3 := h3.Sum(nil)

		if hmac.Equal(tokenBytes, expectedMAC3) {
			validPayload = true
			payloadFormat = "standard"
		}
	}

	// Format 4: Simplified payload (fallback compatibility)
	if !validPayload {
		simplePayload := fmt.Sprintf("%s\x00%s\x00%d", user, filename, size)
		h4 := hmac.New(sha256.New, []byte(secret))
		h4.Write([]byte(simplePayload))
		expectedMAC4 := h4.Sum(nil)

		if hmac.Equal(tokenBytes, expectedMAC4) {
			validPayload = true
			payloadFormat = "simple"
		}
	}

	// Format 5: User-only payload (maximum fallback)
	if !validPayload {
		userPayload := fmt.Sprintf("%s\x00%d", user, expiry)
		h5 := hmac.New(sha256.New, []byte(secret))
		h5.Write([]byte(userPayload))
		expectedMAC5 := h5.Sum(nil)

		if hmac.Equal(tokenBytes, expectedMAC5) {
			validPayload = true
			payloadFormat = "user_only"
		}
	}

	if !validPayload {
		log.Warnf("❌ Invalid Bearer token HMAC for user %s, file %s (tried all 5 payload formats)", user, filename)
		return nil, errors.New("invalid Bearer token HMAC")
	}

	claims := &BearerTokenClaims{
		User:     user,
		Filename: filename,
		Size:     size,
		Expiry:   expiry,
	}

	log.Infof("✅ Bearer token authentication SUCCESSFUL: user=%s, file=%s, format=%s, grace_period=%d seconds",
		user, filename, payloadFormat, gracePeriod)

	return claims, nil
}

// evaluateSecurityLevel determines the required security level based on network changes and standby detection
func evaluateSecurityLevel(session *NetworkResilientSession, currentIP string, userAgent string) int {
	now := time.Now()

	// Initialize if this is the first check
	if session.LastSecurityCheck.IsZero() {
		session.LastSecurityCheck = now
		session.LastActivity = now
		session.SecurityLevel = 1 // Normal level
		return 1
	}

	// Detect potential standby scenario
	timeSinceLastActivity := now.Sub(session.LastActivity)
	standbyThreshold := 30 * time.Minute

	if timeSinceLastActivity > standbyThreshold {
		session.StandbyDetected = true
		log.Infof("🔒 STANDBY DETECTED: %v since last activity for session %s", timeSinceLastActivity, session.SessionID)

		// Long standby requires full re-authentication
		if timeSinceLastActivity > 2*time.Hour {
			log.Warnf("🔐 SECURITY LEVEL 3: Long standby (%v) requires full re-authentication", timeSinceLastActivity)
			return 3
		}

		// Medium standby requires challenge-response
		log.Infof("🔐 SECURITY LEVEL 2: Medium standby (%v) requires challenge-response", timeSinceLastActivity)
		return 2
	}

	// Detect network changes
	if session.LastIP != "" && session.LastIP != currentIP {
		session.NetworkChangeCount++
		log.Infof("🌐 NETWORK CHANGE #%d: %s → %s for session %s",
			session.NetworkChangeCount, session.LastIP, currentIP, session.SessionID)

		// Multiple rapid network changes are suspicious
		if session.NetworkChangeCount > 3 {
			log.Warnf("🔐 SECURITY LEVEL 3: Multiple network changes (%d) requires full re-authentication",
				session.NetworkChangeCount)
			return 3
		}

		// Single network change requires challenge-response
		log.Infof("🔐 SECURITY LEVEL 2: Network change requires challenge-response")
		return 2
	}

	// Check for suspicious user agent changes
	if session.UserAgent != "" && session.UserAgent != userAgent {
		log.Warnf("🔐 SECURITY LEVEL 3: User agent change detected - potential device hijacking")
		return 3
	}

	// Normal operation
	return 1
}

// generateSecurityChallenge creates a challenge for Level 2 authentication
func generateSecurityChallenge(session *NetworkResilientSession, secret string) (string, error) {
	// Create a time-based challenge using session data
	timestamp := time.Now().Unix()
	challengeData := fmt.Sprintf("%s:%s:%d", session.SessionID, session.UserJID, timestamp)

	h := hmac.New(sha256.New, []byte(secret))
	h.Write([]byte(challengeData))
	challenge := hex.EncodeToString(h.Sum(nil))

	log.Infof("🔐 Generated security challenge for session %s", session.SessionID)
	return challenge, nil
}

// validateSecurityChallenge verifies Level 2 challenge-response
func validateSecurityChallenge(session *NetworkResilientSession, providedResponse string, secret string) bool {
	// This would validate against the expected response
	// For now, we'll implement a simple time-window validation
	timestamp := time.Now().Unix()

	// Allow 5-minute window for challenge responses
	for i := int64(0); i <= 300; i += 60 {
		testTimestamp := timestamp - i
		challengeData := fmt.Sprintf("%s:%s:%d", session.SessionID, session.UserJID, testTimestamp)

		h := hmac.New(sha256.New, []byte(secret))
		h.Write([]byte(challengeData))
		expectedResponse := hex.EncodeToString(h.Sum(nil))

		if expectedResponse == providedResponse {
			log.Infof("✅ Security challenge validated for session %s", session.SessionID)
			return true
		}
	}

	log.Warnf("❌ Security challenge failed for session %s", session.SessionID)
	return false
}

// setSecurityHeaders adds appropriate headers for re-authentication requests
func setSecurityHeaders(w http.ResponseWriter, securityLevel int, challenge string) {
	switch securityLevel {
	case 2:
		// Challenge-response required
		w.Header().Set("WWW-Authenticate", fmt.Sprintf("HMAC-Challenge challenge=\"%s\"", challenge))
		w.Header().Set("X-Security-Level", "2")
		w.Header().Set("X-Auth-Required", "challenge-response")
	case 3:
		// Full re-authentication required
		w.Header().Set("WWW-Authenticate", "HMAC realm=\"HMAC File Server\"")
		w.Header().Set("X-Security-Level", "3")
		w.Header().Set("X-Auth-Required", "full-authentication")
	default:
		// Normal level
		w.Header().Set("X-Security-Level", "1")
	}
}

// validateBearerTokenWithSession validates Bearer token with session recovery support
// ENHANCED FOR NETWORK SWITCHING: 5G ↔ WiFi transition support with session persistence
func validateBearerTokenWithSession(r *http.Request, secret string) (*BearerTokenClaims, error) {
	// Step 1: Try standard Bearer token validation first
	claims, err := validateBearerToken(r, secret)
	if err == nil {
		// Token is valid - create or update session for network resilience
		sessionID := getSessionIDFromRequest(r)
		if sessionID == "" {
			sessionID = generateSessionID(claims.User, claims.Filename)
		}

		// Get or create session
		session := sessionStore.GetSession(sessionID)
		if session == nil {
			session = &NetworkResilientSession{
				SessionID:          sessionID,
				UserJID:            claims.User,
				OriginalToken:      getBearerTokenFromRequest(r),
				CreatedAt:          time.Now(),
				MaxRefreshes:       10,
				NetworkHistory:     []NetworkEvent{},
				SecurityLevel:      1,
				LastSecurityCheck:  time.Now(),
				NetworkChangeCount: 0,
				StandbyDetected:    false,
				LastActivity:       time.Now(),
			}
		}

		// Update session with current network context
		currentIP := getClientIP(r)
		userAgent := r.Header.Get("User-Agent")

		// ENHANCED SECURITY: Evaluate security level based on network changes and standby
		requiredSecurityLevel := evaluateSecurityLevel(session, currentIP, userAgent)
		session.SecurityLevel = requiredSecurityLevel
		session.LastActivity = time.Now()

		// Handle security level requirements
		if requiredSecurityLevel > 1 {
			// Extract response writer from context for security headers
			w, ok := r.Context().Value("responseWriter").(http.ResponseWriter)
			if !ok {
				log.Errorf("❌ Could not extract response writer for security headers")
				return nil, fmt.Errorf("security evaluation failed")
			}

			switch requiredSecurityLevel {
			case 2:
				// Challenge-response required
				challenge, err := generateSecurityChallenge(session, secret)
				if err != nil {
					log.Errorf("❌ Failed to generate security challenge: %v", err)
					return nil, fmt.Errorf("security challenge generation failed")
				}

				// Check if client provided challenge response
				challengeResponse := r.Header.Get("X-Challenge-Response")
				if challengeResponse == "" {
					// No response provided, send challenge
					setSecurityHeaders(w, 2, challenge)
					return nil, fmt.Errorf("challenge-response required for network change")
				}

				// Validate challenge response
				if !validateSecurityChallenge(session, challengeResponse, secret) {
					setSecurityHeaders(w, 2, challenge)
					return nil, fmt.Errorf("invalid challenge response")
				}

				log.Infof("✅ Challenge-response validated for session %s", sessionID)

			case 3:
				// Full re-authentication required
				setSecurityHeaders(w, 3, "")
				log.Warnf("🔐 Full re-authentication required for session %s", sessionID)
				return nil, fmt.Errorf("full re-authentication required")
			}
		}

		if session.LastIP != "" && session.LastIP != currentIP {
			// Network change detected
			session.NetworkHistory = append(session.NetworkHistory, NetworkEvent{
				Timestamp:   time.Now(),
				FromNetwork: session.LastIP,
				ToNetwork:   currentIP,
				ClientIP:    currentIP,
				UserAgent:   userAgent,
				EventType:   "network_switch",
			})
			log.Infof("🌐 Network switch detected for session %s: %s → %s",
				sessionID, session.LastIP, currentIP)
		}

		session.LastIP = currentIP
		session.UserAgent = userAgent
		sessionStore.StoreSession(sessionID, session)

		// Set session headers in response
		if w, ok := r.Context().Value("responseWriter").(http.ResponseWriter); ok {
			setSessionHeaders(w, sessionID)
		}

		log.Infof("✅ Bearer token valid, session updated: %s (user: %s)", sessionID, claims.User)
		return claims, nil
	}

	// Step 2: Token validation failed - try session recovery
	sessionID := getSessionIDFromRequest(r)
	if sessionID != "" {
		session := sessionStore.GetSession(sessionID)
		if session != nil {
			// Check if session is still valid (within 72-hour window)
			sessionAge := time.Since(session.CreatedAt)
			if sessionAge < 72*time.Hour {
				log.Infof("🔄 Session recovery attempt for %s (age: %v)", sessionID, sessionAge)

				// Check if we can refresh the token
				if session.RefreshCount < session.MaxRefreshes {
					_, err := refreshSessionToken(session, secret, r)
					if err == nil {
						// Token refresh successful
						session.RefreshCount++
						session.LastSeen = time.Now()

						// Add refresh event to history
						session.NetworkHistory = append(session.NetworkHistory, NetworkEvent{
							Timestamp: time.Now(),
							ClientIP:  getClientIP(r),
							UserAgent: r.Header.Get("User-Agent"),
							EventType: "token_refresh",
						})

						sessionStore.StoreSession(sessionID, session)

						// Create claims from refreshed session
						refreshedClaims := &BearerTokenClaims{
							User:     session.UserJID,
							Filename: extractFilenameFromPath(r.URL.Path),
							Size:     extractSizeFromRequest(r),
							Expiry:   time.Now().Add(24 * time.Hour).Unix(),
						}

						log.Infof("✅ Session recovery successful: %s (refresh #%d)",
							sessionID, session.RefreshCount)
						return refreshedClaims, nil
					}
				} else {
					log.Warnf("❌ Session %s exceeded maximum refreshes (%d)",
						sessionID, session.MaxRefreshes)
				}
			} else {
				log.Warnf("❌ Session %s expired (age: %v, max: 72h)", sessionID, sessionAge)
			}
		} else {
			log.Warnf("❌ Session %s not found in store", sessionID)
		}
	}

	// Step 3: No valid token or session recovery possible
	log.Warnf("❌ Authentication failed: %v (no session recovery available)", err)
	return nil, fmt.Errorf("authentication failed: %v", err)
}

// refreshSessionToken generates a new token for an existing session
func refreshSessionToken(session *NetworkResilientSession, secret string, r *http.Request) (string, error) {
	if session.RefreshCount >= session.MaxRefreshes {
		return "", fmt.Errorf("maximum token refreshes exceeded")
	}

	// Generate new HMAC token with extended validity
	timestamp := time.Now().Unix()
	expiry := timestamp + 86400 // 24 hours
	filename := extractFilenameFromPath(r.URL.Path)
	size := extractSizeFromRequest(r)

	// Use session-based payload format for refresh
	payload := fmt.Sprintf("%s\x00%s\x00%d\x00%d\x00%s\x00session_refresh",
		session.UserJID,
		filename,
		size,
		expiry,
		session.SessionID)

	h := hmac.New(sha256.New, []byte(secret))
	h.Write([]byte(payload))
	newToken := base64.StdEncoding.EncodeToString(h.Sum(nil))

	log.Infof("🆕 Generated refresh token for session %s (refresh #%d)",
		session.SessionID, session.RefreshCount+1)

	return newToken, nil
}

// Helper functions for token and session management
func getBearerTokenFromRequest(r *http.Request) string {
	authHeader := r.Header.Get("Authorization")
	if strings.HasPrefix(authHeader, "Bearer ") {
		return strings.TrimPrefix(authHeader, "Bearer ")
	}
	return ""
}

func extractFilenameFromPath(path string) string {
	pathParts := strings.Split(strings.Trim(path, "/"), "/")
	if len(pathParts) >= 1 {
		return pathParts[len(pathParts)-1]
	}
	return "unknown"
}

func extractSizeFromRequest(r *http.Request) int64 {
	if sizeStr := r.Header.Get("Content-Length"); sizeStr != "" {
		if size, err := strconv.ParseInt(sizeStr, 10, 64); err == nil {
			return size
		}
	}
	if sizeStr := r.URL.Query().Get("size"); sizeStr != "" {
		if size, err := strconv.ParseInt(sizeStr, 10, 64); err == nil {
			return size
		}
	}
	return 0
}

// validateHMAC validates the HMAC signature of the request for legacy protocols and POST uploads.
// ENHANCED FOR 100% WIFI ↔ LTE SWITCHING AND STANDBY RECOVERY RELIABILITY
func validateHMAC(r *http.Request, secret string) error {
	log.Debugf("🔍 validateHMAC: Validating request to %s with query: %s", r.URL.Path, r.URL.RawQuery)

	// Check for X-Signature header (for POST uploads)
	signature := r.Header.Get("X-Signature")
	if signature != "" {
		// This is a POST upload with X-Signature header
		message := r.URL.Path
		h := hmac.New(sha256.New, []byte(secret))
		h.Write([]byte(message))
		expectedSignature := hex.EncodeToString(h.Sum(nil))

		if !hmac.Equal([]byte(signature), []byte(expectedSignature)) {
			log.Warnf("❌ Invalid HMAC signature in X-Signature header")
			return errors.New("invalid HMAC signature in X-Signature header")
		}
		log.Debugf("✅ X-Signature HMAC authentication successful")
		return nil
	}

	// Check for legacy URL-based HMAC protocols (v, v2, token)
	query := r.URL.Query()

	var protocolVersion string
	var providedMACHex string

	if query.Get("v2") != "" {
		protocolVersion = "v2"
		providedMACHex = query.Get("v2")
	} else if query.Get("token") != "" {
		protocolVersion = "token"
		providedMACHex = query.Get("token")
	} else if query.Get("v") != "" {
		protocolVersion = "v"
		providedMACHex = query.Get("v")
	} else {
		return errors.New("no HMAC signature found (missing X-Signature header or v/v2/token query parameter)")
	}

	// Extract file path from URL
	fileStorePath := strings.TrimPrefix(r.URL.Path, "/")

	// ENHANCED HMAC CALCULATION: Try multiple formats for maximum compatibility
	var validMAC bool
	var messageFormat string

	// Calculate HMAC based on protocol version with enhanced compatibility
	mac := hmac.New(sha256.New, []byte(secret))

	if protocolVersion == "v" {
		// Format 1: Legacy v protocol - fileStorePath + "\x20" + contentLength
		message1 := fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)
		mac.Reset()
		mac.Write([]byte(message1))
		calculatedMAC1 := mac.Sum(nil)
		calculatedMACHex1 := hex.EncodeToString(calculatedMAC1)

		// Decode provided MAC
		if providedMAC, err := hex.DecodeString(providedMACHex); err == nil {
			if hmac.Equal(calculatedMAC1, providedMAC) {
				validMAC = true
				messageFormat = "v_standard"
				log.Debugf("✅ Legacy v protocol HMAC validated: %s", calculatedMACHex1)
			}
		}

		// Format 2: Try without content length for compatibility
		if !validMAC {
			message2 := fileStorePath
			mac.Reset()
			mac.Write([]byte(message2))
			calculatedMAC2 := mac.Sum(nil)

			if providedMAC, err := hex.DecodeString(providedMACHex); err == nil {
				if hmac.Equal(calculatedMAC2, providedMAC) {
					validMAC = true
					messageFormat = "v_simple"
					log.Debugf("✅ Legacy v protocol HMAC validated (simple format)")
				}
			}
		}
	} else {
		// v2 and token protocols: Enhanced format compatibility
		contentType := GetContentType(fileStorePath)

		// Format 1: Standard format - fileStorePath + "\x00" + contentLength + "\x00" + contentType
		message1 := fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10) + "\x00" + contentType
		mac.Reset()
		mac.Write([]byte(message1))
		calculatedMAC1 := mac.Sum(nil)
		calculatedMACHex1 := hex.EncodeToString(calculatedMAC1)

		if providedMAC, err := hex.DecodeString(providedMACHex); err == nil {
			if hmac.Equal(calculatedMAC1, providedMAC) {
				validMAC = true
				messageFormat = protocolVersion + "_standard"
				log.Debugf("✅ %s protocol HMAC validated (standard): %s", protocolVersion, calculatedMACHex1)
			}
		}

		// Format 2: Without content type for compatibility
		if !validMAC {
			message2 := fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10)
			mac.Reset()
			mac.Write([]byte(message2))
			calculatedMAC2 := mac.Sum(nil)

			if providedMAC, err := hex.DecodeString(providedMACHex); err == nil {
				if hmac.Equal(calculatedMAC2, providedMAC) {
					validMAC = true
					messageFormat = protocolVersion + "_no_content_type"
					log.Debugf("✅ %s protocol HMAC validated (no content type)", protocolVersion)
				}
			}
		}

		// Format 3: Simple path only for maximum compatibility
		if !validMAC {
			message3 := fileStorePath
			mac.Reset()
			mac.Write([]byte(message3))
			calculatedMAC3 := mac.Sum(nil)

			if providedMAC, err := hex.DecodeString(providedMACHex); err == nil {
				if hmac.Equal(calculatedMAC3, providedMAC) {
					validMAC = true
					messageFormat = protocolVersion + "_simple"
					log.Debugf("✅ %s protocol HMAC validated (simple path)", protocolVersion)
				}
			}
		}
	}

	if !validMAC {
		log.Warnf("❌ Invalid MAC for %s protocol (tried all formats)", protocolVersion)
		return fmt.Errorf("invalid MAC for %s protocol", protocolVersion)
	}

	log.Infof("✅ %s HMAC authentication SUCCESSFUL: format=%s, path=%s",
		protocolVersion, messageFormat, r.URL.Path)
	return nil
}

// validateV3HMAC validates the HMAC signature for v3 protocol (mod_http_upload_external).
// ENHANCED FOR 100% WIFI ↔ LTE SWITCHING AND STANDBY RECOVERY RELIABILITY
func validateV3HMAC(r *http.Request, secret string) error {
	query := r.URL.Query()

	// Extract v3 signature and expires from query parameters
	signature := query.Get("v3")
	expiresStr := query.Get("expires")

	if signature == "" {
		return errors.New("missing v3 signature parameter")
	}

	if expiresStr == "" {
		return errors.New("missing expires parameter")
	}

	// Parse expires timestamp
	expires, err := strconv.ParseInt(expiresStr, 10, 64)
	if err != nil {
		return fmt.Errorf("invalid expires parameter: %v", err)
	}

	// ULTRA-FLEXIBLE GRACE PERIODS FOR V3 PROTOCOL NETWORK SWITCHING
	now := time.Now().Unix()

	if now > expires {
		// Base grace period: 8 hours (significantly increased for WiFi ↔ LTE reliability)
		gracePeriod := int64(28800) // 8 hours base grace period

		// Enhanced mobile XMPP client detection
		userAgent := r.Header.Get("User-Agent")
		isMobileXMPP := strings.Contains(strings.ToLower(userAgent), "gajim") ||
			strings.Contains(strings.ToLower(userAgent), "dino") ||
			strings.Contains(strings.ToLower(userAgent), "conversations") ||
			strings.Contains(strings.ToLower(userAgent), "android") ||
			strings.Contains(strings.ToLower(userAgent), "mobile") ||
			strings.Contains(strings.ToLower(userAgent), "xmpp") ||
			strings.Contains(strings.ToLower(userAgent), "client") ||
			strings.Contains(strings.ToLower(userAgent), "bot")

		if isMobileXMPP {
			gracePeriod = int64(43200) // 12 hours for mobile XMPP clients
			log.Infof("📱 V3: Mobile XMPP client detected (%s), using 12-hour grace period", userAgent)
		}

		// Network resilience parameters for V3 protocol
		sessionId := query.Get("session_id")
		networkResilience := query.Get("network_resilience")
		resumeAllowed := query.Get("resume_allowed")
		if sessionId != "" || networkResilience == "true" || resumeAllowed == "true" {
			gracePeriod = int64(86400) // 24 hours for network resilience scenarios
			log.Infof("🌐 V3: Network resilience mode detected, using 24-hour grace period")
		}

		// Detect network switching indicators
		clientIP := getClientIP(r)
		xForwardedFor := r.Header.Get("X-Forwarded-For")
		xRealIP := r.Header.Get("X-Real-IP")

		if xForwardedFor != "" || xRealIP != "" {
			// Client behind proxy/NAT - likely mobile network switching
			gracePeriod = int64(86400) // 24 hours for proxy/NAT scenarios
			log.Infof("🔄 V3: Network switching detected (IP: %s, X-Forwarded-For: %s), using 24-hour grace period",
				clientIP, xForwardedFor)
		}

		// Large file uploads get additional grace time
		if contentLengthStr := r.Header.Get("Content-Length"); contentLengthStr != "" {
			if contentLength, parseErr := strconv.ParseInt(contentLengthStr, 10, 64); parseErr == nil {
				// For files > 10MB, add additional grace time
				if contentLength > 10*1024*1024 {
					additionalTime := (contentLength / (10 * 1024 * 1024)) * 3600 // 1 hour per 10MB
					gracePeriod += additionalTime
					log.Infof("📁 V3: Large file (%d bytes), extending grace period by %d seconds",
						contentLength, additionalTime)
				}
			}
		}

		// Maximum grace period cap: 48 hours
		maxGracePeriod := int64(172800) // 48 hours absolute maximum
		if gracePeriod > maxGracePeriod {
			gracePeriod = maxGracePeriod
			log.Infof("⚠️  V3: Grace period capped at 48 hours maximum")
		}

		// STANDBY RECOVERY: Handle device standby scenarios
		expiredTime := now - expires
		standbyGraceExtension := int64(86400) // Additional 24 hours for standby
		isLikelyStandbyRecovery := expiredTime > gracePeriod && expiredTime < (gracePeriod+standbyGraceExtension)

		if expiredTime > gracePeriod && !isLikelyStandbyRecovery {
			// Ultra-generous final check for mobile scenarios
			ultraMaxGrace := int64(259200) // 72 hours ultra-maximum for critical scenarios
			if isMobileXMPP && expiredTime < ultraMaxGrace {
				log.Warnf("⚡ V3 ULTRA-GRACE: Mobile client token expired %d seconds ago, allowing within 72-hour window", expiredTime)
			} else {
				log.Warnf("❌ V3 signature expired beyond all grace periods: now=%d, expires=%d, expired_for=%d seconds, grace_period=%d, user_agent=%s",
					now, expires, expiredTime, gracePeriod, userAgent)
				return fmt.Errorf("signature has expired beyond grace period (expired %d seconds ago, grace period: %d seconds)",
					expiredTime, gracePeriod)
			}
		} else if isLikelyStandbyRecovery {
			log.Infof("💤 V3 STANDBY RECOVERY: Allowing signature within extended standby window (expired %d seconds ago)", expiredTime)
		} else {
			log.Infof("✅ V3 signature within grace period: %d seconds remaining", gracePeriod-expiredTime)
		}
	} else {
		log.Debugf("✅ V3 signature still valid: %d seconds until expiry", expires-now)
	}

	// ENHANCED MESSAGE CONSTRUCTION: Try multiple formats for compatibility
	var validSignature bool
	var messageFormat string

	// Format 1: Standard v3 format
	message1 := fmt.Sprintf("%s\n%s\n%s", r.Method, expiresStr, r.URL.Path)
	h1 := hmac.New(sha256.New, []byte(secret))
	h1.Write([]byte(message1))
	expectedSignature1 := hex.EncodeToString(h1.Sum(nil))

	if hmac.Equal([]byte(signature), []byte(expectedSignature1)) {
		validSignature = true
		messageFormat = "standard_v3"
	}

	// Format 2: Alternative format with query string
	if !validSignature {
		pathWithQuery := r.URL.Path
		if r.URL.RawQuery != "" {
			pathWithQuery += "?" + r.URL.RawQuery
		}
		message2 := fmt.Sprintf("%s\n%s\n%s", r.Method, expiresStr, pathWithQuery)
		h2 := hmac.New(sha256.New, []byte(secret))
		h2.Write([]byte(message2))
		expectedSignature2 := hex.EncodeToString(h2.Sum(nil))

		if hmac.Equal([]byte(signature), []byte(expectedSignature2)) {
			validSignature = true
			messageFormat = "with_query"
		}
	}

	// Format 3: Simplified format (fallback)
	if !validSignature {
		message3 := fmt.Sprintf("%s\n%s", r.Method, r.URL.Path)
		h3 := hmac.New(sha256.New, []byte(secret))
		h3.Write([]byte(message3))
		expectedSignature3 := hex.EncodeToString(h3.Sum(nil))

		if hmac.Equal([]byte(signature), []byte(expectedSignature3)) {
			validSignature = true
			messageFormat = "simplified"
		}
	}

	if !validSignature {
		log.Warnf("❌ Invalid V3 HMAC signature (tried all 3 formats)")
		return errors.New("invalid v3 HMAC signature")
	}

	log.Infof("✅ V3 HMAC authentication SUCCESSFUL: format=%s, method=%s, path=%s",
		messageFormat, r.Method, r.URL.Path)
	return nil
}

// copyWithProgressTracking copies data with progress tracking for large downloads
func copyWithProgressTracking(dst io.Writer, src io.Reader, buf []byte, totalSize int64, clientIP string) (int64, error) {
	var written int64
	lastLogTime := time.Now()

	for {
		n, err := src.Read(buf)
		if n > 0 {
			w, werr := dst.Write(buf[:n])
			written += int64(w)
			if werr != nil {
				return written, werr
			}

			// Log progress for large files every 10MB or 30 seconds
			if totalSize > 50*1024*1024 &&
				(written%10*1024*1024 == 0 || time.Since(lastLogTime) > 30*time.Second) {
				progress := float64(written) / float64(totalSize) * 100
				log.Infof("📥 Download progress: %.1f%% (%s/%s) for IP %s",
					progress, formatBytes(written), formatBytes(totalSize), clientIP)
				lastLogTime = time.Now()
			}
		}
		if err == io.EOF {
			break
		}
		if err != nil {
			return written, err
		}
	}

	return written, nil
}
