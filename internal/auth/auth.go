// Package auth contains all authentication and HMAC logic.
// WARNING: These functions are copied exactly from the original codebase.
// Do NOT modify any authentication or HMAC logic.
package auth

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"errors"
	"fmt"
	"net/http"
	"strconv"
	"strings"
	"time"

	jwt "github.com/golang-jwt/jwt/v5"
	"github.com/sirupsen/logrus"

	"git.uuxo.net/uuxo/hmac-file-server/internal/utils"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// BearerTokenClaims represents the claims extracted from a Bearer token.
type BearerTokenClaims struct {
	User     string
	Filename string
	Size     int64
	Expiry   int64
}

// SessionStore is an interface for session storage operations needed by auth.
type SessionStore interface {
	GetSession(sessionID string) *NetworkResilientSession
	StoreSession(sessionID string, session *NetworkResilientSession)
}

// NetworkResilientSession represents a persistent session for network switching.
type NetworkResilientSession struct {
	SessionID          string         `json:"session_id"`
	UserJID            string         `json:"user_jid"`
	OriginalToken      string         `json:"original_token"`
	CreatedAt          time.Time      `json:"created_at"`
	LastSeen           time.Time      `json:"last_seen"`
	NetworkHistory     []NetworkEvent `json:"network_history"`
	RefreshCount       int            `json:"refresh_count"`
	MaxRefreshes       int            `json:"max_refreshes"`
	LastIP             string         `json:"last_ip"`
	UserAgent          string         `json:"user_agent"`
	SecurityLevel      int            `json:"security_level"`
	LastSecurityCheck  time.Time      `json:"last_security_check"`
	NetworkChangeCount int            `json:"network_change_count"`
	StandbyDetected    bool           `json:"standby_detected"`
	LastActivity       time.Time      `json:"last_activity"`
}

// NetworkEvent tracks network transitions during session.
type NetworkEvent struct {
	Timestamp   time.Time `json:"timestamp"`
	FromNetwork string    `json:"from_network"`
	ToNetwork   string    `json:"to_network"`
	ClientIP    string    `json:"client_ip"`
	UserAgent   string    `json:"user_agent"`
	EventType   string    `json:"event_type"`
}

// ValidateJWTFromRequest extracts and validates a JWT from the request.
func ValidateJWTFromRequest(r *http.Request, secret string) (*jwt.Token, error) {
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

// ValidateBearerToken validates Bearer token authentication from ejabberd module.
// ENHANCED FOR 100% WIFI ↔ LTE SWITCHING AND STANDBY RECOVERY RELIABILITY
func ValidateBearerToken(r *http.Request, secret string) (*BearerTokenClaims, error) {
	authHeader := r.Header.Get("Authorization")
	if authHeader == "" {
		return nil, errors.New("missing Authorization header")
	}

	if !strings.HasPrefix(authHeader, "Bearer ") {
		return nil, errors.New("invalid Authorization header format")
	}

	token := strings.TrimPrefix(authHeader, "Bearer ")
	if token == "" {
		return nil, errors.New("empty Bearer token")
	}

	tokenBytes, err := base64.StdEncoding.DecodeString(token)
	if err != nil {
		return nil, fmt.Errorf("invalid base64 token: %v", err)
	}

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

	now := time.Now().Unix()

	gracePeriod := int64(28800)

	userAgent := r.Header.Get("User-Agent")
	isMobileXMPP := strings.Contains(strings.ToLower(userAgent), "conversations") ||
		strings.Contains(strings.ToLower(userAgent), "dino") ||
		strings.Contains(strings.ToLower(userAgent), "gajim") ||
		strings.Contains(strings.ToLower(userAgent), "android") ||
		strings.Contains(strings.ToLower(userAgent), "mobile") ||
		strings.Contains(strings.ToLower(userAgent), "xmpp") ||
		strings.Contains(strings.ToLower(userAgent), "client") ||
		strings.Contains(strings.ToLower(userAgent), "bot")

	isDesktopXMPP := strings.Contains(strings.ToLower(userAgent), "dino") ||
		strings.Contains(strings.ToLower(userAgent), "gajim")

	if isMobileXMPP || isDesktopXMPP {
		if isDesktopXMPP {
			gracePeriod = int64(86400)
			log.Infof("Desktop XMPP client detected (%s), using 24-hour grace period for session restoration", userAgent)
		} else {
			gracePeriod = int64(43200)
			log.Infof("Mobile XMPP client detected (%s), using extended 12-hour grace period", userAgent)
		}
	}

	sessionId := query.Get("session_id")
	networkResilience := query.Get("network_resilience")
	resumeAllowed := query.Get("resume_allowed")

	if sessionId != "" || networkResilience == "true" || resumeAllowed == "true" {
		gracePeriod = int64(86400)
		log.Infof("Network resilience mode activated (session_id: %s, network_resilience: %s), using 24-hour grace period",
			sessionId, networkResilience)
	}

	clientIP := utils.GetClientIP(r)
	xForwardedFor := r.Header.Get("X-Forwarded-For")
	xRealIP := r.Header.Get("X-Real-IP")

	if xForwardedFor != "" || xRealIP != "" {
		gracePeriod = int64(86400)
		log.Infof("Network switching detected (client IP: %s, X-Forwarded-For: %s, X-Real-IP: %s), using 24-hour grace period",
			clientIP, xForwardedFor, xRealIP)
	}

	contentLength := r.Header.Get("Content-Length")
	var size int64 = 0
	if contentLength != "" {
		size, _ = strconv.ParseInt(contentLength, 10, 64)
		if size > 10*1024*1024 {
			additionalTime := (size / (10 * 1024 * 1024)) * 3600
			gracePeriod += additionalTime
			log.Infof("Large file detected (%d bytes), extending grace period by %d seconds", size, additionalTime)
		}
	}

	maxAbsoluteGrace := int64(172800)
	if gracePeriod > maxAbsoluteGrace {
		gracePeriod = maxAbsoluteGrace
		log.Infof("Grace period capped at 48 hours maximum")
	}

	isLikelyStandbyRecovery := false
	standbyGraceExtension := int64(86400)

	if now > expiry {
		expiredTime := now - expiry

		if expiredTime > gracePeriod && expiredTime < (gracePeriod+standbyGraceExtension) {
			isLikelyStandbyRecovery = true
			log.Infof("STANDBY RECOVERY: Token expired %d seconds ago, within standby recovery window", expiredTime)
		}

		if expiredTime > gracePeriod && !isLikelyStandbyRecovery {
			isDesktopSessionRestore := false
			if isDesktopXMPP && expiredTime < int64(172800) {
				isDesktopSessionRestore = true
				log.Infof("DESKTOP SESSION RESTORE: %s token expired %d seconds ago, allowing within 48-hour desktop restoration window", userAgent, expiredTime)
			}

			ultraMaxGrace := int64(259200)
			if (isMobileXMPP && expiredTime < ultraMaxGrace) || isDesktopSessionRestore {
				if isMobileXMPP {
					log.Warnf("ULTRA-GRACE: Mobile XMPP client token expired %d seconds ago, allowing within 72-hour ultra-grace window", expiredTime)
				}
			} else {
				log.Warnf("Bearer token expired beyond all grace periods: now=%d, expiry=%d, expired_for=%d seconds, grace_period=%d, user_agent=%s",
					now, expiry, expiredTime, gracePeriod, userAgent)
				return nil, fmt.Errorf("token has expired beyond grace period (expired %d seconds ago, grace period: %d seconds)",
					expiredTime, gracePeriod)
			}
		} else if isLikelyStandbyRecovery {
			log.Infof("STANDBY RECOVERY successful: allowing token within extended standby window")
		} else {
			log.Infof("Bearer token expired but within grace period: %d seconds remaining", gracePeriod-expiredTime)
		}
	} else {
		log.Debugf("Bearer token still valid: %d seconds until expiry", expiry-now)
	}

	pathParts := strings.Split(strings.Trim(r.URL.Path, "/"), "/")
	if len(pathParts) < 1 {
		return nil, errors.New("invalid upload path format")
	}

	filename := ""
	if len(pathParts) >= 3 {
		filename = pathParts[len(pathParts)-1]
	} else if len(pathParts) >= 1 {
		filename = pathParts[len(pathParts)-1]
	}

	if filename == "" {
		filename = "upload"
	}

	var validPayload bool
	var payloadFormat string

	extendedPayload := fmt.Sprintf("%s\x00%s\x00%d\x00%d\x00%d\x00network_resilient",
		user, filename, size, expiry-86400, expiry)
	h1 := hmac.New(sha256.New, []byte(secret))
	h1.Write([]byte(extendedPayload))
	expectedMAC1 := h1.Sum(nil)

	if hmac.Equal(tokenBytes, expectedMAC1) {
		validPayload = true
		payloadFormat = "network_resilient"
	}

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
		log.Warnf("Invalid Bearer token HMAC for user %s, file %s (tried all 5 payload formats)", user, filename)
		return nil, errors.New("invalid Bearer token HMAC")
	}

	claims := &BearerTokenClaims{
		User:     user,
		Filename: filename,
		Size:     size,
		Expiry:   expiry,
	}

	log.Infof("Bearer token authentication SUCCESSFUL: user=%s, file=%s, format=%s, grace_period=%d seconds",
		user, filename, payloadFormat, gracePeriod)

	return claims, nil
}

// EvaluateSecurityLevel determines the required security level based on network changes and standby detection.
func EvaluateSecurityLevel(session *NetworkResilientSession, currentIP string, userAgent string) int {
	now := time.Now()

	if session.LastSecurityCheck.IsZero() {
		session.LastSecurityCheck = now
		session.LastActivity = now
		session.SecurityLevel = 1
		return 1
	}

	timeSinceLastActivity := now.Sub(session.LastActivity)
	standbyThreshold := 30 * time.Minute

	if timeSinceLastActivity > standbyThreshold {
		session.StandbyDetected = true
		log.Infof("STANDBY DETECTED: %v since last activity for session %s", timeSinceLastActivity, session.SessionID)

		if timeSinceLastActivity > 2*time.Hour {
			log.Warnf("SECURITY LEVEL 3: Long standby (%v) requires full re-authentication", timeSinceLastActivity)
			return 3
		}

		log.Infof("SECURITY LEVEL 2: Medium standby (%v) requires challenge-response", timeSinceLastActivity)
		return 2
	}

	if session.LastIP != "" && session.LastIP != currentIP {
		session.NetworkChangeCount++
		log.Infof("NETWORK CHANGE #%d: %s -> %s for session %s",
			session.NetworkChangeCount, session.LastIP, currentIP, session.SessionID)

		if session.NetworkChangeCount > 3 {
			log.Warnf("SECURITY LEVEL 3: Multiple network changes (%d) requires full re-authentication",
				session.NetworkChangeCount)
			return 3
		}

		log.Infof("SECURITY LEVEL 2: Network change requires challenge-response")
		return 2
	}

	if session.UserAgent != "" && session.UserAgent != userAgent {
		log.Warnf("SECURITY LEVEL 3: User agent change detected - potential device hijacking")
		return 3
	}

	return 1
}

// GenerateSecurityChallenge creates a challenge for Level 2 authentication.
func GenerateSecurityChallenge(session *NetworkResilientSession, secret string) (string, error) {
	timestamp := time.Now().Unix()
	challengeData := fmt.Sprintf("%s:%s:%d", session.SessionID, session.UserJID, timestamp)

	h := hmac.New(sha256.New, []byte(secret))
	h.Write([]byte(challengeData))
	challenge := hex.EncodeToString(h.Sum(nil))

	log.Infof("Generated security challenge for session %s", session.SessionID)
	return challenge, nil
}

// ValidateSecurityChallenge verifies Level 2 challenge-response.
func ValidateSecurityChallenge(session *NetworkResilientSession, providedResponse string, secret string) bool {
	timestamp := time.Now().Unix()

	for i := int64(0); i <= 300; i += 60 {
		testTimestamp := timestamp - i
		challengeData := fmt.Sprintf("%s:%s:%d", session.SessionID, session.UserJID, testTimestamp)

		h := hmac.New(sha256.New, []byte(secret))
		h.Write([]byte(challengeData))
		expectedResponse := hex.EncodeToString(h.Sum(nil))

		if expectedResponse == providedResponse {
			log.Infof("Security challenge validated for session %s", session.SessionID)
			return true
		}
	}

	log.Warnf("Security challenge failed for session %s", session.SessionID)
	return false
}

// SetSecurityHeaders adds appropriate headers for re-authentication requests.
func SetSecurityHeaders(w http.ResponseWriter, securityLevel int, challenge string) {
	switch securityLevel {
	case 2:
		w.Header().Set("WWW-Authenticate", fmt.Sprintf("HMAC-Challenge challenge=\"%s\"", challenge))
		w.Header().Set("X-Security-Level", "2")
		w.Header().Set("X-Auth-Required", "challenge-response")
	case 3:
		w.Header().Set("WWW-Authenticate", "HMAC realm=\"HMAC File Server\"")
		w.Header().Set("X-Security-Level", "3")
		w.Header().Set("X-Auth-Required", "full-authentication")
	default:
		w.Header().Set("X-Security-Level", "1")
	}
}

// ValidateBearerTokenWithSession validates Bearer token with session recovery support.
// ENHANCED FOR NETWORK SWITCHING: 5G ↔ WiFi transition support with session persistence
func ValidateBearerTokenWithSession(r *http.Request, secret string, store SessionStore, generateSessionIDFn func(string, string) string) (*BearerTokenClaims, error) {
	claims, err := ValidateBearerToken(r, secret)
	if err == nil {
		sessionID := GetSessionIDFromRequest(r)
		if sessionID == "" {
			sessionID = generateSessionIDFn(claims.User, claims.Filename)
		}

		session := store.GetSession(sessionID)
		if session == nil {
			session = &NetworkResilientSession{
				SessionID:          sessionID,
				UserJID:            claims.User,
				OriginalToken:      GetBearerTokenFromRequest(r),
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

		currentIP := utils.GetClientIP(r)
		userAgent := r.Header.Get("User-Agent")

		requiredSecurityLevel := EvaluateSecurityLevel(session, currentIP, userAgent)
		session.SecurityLevel = requiredSecurityLevel
		session.LastActivity = time.Now()

		if requiredSecurityLevel > 1 {
			w, ok := r.Context().Value(ResponseWriterKey).(http.ResponseWriter)
			if !ok {
				log.Errorf("Could not extract response writer for security headers")
				return nil, fmt.Errorf("security evaluation failed")
			}

			switch requiredSecurityLevel {
			case 2:
				challenge, err := GenerateSecurityChallenge(session, secret)
				if err != nil {
					log.Errorf("Failed to generate security challenge: %v", err)
					return nil, fmt.Errorf("security challenge generation failed")
				}

				challengeResponse := r.Header.Get("X-Challenge-Response")
				if challengeResponse == "" {
					SetSecurityHeaders(w, 2, challenge)
					return nil, fmt.Errorf("challenge-response required for network change")
				}

				if !ValidateSecurityChallenge(session, challengeResponse, secret) {
					SetSecurityHeaders(w, 2, challenge)
					return nil, fmt.Errorf("invalid challenge response")
				}

				log.Infof("Challenge-response validated for session %s", sessionID)

			case 3:
				SetSecurityHeaders(w, 3, "")
				log.Warnf("Full re-authentication required for session %s", sessionID)
				return nil, fmt.Errorf("full re-authentication required")
			}
		}

		if session.LastIP != "" && session.LastIP != currentIP {
			session.NetworkHistory = append(session.NetworkHistory, NetworkEvent{
				Timestamp:   time.Now(),
				FromNetwork: session.LastIP,
				ToNetwork:   currentIP,
				ClientIP:    currentIP,
				UserAgent:   userAgent,
				EventType:   "network_switch",
			})
			log.Infof("Network switch detected for session %s: %s -> %s",
				sessionID, session.LastIP, currentIP)
		}

		session.LastIP = currentIP
		session.UserAgent = userAgent
		store.StoreSession(sessionID, session)

		if w, ok := r.Context().Value(ResponseWriterKey).(http.ResponseWriter); ok {
			SetSessionHeaders(w, sessionID)
		}

		log.Infof("Bearer token valid, session updated: %s (user: %s)", sessionID, claims.User)
		return claims, nil
	}

	sessionID := GetSessionIDFromRequest(r)
	if sessionID != "" {
		session := store.GetSession(sessionID)
		if session != nil {
			sessionAge := time.Since(session.CreatedAt)
			if sessionAge < 72*time.Hour {
				log.Infof("Session recovery attempt for %s (age: %v)", sessionID, sessionAge)

				if session.RefreshCount < session.MaxRefreshes {
					_, refreshErr := RefreshSessionToken(session, secret, r)
					if refreshErr == nil {
						session.RefreshCount++
						session.LastSeen = time.Now()

						session.NetworkHistory = append(session.NetworkHistory, NetworkEvent{
							Timestamp: time.Now(),
							ClientIP:  utils.GetClientIP(r),
							UserAgent: r.Header.Get("User-Agent"),
							EventType: "token_refresh",
						})

						store.StoreSession(sessionID, session)

						refreshedClaims := &BearerTokenClaims{
							User:     session.UserJID,
							Filename: ExtractFilenameFromPath(r.URL.Path),
							Size:     ExtractSizeFromRequest(r),
							Expiry:   time.Now().Add(24 * time.Hour).Unix(),
						}

						log.Infof("Session recovery successful: %s (refresh #%d)",
							sessionID, session.RefreshCount)
						return refreshedClaims, nil
					}
				} else {
					log.Warnf("Session %s exceeded maximum refreshes (%d)",
						sessionID, session.MaxRefreshes)
				}
			} else {
				log.Warnf("Session %s expired (age: %v, max: 72h)", sessionID, sessionAge)
			}
		} else {
			log.Warnf("Session %s not found in store", sessionID)
		}
	}

	log.Warnf("Authentication failed: %v (no session recovery available)", err)
	return nil, fmt.Errorf("authentication failed: %v", err)
}

// ValidateHMAC validates the HMAC signature of the request for legacy protocols and POST uploads.
// ENHANCED FOR 100% WIFI ↔ LTE SWITCHING AND STANDBY RECOVERY RELIABILITY
func ValidateHMAC(r *http.Request, secret string) error {
	log.Debugf("validateHMAC: Validating request to %s with query: %s", r.URL.Path, r.URL.RawQuery)

	signature := r.Header.Get("X-Signature")
	if signature != "" {
		message := r.URL.Path
		h := hmac.New(sha256.New, []byte(secret))
		h.Write([]byte(message))
		expectedSignature := hex.EncodeToString(h.Sum(nil))

		if !hmac.Equal([]byte(signature), []byte(expectedSignature)) {
			log.Warnf("Invalid HMAC signature in X-Signature header")
			return errors.New("invalid HMAC signature in X-Signature header")
		}
		log.Debugf("X-Signature HMAC authentication successful")
		return nil
	}

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

	fileStorePath := strings.TrimPrefix(r.URL.Path, "/")

	var validMAC bool
	var messageFormat string

	mac := hmac.New(sha256.New, []byte(secret))

	if protocolVersion == "v" {
		message1 := fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)
		mac.Reset()
		mac.Write([]byte(message1))
		calculatedMAC1 := mac.Sum(nil)

		if providedMAC, err := hex.DecodeString(providedMACHex); err == nil {
			if hmac.Equal(calculatedMAC1, providedMAC) {
				validMAC = true
				messageFormat = "v_standard"
				log.Debugf("Legacy v protocol HMAC validated")
			}
		}

		if !validMAC {
			message2 := fileStorePath
			mac.Reset()
			mac.Write([]byte(message2))
			calculatedMAC2 := mac.Sum(nil)

			if providedMAC, err := hex.DecodeString(providedMACHex); err == nil {
				if hmac.Equal(calculatedMAC2, providedMAC) {
					validMAC = true
					messageFormat = "v_simple"
					log.Debugf("Legacy v protocol HMAC validated (simple format)")
				}
			}
		}
	} else {
		contentType := utils.GetContentType(fileStorePath)

		message1 := fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10) + "\x00" + contentType
		mac.Reset()
		mac.Write([]byte(message1))
		calculatedMAC1 := mac.Sum(nil)

		if providedMAC, err := hex.DecodeString(providedMACHex); err == nil {
			if hmac.Equal(calculatedMAC1, providedMAC) {
				validMAC = true
				messageFormat = protocolVersion + "_standard"
				log.Debugf("%s protocol HMAC validated (standard)", protocolVersion)
			}
		}

		if !validMAC {
			message2 := fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10)
			mac.Reset()
			mac.Write([]byte(message2))
			calculatedMAC2 := mac.Sum(nil)

			if providedMAC, err := hex.DecodeString(providedMACHex); err == nil {
				if hmac.Equal(calculatedMAC2, providedMAC) {
					validMAC = true
					messageFormat = protocolVersion + "_no_content_type"
					log.Debugf("%s protocol HMAC validated (no content type)", protocolVersion)
				}
			}
		}

		if !validMAC {
			message3 := fileStorePath
			mac.Reset()
			mac.Write([]byte(message3))
			calculatedMAC3 := mac.Sum(nil)

			if providedMAC, err := hex.DecodeString(providedMACHex); err == nil {
				if hmac.Equal(calculatedMAC3, providedMAC) {
					validMAC = true
					messageFormat = protocolVersion + "_simple"
					log.Debugf("%s protocol HMAC validated (simple path)", protocolVersion)
				}
			}
		}
	}

	if !validMAC {
		log.Warnf("Invalid MAC for %s protocol (tried all formats)", protocolVersion)
		return fmt.Errorf("invalid MAC for %s protocol", protocolVersion)
	}

	log.Infof("%s HMAC authentication SUCCESSFUL: format=%s, path=%s",
		protocolVersion, messageFormat, r.URL.Path)
	return nil
}

// ValidateV3HMAC validates the HMAC signature for v3 protocol (mod_http_upload_external).
// ENHANCED FOR 100% WIFI ↔ LTE SWITCHING AND STANDBY RECOVERY RELIABILITY
func ValidateV3HMAC(r *http.Request, secret string) error {
	query := r.URL.Query()

	signature := query.Get("v3")
	expiresStr := query.Get("expires")

	if signature == "" {
		return errors.New("missing v3 signature parameter")
	}

	if expiresStr == "" {
		return errors.New("missing expires parameter")
	}

	expires, err := strconv.ParseInt(expiresStr, 10, 64)
	if err != nil {
		return fmt.Errorf("invalid expires parameter: %v", err)
	}

	now := time.Now().Unix()

	if now > expires {
		gracePeriod := int64(28800)

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
			gracePeriod = int64(43200)
			log.Infof("V3: Mobile XMPP client detected (%s), using 12-hour grace period", userAgent)
		}

		sessionId := query.Get("session_id")
		networkResilience := query.Get("network_resilience")
		resumeAllowed := query.Get("resume_allowed")
		if sessionId != "" || networkResilience == "true" || resumeAllowed == "true" {
			gracePeriod = int64(86400)
			log.Infof("V3: Network resilience mode detected, using 24-hour grace period")
		}

		clientIP := utils.GetClientIP(r)
		xForwardedFor := r.Header.Get("X-Forwarded-For")
		xRealIP := r.Header.Get("X-Real-IP")

		if xForwardedFor != "" || xRealIP != "" {
			gracePeriod = int64(86400)
			log.Infof("V3: Network switching detected (IP: %s, X-Forwarded-For: %s), using 24-hour grace period",
				clientIP, xForwardedFor)
		}

		if contentLengthStr := r.Header.Get("Content-Length"); contentLengthStr != "" {
			if contentLength, parseErr := strconv.ParseInt(contentLengthStr, 10, 64); parseErr == nil {
				if contentLength > 10*1024*1024 {
					additionalTime := (contentLength / (10 * 1024 * 1024)) * 3600
					gracePeriod += additionalTime
					log.Infof("V3: Large file (%d bytes), extending grace period by %d seconds",
						contentLength, additionalTime)
				}
			}
		}

		maxGracePeriod := int64(172800)
		if gracePeriod > maxGracePeriod {
			gracePeriod = maxGracePeriod
			log.Infof("V3: Grace period capped at 48 hours maximum")
		}

		expiredTime := now - expires
		standbyGraceExtension := int64(86400)
		isLikelyStandbyRecovery := expiredTime > gracePeriod && expiredTime < (gracePeriod+standbyGraceExtension)

		if expiredTime > gracePeriod && !isLikelyStandbyRecovery {
			ultraMaxGrace := int64(259200)
			if isMobileXMPP && expiredTime < ultraMaxGrace {
				log.Warnf("V3 ULTRA-GRACE: Mobile client token expired %d seconds ago, allowing within 72-hour window", expiredTime)
			} else {
				log.Warnf("V3 signature expired beyond all grace periods: now=%d, expires=%d, expired_for=%d seconds, grace_period=%d, user_agent=%s",
					now, expires, expiredTime, gracePeriod, userAgent)
				return fmt.Errorf("signature has expired beyond grace period (expired %d seconds ago, grace period: %d seconds)",
					expiredTime, gracePeriod)
			}
		} else if isLikelyStandbyRecovery {
			log.Infof("V3 STANDBY RECOVERY: Allowing signature within extended standby window (expired %d seconds ago)", expiredTime)
		} else {
			log.Infof("V3 signature within grace period: %d seconds remaining", gracePeriod-expiredTime)
		}
	} else {
		log.Debugf("V3 signature still valid: %d seconds until expiry", expires-now)
	}

	var validSignature bool
	var messageFormat string

	message1 := fmt.Sprintf("%s\n%s\n%s", r.Method, expiresStr, r.URL.Path)
	h1 := hmac.New(sha256.New, []byte(secret))
	h1.Write([]byte(message1))
	expectedSignature1 := hex.EncodeToString(h1.Sum(nil))

	if hmac.Equal([]byte(signature), []byte(expectedSignature1)) {
		validSignature = true
		messageFormat = "standard_v3"
	}

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
		log.Warnf("Invalid V3 HMAC signature (tried all 3 formats)")
		return errors.New("invalid v3 HMAC signature")
	}

	log.Infof("V3 HMAC authentication SUCCESSFUL: format=%s, method=%s, path=%s",
		messageFormat, r.Method, r.URL.Path)
	return nil
}

// RefreshSessionToken generates a new token for an existing session.
func RefreshSessionToken(session *NetworkResilientSession, secret string, r *http.Request) (string, error) {
	if session.RefreshCount >= session.MaxRefreshes {
		return "", fmt.Errorf("maximum token refreshes exceeded")
	}

	timestamp := time.Now().Unix()
	expiry := timestamp + 86400
	filename := ExtractFilenameFromPath(r.URL.Path)
	size := ExtractSizeFromRequest(r)

	payload := fmt.Sprintf("%s\x00%s\x00%d\x00%d\x00%s\x00session_refresh",
		session.UserJID,
		filename,
		size,
		expiry,
		session.SessionID)

	h := hmac.New(sha256.New, []byte(secret))
	h.Write([]byte(payload))
	token := base64.StdEncoding.EncodeToString(h.Sum(nil))

	log.Infof("Generated refresh token for session %s (refresh #%d)",
		session.SessionID, session.RefreshCount+1)

	return token, nil
}

// GetBearerTokenFromRequest extracts the Bearer token string from the request.
func GetBearerTokenFromRequest(r *http.Request) string {
	authHeader := r.Header.Get("Authorization")
	if strings.HasPrefix(authHeader, "Bearer ") {
		return strings.TrimPrefix(authHeader, "Bearer ")
	}
	return ""
}

// ExtractFilenameFromPath extracts the filename from a URL path.
func ExtractFilenameFromPath(path string) string {
	pathParts := strings.Split(strings.Trim(path, "/"), "/")
	if len(pathParts) >= 1 {
		return pathParts[len(pathParts)-1]
	}
	return "unknown"
}

// ExtractSizeFromRequest tries to extract file size from Content-Length header or query params.
func ExtractSizeFromRequest(r *http.Request) int64 {
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

// ResponseWriterKey is the context key used to store http.ResponseWriter.
type ContextKey string

const ResponseWriterKey ContextKey = "responseWriter"

// SetSessionHeaders adds session response headers for client tracking.
func SetSessionHeaders(w http.ResponseWriter, sessionID string) {
	w.Header().Set("X-Session-ID", sessionID)
	w.Header().Set("X-Session-Timeout", "259200")
	w.Header().Set("X-Network-Resilience", "enabled")
}

// GetSessionIDFromRequest extracts session ID from request headers, query params, or auth token.
func GetSessionIDFromRequest(r *http.Request) string {
	if sessionID := r.Header.Get("X-Session-ID"); sessionID != "" {
		return sessionID
	}
	if sessionID := r.URL.Query().Get("session_id"); sessionID != "" {
		return sessionID
	}
	if auth := r.Header.Get("Authorization"); strings.HasPrefix(auth, "Bearer ") {
		token := strings.TrimPrefix(auth, "Bearer ")
		h := sha256.New()
		h.Write([]byte(token))
		return fmt.Sprintf("auth_%s", hex.EncodeToString(h.Sum(nil))[:16])
	}
	return ""
}

// GenerateSessionID generates a session ID from user and context.
func GenerateSessionID(userJID, filename string) string {
	h := sha256.New()
	h.Write([]byte(fmt.Sprintf("%s:%s:%d", userJID, filename, time.Now().UnixNano())))
	return fmt.Sprintf("sess_%s", hex.EncodeToString(h.Sum(nil))[:16])
}

// GenerateUploadSessionID generates a session ID for multi-upload scenarios.
func GenerateUploadSessionID(uploadType, userAgent, clientIP string) string {
	h := sha256.New()
	h.Write([]byte(fmt.Sprintf("%s:%s:%s:%d", uploadType, userAgent, clientIP, time.Now().UnixNano())))
	return fmt.Sprintf("upload_%s", hex.EncodeToString(h.Sum(nil))[:16])
}
