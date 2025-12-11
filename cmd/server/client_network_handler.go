// client_network_handler.go - Handles clients with multiple network interfaces
// This is the CORRECT implementation focusing on CLIENT multi-interface support

package main

import (
	"fmt"
	"net"
	"net/http"
	"strings"
	"sync"
	"time"
)

// ClientConnectionTracker manages clients that switch between network interfaces
type ClientConnectionTracker struct {
	sessions    map[string]*ClientSession  // sessionID -> session info
	ipToSession map[string]string          // IP -> sessionID for quick lookup
	mutex       sync.RWMutex
	config      *ClientNetworkConfig
}

// ClientSession represents a client that may connect from multiple IPs/interfaces
type ClientSession struct {
	SessionID    string
	ClientIPs    []string          // All IPs this session has used
	ConnectionType string          // mobile, wifi, ethernet, unknown
	LastSeen     time.Time
	UploadInfo   *UploadSessionInfo
	NetworkQuality float64         // 0-100 quality score
	mutex        sync.RWMutex
}

// UploadSessionInfo tracks upload progress across network switches
type UploadSessionInfo struct {
	FileName       string
	TotalSize      int64
	UploadedBytes  int64
	ChunkSize      int64
	LastChunkID    int
	Chunks         map[int]bool  // chunkID -> received
	Started        time.Time
	LastActivity   time.Time
}

// ClientNetworkConfig holds configuration for client network handling
type ClientNetworkConfig struct {
	SessionBasedTracking      bool          `toml:"session_based_tracking" mapstructure:"session_based_tracking"`
	AllowIPChanges           bool          `toml:"allow_ip_changes" mapstructure:"allow_ip_changes"`
	SessionMigrationTimeout  time.Duration // Will be parsed from string in main.go
	MaxIPChangesPerSession   int           `toml:"max_ip_changes_per_session" mapstructure:"max_ip_changes_per_session"`
	ClientConnectionDetection bool         `toml:"client_connection_detection" mapstructure:"client_connection_detection"`
	AdaptToClientNetwork     bool          `toml:"adapt_to_client_network" mapstructure:"adapt_to_client_network"`
}

// ConnectionType represents different client connection types
type ConnectionType int

const (
	ConnectionUnknown ConnectionType = iota
	ConnectionMobile   // LTE/5G
	ConnectionWiFi     // WiFi
	ConnectionEthernet // Wired
)

func (ct ConnectionType) String() string {
	switch ct {
	case ConnectionMobile:
		return "mobile"
	case ConnectionWiFi:
		return "wifi"
	case ConnectionEthernet:
		return "ethernet"
	default:
		return "unknown"
	}
}

// NewClientConnectionTracker creates a new tracker for multi-interface clients
func NewClientConnectionTracker(config *ClientNetworkConfig) *ClientConnectionTracker {
	return &ClientConnectionTracker{
		sessions:    make(map[string]*ClientSession),
		ipToSession: make(map[string]string),
		config:      config,
	}
}

// DetectClientConnectionType analyzes the request to determine client connection type
func (cct *ClientConnectionTracker) DetectClientConnectionType(r *http.Request) string {
	// Check User-Agent for mobile indicators
	userAgent := strings.ToLower(r.Header.Get("User-Agent"))
	
	// Mobile detection
	if containsAny(userAgent, "mobile", "android", "iphone", "ipad", "phone") {
		return "mobile"
	}
	
	// Check for specific network indicators in headers
	// X-Forwarded-For might indicate client is behind a mobile carrier NAT
	// This is noted for future enhancement
	_ = r.Header.Get("X-Forwarded-For")
	
	// Check connection patterns (this would need more sophisticated logic)
	clientIP := getClientIP(r)
	if cct.isLikelyMobileIP(clientIP) {
		return "mobile"
	}
	
	// Default assumption for unknown
	return "unknown"
}

// TrackClientSession tracks a client session across potential IP changes
func (cct *ClientConnectionTracker) TrackClientSession(sessionID string, clientIP string, r *http.Request) *ClientSession {
	cct.mutex.Lock()
	defer cct.mutex.Unlock()
	
	// Check if this IP is already associated with a different session
	if existingSessionID, exists := cct.ipToSession[clientIP]; exists && existingSessionID != sessionID {
		// This IP was previously used by a different session
		// This could indicate a client that switched networks
		if cct.config.AllowIPChanges {
			// Remove old association
			delete(cct.ipToSession, clientIP)
		}
	}
	
	// Get or create session
	session, exists := cct.sessions[sessionID]
	if !exists {
		session = &ClientSession{
			SessionID:      sessionID,
			ClientIPs:      []string{clientIP},
			ConnectionType: cct.DetectClientConnectionType(r),
			LastSeen:       time.Now(),
			NetworkQuality: 100.0, // Start with good quality
		}
		cct.sessions[sessionID] = session
	} else {
		session.mutex.Lock()
		// Add this IP if it's not already tracked
		if !contains(session.ClientIPs, clientIP) {
			if len(session.ClientIPs) < cct.config.MaxIPChangesPerSession {
				session.ClientIPs = append(session.ClientIPs, clientIP)
				fmt.Printf("Client session %s now using new IP: %s (total IPs: %d)\n", 
					sessionID, clientIP, len(session.ClientIPs))
			}
		}
		session.LastSeen = time.Now()
		session.mutex.Unlock()
	}
	
	// Update IP to session mapping
	cct.ipToSession[clientIP] = sessionID
	
	return session
}

// GetOptimalChunkSize returns the optimal chunk size for a client's connection type
func (cct *ClientConnectionTracker) GetOptimalChunkSize(session *ClientSession) int64 {
	switch session.ConnectionType {
	case "mobile":
		return 256 * 1024 // 256KB for mobile/LTE
	case "wifi":
		return 2 * 1024 * 1024 // 2MB for WiFi
	case "ethernet":
		return 8 * 1024 * 1024 // 8MB for ethernet
	default:
		return 1 * 1024 * 1024 // 1MB default
	}
}

// GetOptimalTimeout returns the optimal timeout for a client's connection type
func (cct *ClientConnectionTracker) GetOptimalTimeout(session *ClientSession, baseTimeout time.Duration) time.Duration {
	switch session.ConnectionType {
	case "mobile":
		return time.Duration(float64(baseTimeout) * 2.0) // 2x timeout for mobile
	case "wifi":
		return baseTimeout // Standard timeout for WiFi
	case "ethernet":
		return time.Duration(float64(baseTimeout) * 0.8) // 0.8x timeout for ethernet
	default:
		return baseTimeout
	}
}

// HandleClientReconnection handles when a client reconnects from a different IP
func (cct *ClientConnectionTracker) HandleClientReconnection(sessionID string, newIP string, r *http.Request) error {
	cct.mutex.Lock()
	defer cct.mutex.Unlock()
	
	session, exists := cct.sessions[sessionID]
	if !exists {
		return fmt.Errorf("session %s not found", sessionID)
	}
	
	session.mutex.Lock()
	defer session.mutex.Unlock()
	
	// Check if this is actually a new IP
	if contains(session.ClientIPs, newIP) {
		// Client reconnected from known IP
		session.LastSeen = time.Now()
		return nil
	}
	
	// This is a new IP for this session - client likely switched networks
	if len(session.ClientIPs) >= cct.config.MaxIPChangesPerSession {
		return fmt.Errorf("session %s exceeded maximum IP changes (%d)", 
			sessionID, cct.config.MaxIPChangesPerSession)
	}
	
	// Add new IP and update connection type
	session.ClientIPs = append(session.ClientIPs, newIP)
	session.ConnectionType = cct.DetectClientConnectionType(r)
	session.LastSeen = time.Now()
	
	// Update IP mapping
	cct.ipToSession[newIP] = sessionID
	
	fmt.Printf("Client session %s reconnected from new IP %s (connection type: %s)\n",
		sessionID, newIP, session.ConnectionType)
	
	return nil
}

// ResumeUpload handles resuming an upload when client switches networks
func (cct *ClientConnectionTracker) ResumeUpload(sessionID string, uploadInfo *UploadSessionInfo) error {
	cct.mutex.RLock()
	session, exists := cct.sessions[sessionID]
	cct.mutex.RUnlock()
	
	if !exists {
		return fmt.Errorf("session %s not found for upload resume", sessionID)
	}
	
	session.mutex.Lock()
	session.UploadInfo = uploadInfo
	session.LastSeen = time.Now()
	session.mutex.Unlock()
	
	fmt.Printf("Resumed upload for session %s: %s (%d/%d bytes)\n",
		sessionID, uploadInfo.FileName, uploadInfo.UploadedBytes, uploadInfo.TotalSize)
	
	return nil
}

// CleanupStaleSession removes sessions that haven't been seen recently
func (cct *ClientConnectionTracker) CleanupStaleSessions() {
	cct.mutex.Lock()
	defer cct.mutex.Unlock()
	
	cutoff := time.Now().Add(-cct.config.SessionMigrationTimeout)
	
	for sessionID, session := range cct.sessions {
		if session.LastSeen.Before(cutoff) {
			// Remove from IP mappings
			for _, ip := range session.ClientIPs {
				delete(cct.ipToSession, ip)
			}
			// Remove session
			delete(cct.sessions, sessionID)
			fmt.Printf("Cleaned up stale session: %s\n", sessionID)
		}
	}
}

// isLikelyMobileIP attempts to determine if an IP is from a mobile carrier
func (cct *ClientConnectionTracker) isLikelyMobileIP(ip string) bool {
	// This is a simplified check - in practice, you'd check against
	// known mobile carrier IP ranges
	
	parsedIP := net.ParseIP(ip)
	if parsedIP == nil {
		return false
	}
	
	// Example: Some mobile carriers use specific IP ranges
	// This would need to be populated with actual carrier ranges
	mobileRanges := []string{
		"10.0.0.0/8",    // Some carriers use 10.x for mobile
		"172.16.0.0/12", // Some carriers use 172.x for mobile
	}
	
	for _, rangeStr := range mobileRanges {
		_, cidr, err := net.ParseCIDR(rangeStr)
		if err != nil {
			continue
		}
		if cidr.Contains(parsedIP) {
			return true
		}
	}
	
	return false
}

// Helper function to start cleanup routine
func (cct *ClientConnectionTracker) StartCleanupRoutine() {
	go func() {
		ticker := time.NewTicker(5 * time.Minute) // Clean up every 5 minutes
		defer ticker.Stop()
		
		for range ticker.C {
			cct.CleanupStaleSessions()
		}
	}()
}
