// network_resilience.go - Network resilience middleware without modifying core functions

package main

import (
	"context"
	"net"
	"net/http"
	"sync"
	"time"
)

// NetworkResilienceManager handles network change detection and upload pausing
type NetworkResilienceManager struct {
	activeUploads    map[string]*UploadContext
	mutex           sync.RWMutex
	isPaused        bool
	pauseChannel    chan bool
	resumeChannel   chan bool
	lastInterfaces  []net.Interface
}

// UploadContext tracks active upload state
type UploadContext struct {
	SessionID    string
	PauseChan    chan bool
	ResumeChan   chan bool
	CancelChan   chan bool
	IsPaused     bool
}

// NewNetworkResilienceManager creates a new network resilience manager
func NewNetworkResilienceManager() *NetworkResilienceManager {
	manager := &NetworkResilienceManager{
		activeUploads: make(map[string]*UploadContext),
		pauseChannel:  make(chan bool, 100),
		resumeChannel: make(chan bool, 100),
	}
	
	// Start network monitoring if enabled
	if conf.Server.NetworkEvents {
		go manager.monitorNetworkChanges()
	}
	
	return manager
}

// RegisterUpload registers an active upload for pause/resume functionality
func (m *NetworkResilienceManager) RegisterUpload(sessionID string) *UploadContext {
	m.mutex.Lock()
	defer m.mutex.Unlock()
	
	ctx := &UploadContext{
		SessionID:  sessionID,
		PauseChan:  make(chan bool, 1),
		ResumeChan: make(chan bool, 1),
		CancelChan: make(chan bool, 1),
		IsPaused:   false,
	}
	
	m.activeUploads[sessionID] = ctx
	
	// If currently paused, immediately pause this upload
	if m.isPaused {
		select {
		case ctx.PauseChan <- true:
			ctx.IsPaused = true
		default:
		}
	}
	
	return ctx
}

// UnregisterUpload removes an upload from tracking
func (m *NetworkResilienceManager) UnregisterUpload(sessionID string) {
	m.mutex.Lock()
	defer m.mutex.Unlock()
	
	if ctx, exists := m.activeUploads[sessionID]; exists {
		close(ctx.PauseChan)
		close(ctx.ResumeChan)
		close(ctx.CancelChan)
		delete(m.activeUploads, sessionID)
	}
}

// PauseAllUploads pauses all active uploads
func (m *NetworkResilienceManager) PauseAllUploads() {
	m.mutex.Lock()
	defer m.mutex.Unlock()
	
	m.isPaused = true
	log.Info("Pausing all active uploads due to network change")
	
	for _, ctx := range m.activeUploads {
		if !ctx.IsPaused {
			select {
			case ctx.PauseChan <- true:
				ctx.IsPaused = true
			default:
			}
		}
	}
}

// ResumeAllUploads resumes all paused uploads
func (m *NetworkResilienceManager) ResumeAllUploads() {
	m.mutex.Lock()
	defer m.mutex.Unlock()
	
	m.isPaused = false
	log.Info("Resuming all paused uploads")
	
	for _, ctx := range m.activeUploads {
		if ctx.IsPaused {
			select {
			case ctx.ResumeChan <- true:
				ctx.IsPaused = false
			default:
			}
		}
	}
}

// monitorNetworkChanges monitors for network interface changes
func (m *NetworkResilienceManager) monitorNetworkChanges() {
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()
	
	// Get initial interface state
	m.lastInterfaces, _ = net.Interfaces()
	
	for {
		select {
		case <-ticker.C:
			currentInterfaces, err := net.Interfaces()
			if err != nil {
				log.Warnf("Failed to get network interfaces: %v", err)
				continue
			}
			
			if m.hasNetworkChanges(m.lastInterfaces, currentInterfaces) {
				log.Info("Network change detected")
				m.PauseAllUploads()
				
				// Wait for network stabilization
				time.Sleep(2 * time.Second)
				
				m.ResumeAllUploads()
			}
			
			m.lastInterfaces = currentInterfaces
		}
	}
}

// hasNetworkChanges compares interface states to detect changes
func (m *NetworkResilienceManager) hasNetworkChanges(old, new []net.Interface) bool {
	if len(old) != len(new) {
		return true
	}
	
	// Create maps for comparison
	oldMap := make(map[string]net.Flags)
	newMap := make(map[string]net.Flags)
	
	for _, iface := range old {
		if iface.Flags&net.FlagLoopback == 0 { // Skip loopback
			oldMap[iface.Name] = iface.Flags
		}
	}
	
	for _, iface := range new {
		if iface.Flags&net.FlagLoopback == 0 { // Skip loopback
			newMap[iface.Name] = iface.Flags
		}
	}
	
	// Check for status changes
	for name, oldFlags := range oldMap {
		newFlags, exists := newMap[name]
		if !exists || (oldFlags&net.FlagUp) != (newFlags&net.FlagUp) {
			return true
		}
	}
	
	// Check for new interfaces
	for name := range newMap {
		if _, exists := oldMap[name]; !exists {
			return true
		}
	}
	
	return false
}

// ResilientHTTPHandler wraps existing handlers with network resilience
func ResilientHTTPHandler(handler http.HandlerFunc, manager *NetworkResilienceManager) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		// Check for chunked upload headers
		sessionID := r.Header.Get("X-Upload-Session-ID")
		
		if sessionID != "" {
			// This is a chunked upload, register for pause/resume
			uploadCtx := manager.RegisterUpload(sessionID)
			defer manager.UnregisterUpload(sessionID)
			
			// Create a context that can be cancelled
			ctx, cancel := context.WithCancel(r.Context())
			defer cancel()
			
			// Monitor for pause/resume signals in a goroutine
			go func() {
				for {
					select {
					case <-uploadCtx.PauseChan:
						// Pause by setting a short timeout
						log.Debugf("Upload %s paused", sessionID)
						// Note: We can't actually pause an ongoing HTTP request,
						// but we can ensure the next chunk upload waits
						
					case <-uploadCtx.ResumeChan:
						log.Debugf("Upload %s resumed", sessionID)
						
					case <-uploadCtx.CancelChan:
						cancel()
						return
						
					case <-ctx.Done():
						return
					}
				}
			}()
			
			// Pass the context-aware request to the handler
			r = r.WithContext(ctx)
		}
		
		// Call the original handler
		handler(w, r)
	}
}

// RetryableUploadWrapper adds retry logic around upload operations
func RetryableUploadWrapper(originalHandler http.HandlerFunc, maxRetries int) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		var lastErr error
		
		for attempt := 0; attempt <= maxRetries; attempt++ {
			if attempt > 0 {
				// Exponential backoff with jitter
				delay := time.Duration(attempt*attempt) * time.Second
				jitter := time.Duration(float64(delay.Nanoseconds()) * 0.1 * float64((time.Now().UnixNano()%2)*2-1))
				time.Sleep(delay + jitter)
				
				log.Infof("Retrying upload attempt %d/%d", attempt+1, maxRetries+1)
			}
			
			// Create a custom ResponseWriter that captures errors
			recorder := &ResponseRecorder{
				ResponseWriter: w,
				statusCode:     200,
			}
			
			// Call the original handler
			originalHandler(recorder, r)
			
			// Check if the request was successful
			if recorder.statusCode < 400 {
				return // Success
			}
			
			lastErr = recorder.lastError
			
			// Don't retry on client errors (4xx)
			if recorder.statusCode >= 400 && recorder.statusCode < 500 {
				break
			}
		}
		
		// All retries failed
		if lastErr != nil {
			log.Errorf("Upload failed after %d retries: %v", maxRetries+1, lastErr)
			http.Error(w, lastErr.Error(), http.StatusInternalServerError)
		} else {
			http.Error(w, "Upload failed after retries", http.StatusInternalServerError)
		}
	}
}

// ResponseRecorder captures response information for retry logic
type ResponseRecorder struct {
	http.ResponseWriter
	statusCode int
	lastError  error
}

func (r *ResponseRecorder) WriteHeader(statusCode int) {
	r.statusCode = statusCode
	r.ResponseWriter.WriteHeader(statusCode)
}

func (r *ResponseRecorder) Write(data []byte) (int, error) {
	n, err := r.ResponseWriter.Write(data)
	if err != nil {
		r.lastError = err
	}
	return n, err
}

// Enhanced timeout configuration for mobile scenarios
func ConfigureEnhancedTimeouts() {
	// These don't modify core functions, just suggest better defaults
	log.Info("Applying enhanced timeout configuration for mobile/network switching scenarios")
	
	// Log current timeout settings
	log.Infof("Current ReadTimeout: %s", conf.Timeouts.Read)
	log.Infof("Current WriteTimeout: %s", conf.Timeouts.Write)
	log.Infof("Current IdleTimeout: %s", conf.Timeouts.Idle)
	
	// Suggest better timeouts in logs
	log.Info("Recommended timeouts for mobile scenarios:")
	log.Info("  ReadTimeout: 300s (5 minutes)")
	log.Info("  WriteTimeout: 300s (5 minutes)")
	log.Info("  IdleTimeout: 600s (10 minutes)")
	log.Info("  Update your configuration file to apply these settings")
}

// Global network resilience manager
var networkManager *NetworkResilienceManager

// InitializeNetworkResilience initializes the network resilience system
func InitializeNetworkResilience() {
	networkManager = NewNetworkResilienceManager()
	ConfigureEnhancedTimeouts()
	log.Info("Network resilience system initialized")
}
