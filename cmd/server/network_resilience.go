// network_resilience.go - Enhanced network resilience with quality monitoring and fast detection

package main

import (
	"context"
	"fmt"
	"io"
	"net"
	"net/http"
	"os/exec"
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
	
	// Enhanced monitoring
	qualityMonitor   *NetworkQualityMonitor
	adaptiveTicker   *AdaptiveTicker
	config          *NetworkResilienceConfigLocal
}

// NetworkQualityMonitor tracks connection quality per interface
type NetworkQualityMonitor struct {
	interfaces map[string]*InterfaceQuality
	mutex      sync.RWMutex
	thresholds NetworkThresholds
}

// InterfaceQuality represents the quality metrics of a network interface
type InterfaceQuality struct {
	Name         string
	RTT          time.Duration
	PacketLoss   float64
	Bandwidth    int64
	Stability    float64
	LastGood     time.Time
	Connectivity ConnectivityState
	Samples      []QualitySample
}

// QualitySample represents a point-in-time quality measurement
type QualitySample struct {
	Timestamp  time.Time
	RTT        time.Duration
	PacketLoss float64
	Success    bool
}

// ConnectivityState represents the current state of network connectivity
type ConnectivityState int

const (
	ConnectivityUnknown ConnectivityState = iota
	ConnectivityGood
	ConnectivityDegraded
	ConnectivityPoor
	ConnectivityFailed
)

// NetworkThresholds defines quality thresholds for network assessment
type NetworkThresholds struct {
	RTTWarning      time.Duration  // 200ms
	RTTCritical     time.Duration  // 1000ms
	PacketLossWarn  float64        // 2%
	PacketLossCrit  float64        // 10%
	StabilityMin    float64        // 0.8
	SampleWindow    int            // Number of samples to keep
}

// NetworkResilienceConfigLocal holds configuration for enhanced network resilience
type NetworkResilienceConfigLocal struct {
	FastDetection        bool          `toml:"fast_detection"`
	QualityMonitoring    bool          `toml:"quality_monitoring"`
	PredictiveSwitching  bool          `toml:"predictive_switching"`
	MobileOptimizations  bool          `toml:"mobile_optimizations"`
	DetectionInterval    time.Duration `toml:"detection_interval"`
	QualityCheckInterval time.Duration `toml:"quality_check_interval"`
	MaxDetectionInterval time.Duration `toml:"max_detection_interval"`
}

// AdaptiveTicker provides adaptive timing for network monitoring
type AdaptiveTicker struct {
	C              <-chan time.Time
	ticker         *time.Ticker
	minInterval    time.Duration
	maxInterval    time.Duration
	currentInterval time.Duration
	unstableCount  int
	done           chan bool
}

// NewNetworkResilienceManager creates a new network resilience manager with enhanced capabilities
func NewNetworkResilienceManager() *NetworkResilienceManager {
	// Get configuration from global config, with sensible defaults
	config := &NetworkResilienceConfigLocal{
		FastDetection:        true,
		QualityMonitoring:    true,
		PredictiveSwitching:  true,
		MobileOptimizations:  true,
		DetectionInterval:    1 * time.Second,
		QualityCheckInterval: 5 * time.Second,
		MaxDetectionInterval: 10 * time.Second,
	}
	
	// Override with values from config file if available
	if conf.NetworkResilience.DetectionInterval != "" {
		if duration, err := time.ParseDuration(conf.NetworkResilience.DetectionInterval); err == nil {
			config.DetectionInterval = duration
		}
	}
	if conf.NetworkResilience.QualityCheckInterval != "" {
		if duration, err := time.ParseDuration(conf.NetworkResilience.QualityCheckInterval); err == nil {
			config.QualityCheckInterval = duration
		}
	}
	if conf.NetworkResilience.MaxDetectionInterval != "" {
		if duration, err := time.ParseDuration(conf.NetworkResilience.MaxDetectionInterval); err == nil {
			config.MaxDetectionInterval = duration
		}
	}
	
	// Override boolean settings if explicitly set
	config.FastDetection = conf.NetworkResilience.FastDetection
	config.QualityMonitoring = conf.NetworkResilience.QualityMonitoring
	config.PredictiveSwitching = conf.NetworkResilience.PredictiveSwitching
	config.MobileOptimizations = conf.NetworkResilience.MobileOptimizations
	
	// Create quality monitor with mobile-optimized thresholds
	thresholds := NetworkThresholds{
		RTTWarning:      200 * time.Millisecond,
		RTTCritical:     1000 * time.Millisecond,
		PacketLossWarn:  2.0,
		PacketLossCrit:  10.0,
		StabilityMin:    0.8,
		SampleWindow:    10,
	}
	
	// Adjust thresholds for mobile optimizations
	if config.MobileOptimizations {
		thresholds.RTTWarning = 500 * time.Millisecond    // More lenient for cellular
		thresholds.RTTCritical = 2000 * time.Millisecond  // Account for cellular latency
		thresholds.PacketLossWarn = 5.0                   // Higher tolerance for mobile
		thresholds.PacketLossCrit = 15.0                  // Mobile networks can be lossy
		thresholds.StabilityMin = 0.6                     // Lower stability expectations
	}
	
	qualityMonitor := &NetworkQualityMonitor{
		interfaces: make(map[string]*InterfaceQuality),
		thresholds: thresholds,
	}
	
	manager := &NetworkResilienceManager{
		activeUploads:  make(map[string]*UploadContext),
		pauseChannel:   make(chan bool, 100),
		resumeChannel:  make(chan bool, 100),
		qualityMonitor: qualityMonitor,
		config:        config,
	}
	
	// Create adaptive ticker for smart monitoring
	manager.adaptiveTicker = NewAdaptiveTicker(
		config.DetectionInterval,
		config.MaxDetectionInterval,
	)
	
	// Start enhanced network monitoring if enabled
	if conf.Server.NetworkEvents {
		if config.FastDetection {
			go manager.monitorNetworkChangesEnhanced()
			log.Info("Fast network change detection enabled")
		} else {
			go manager.monitorNetworkChanges() // Fallback to original method
			log.Info("Standard network change detection enabled")
		}
		
		if config.QualityMonitoring {
			go manager.monitorNetworkQuality()
			log.Info("Network quality monitoring enabled")
		}
	}
	
	log.Infof("Enhanced network resilience manager initialized with fast_detection=%v, quality_monitoring=%v, predictive_switching=%v",
		config.FastDetection, config.QualityMonitoring, config.PredictiveSwitching)
	return manager
}

// NewAdaptiveTicker creates a ticker that adjusts its interval based on network stability
func NewAdaptiveTicker(minInterval, maxInterval time.Duration) *AdaptiveTicker {
	ticker := &AdaptiveTicker{
		minInterval:     minInterval,
		maxInterval:     maxInterval,
		currentInterval: minInterval,
		done:           make(chan bool),
	}
	
	// Create initial ticker
	ticker.ticker = time.NewTicker(minInterval)
	ticker.C = ticker.ticker.C
	
	return ticker
}

// AdjustInterval adjusts the ticker interval based on network stability
func (t *AdaptiveTicker) AdjustInterval(stable bool) {
	if stable {
		// Network is stable, slow down monitoring
		t.unstableCount = 0
		newInterval := t.currentInterval * 2
		if newInterval > t.maxInterval {
			newInterval = t.maxInterval
		}
		if newInterval != t.currentInterval {
			t.currentInterval = newInterval
			t.ticker.Reset(newInterval)
			log.Debugf("Network stable, slowing monitoring to %v", newInterval)
		}
	} else {
		// Network is unstable, speed up monitoring
		t.unstableCount++
		newInterval := t.minInterval
		if newInterval != t.currentInterval {
			t.currentInterval = newInterval
			t.ticker.Reset(newInterval)
			log.Debugf("Network unstable, accelerating monitoring to %v", newInterval)
		}
	}
}

// Stop stops the adaptive ticker
func (t *AdaptiveTicker) Stop() {
	t.ticker.Stop()
	close(t.done)
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

// GetUploadContext retrieves the upload context for a given session ID
func (m *NetworkResilienceManager) GetUploadContext(sessionID string) *UploadContext {
	m.mutex.RLock()
	defer m.mutex.RUnlock()
	
	if ctx, exists := m.activeUploads[sessionID]; exists {
		return ctx
	}
	return nil
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

// monitorNetworkChangesEnhanced provides fast detection with quality monitoring
func (m *NetworkResilienceManager) monitorNetworkChangesEnhanced() {
	log.Info("Starting enhanced network monitoring with fast detection")
	
	// Get initial interface state
	m.lastInterfaces, _ = net.Interfaces()
	
	// Initialize quality monitoring for current interfaces
	m.initializeInterfaceQuality()
	
	for {
		select {
		case <-m.adaptiveTicker.C:
			currentInterfaces, err := net.Interfaces()
			if err != nil {
				log.Warnf("Failed to get network interfaces: %v", err)
				m.adaptiveTicker.AdjustInterval(false) // Network is unstable
				continue
			}
			
			// Check for interface changes
			interfaceChanged := m.hasNetworkChanges(m.lastInterfaces, currentInterfaces)
			
			// Check for quality degradation (predictive switching)
			qualityDegraded := false
			if m.config.PredictiveSwitching {
				qualityDegraded = m.checkQualityDegradation()
			}
			
			networkUnstable := interfaceChanged || qualityDegraded
			
			if interfaceChanged {
				log.Infof("Network interface change detected")
				m.handleNetworkSwitch("interface_change")
			} else if qualityDegraded {
				log.Infof("Network quality degradation detected, preparing for switch")
				m.prepareForNetworkSwitch()
			}
			
			// Adjust monitoring frequency based on stability
			m.adaptiveTicker.AdjustInterval(!networkUnstable)
			
			m.lastInterfaces = currentInterfaces
			
		case <-m.adaptiveTicker.done:
			log.Info("Network monitoring stopped")
			return
		}
	}
}

// monitorNetworkQuality continuously monitors connection quality
func (m *NetworkResilienceManager) monitorNetworkQuality() {
	ticker := time.NewTicker(m.config.QualityCheckInterval)
	defer ticker.Stop()
	
	log.Info("Starting network quality monitoring")
	
	for {
		select {
		case <-ticker.C:
			m.updateNetworkQuality()
		}
	}
}

// initializeInterfaceQuality sets up quality monitoring for current interfaces
func (m *NetworkResilienceManager) initializeInterfaceQuality() {
	interfaces, err := net.Interfaces()
	if err != nil {
		return
	}
	
	m.qualityMonitor.mutex.Lock()
	defer m.qualityMonitor.mutex.Unlock()
	
	for _, iface := range interfaces {
		if iface.Flags&net.FlagLoopback == 0 && iface.Flags&net.FlagUp != 0 {
			m.qualityMonitor.interfaces[iface.Name] = &InterfaceQuality{
				Name:         iface.Name,
				Connectivity: ConnectivityUnknown,
				LastGood:     time.Now(),
				Samples:      make([]QualitySample, 0, m.qualityMonitor.thresholds.SampleWindow),
			}
		}
	}
}

// updateNetworkQuality measures and updates quality metrics for all interfaces
func (m *NetworkResilienceManager) updateNetworkQuality() {
	m.qualityMonitor.mutex.Lock()
	defer m.qualityMonitor.mutex.Unlock()
	
	for name, quality := range m.qualityMonitor.interfaces {
		sample := m.measureInterfaceQuality(name)
		
		// Add sample to history
		quality.Samples = append(quality.Samples, sample)
		if len(quality.Samples) > m.qualityMonitor.thresholds.SampleWindow {
			quality.Samples = quality.Samples[1:]
		}
		
		// Update current metrics
		quality.RTT = sample.RTT
		quality.PacketLoss = m.calculatePacketLoss(quality.Samples)
		quality.Stability = m.calculateStability(quality.Samples)
		quality.Connectivity = m.assessConnectivity(quality)
		
		if sample.Success {
			quality.LastGood = time.Now()
		}
		
		log.Debugf("Interface %s: RTT=%v, Loss=%.1f%%, Stability=%.2f, State=%v",
			name, quality.RTT, quality.PacketLoss, quality.Stability, quality.Connectivity)
	}
}

// measureInterfaceQuality performs a quick connectivity test for an interface
func (m *NetworkResilienceManager) measureInterfaceQuality(interfaceName string) QualitySample {
	sample := QualitySample{
		Timestamp: time.Now(),
		RTT:       0,
		Success:   false,
	}
	
	// Use ping to measure RTT (simplified for demonstration)
	// In production, you'd want more sophisticated testing
	start := time.Now()
	
	// Try to ping a reliable host (Google DNS)
	cmd := exec.Command("ping", "-c", "1", "-W", "2", "8.8.8.8")
	err := cmd.Run()
	
	if err == nil {
		sample.RTT = time.Since(start)
		sample.Success = true
	} else {
		sample.RTT = 2 * time.Second // Timeout value
		sample.Success = false
	}
	
	return sample
}

// calculatePacketLoss calculates packet loss percentage from samples
func (m *NetworkResilienceManager) calculatePacketLoss(samples []QualitySample) float64 {
	if len(samples) == 0 {
		return 0
	}
	
	failed := 0
	for _, sample := range samples {
		if !sample.Success {
			failed++
		}
	}
	
	return float64(failed) / float64(len(samples)) * 100
}

// calculateStability calculates network stability from RTT variance
func (m *NetworkResilienceManager) calculateStability(samples []QualitySample) float64 {
	if len(samples) < 2 {
		return 1.0
	}
	
	// Calculate RTT variance
	var sum, sumSquares float64
	count := 0
	
	for _, sample := range samples {
		if sample.Success {
			rttMs := float64(sample.RTT.Nanoseconds()) / 1e6
			sum += rttMs
			sumSquares += rttMs * rttMs
			count++
		}
	}
	
	if count < 2 {
		return 1.0
	}
	
	mean := sum / float64(count)
	variance := (sumSquares / float64(count)) - (mean * mean)
	
	// Convert variance to stability score (lower variance = higher stability)
	if variance <= 100 { // Very stable (variance < 100ms²)
		return 1.0
	} else if variance <= 1000 { // Moderately stable
		return 1.0 - (variance-100)/900*0.3 // Scale from 1.0 to 0.7
	} else { // Unstable
		return 0.5 // Cap at 0.5 for very unstable connections
	}
}

// assessConnectivity determines connectivity state based on quality metrics
func (m *NetworkResilienceManager) assessConnectivity(quality *InterfaceQuality) ConnectivityState {
	thresholds := m.qualityMonitor.thresholds
	
	// Check if we have recent successful samples
	timeSinceLastGood := time.Since(quality.LastGood)
	if timeSinceLastGood > 30*time.Second {
		return ConnectivityFailed
	}
	
	// Assess based on packet loss
	if quality.PacketLoss >= thresholds.PacketLossCrit {
		return ConnectivityPoor
	} else if quality.PacketLoss >= thresholds.PacketLossWarn {
		return ConnectivityDegraded
	}
	
	// Assess based on RTT
	if quality.RTT >= thresholds.RTTCritical {
		return ConnectivityPoor
	} else if quality.RTT >= thresholds.RTTWarning {
		return ConnectivityDegraded
	}
	
	// Assess based on stability
	if quality.Stability < thresholds.StabilityMin {
		return ConnectivityDegraded
	}
	
	return ConnectivityGood
}

// checkQualityDegradation checks if any interface shows quality degradation
func (m *NetworkResilienceManager) checkQualityDegradation() bool {
	m.qualityMonitor.mutex.RLock()
	defer m.qualityMonitor.mutex.RUnlock()
	
	for _, quality := range m.qualityMonitor.interfaces {
		if quality.Connectivity == ConnectivityPoor || 
		   (quality.Connectivity == ConnectivityDegraded && quality.PacketLoss > 5.0) {
			return true
		}
	}
	
	return false
}

// prepareForNetworkSwitch proactively prepares for an anticipated network switch
func (m *NetworkResilienceManager) prepareForNetworkSwitch() {
	log.Info("Preparing for anticipated network switch due to quality degradation")
	
	// Temporarily pause new uploads but don't stop existing ones
	// This gives ongoing uploads a chance to complete before the switch
	m.mutex.Lock()
	defer m.mutex.Unlock()
	
	// Mark as preparing for switch (could be used by upload handlers)
	for _, ctx := range m.activeUploads {
		select {
		case ctx.PauseChan <- true:
			ctx.IsPaused = true
			log.Debugf("Preemptively paused upload %s", ctx.SessionID)
		default:
		}
	}
	
	// Resume after a short delay to allow network to stabilize
	go func() {
		time.Sleep(5 * time.Second)
		m.ResumeAllUploads()
	}()
}

// handleNetworkSwitch handles an actual network interface change
func (m *NetworkResilienceManager) handleNetworkSwitch(switchType string) {
	log.Infof("Handling network switch: %s", switchType)
	
	m.PauseAllUploads()
	
	// Wait for network stabilization (adaptive based on switch type)
	stabilizationTime := 2 * time.Second
	if switchType == "interface_change" {
		stabilizationTime = 3 * time.Second
	}
	
	time.Sleep(stabilizationTime)
	
	// Re-initialize quality monitoring for new network state
	m.initializeInterfaceQuality()
	
	m.ResumeAllUploads()
}

// monitorNetworkChanges provides the original network monitoring (fallback)
func (m *NetworkResilienceManager) monitorNetworkChanges() {
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()
	
	log.Info("Starting standard network monitoring (5s interval)")
	
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

// copyWithNetworkResilience performs io.Copy with network resilience support
func copyWithNetworkResilience(dst io.Writer, src io.Reader, uploadCtx *UploadContext) (int64, error) {
	if uploadCtx == nil {
		// Fallback to regular copy if no network resilience
		return io.Copy(dst, src)
	}
	
	const bufferSize = 32 * 1024 // 32KB buffer
	buf := make([]byte, bufferSize)
	var written int64
	
	for {
		// Check for network resilience signals before each read
		select {
		case <-uploadCtx.PauseChan:
			log.Debug("Upload paused due to network change, waiting for resume...")
			uploadCtx.IsPaused = true
			// Wait for resume signal
			<-uploadCtx.ResumeChan
			uploadCtx.IsPaused = false
			log.Debug("Upload resumed after network stabilization")
		case <-uploadCtx.CancelChan:
			return written, fmt.Errorf("upload cancelled due to network issues")
		default:
			// Continue with upload
		}
		
		// Read data
		nr, readErr := src.Read(buf)
		if nr > 0 {
			// Write data
			nw, writeErr := dst.Write(buf[:nr])
			if nw > 0 {
				written += int64(nw)
			}
			if writeErr != nil {
				return written, writeErr
			}
			if nr != nw {
				return written, io.ErrShortWrite
			}
		}
		if readErr != nil {
			if readErr != io.EOF {
				return written, readErr
			}
			break
		}
	}
	
	return written, nil
}
