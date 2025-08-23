// adaptive_io.go - Enhanced I/O engine with adaptive buffer management and network optimization

package main

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

// AdaptiveBufferPool manages multiple buffer pools of different sizes
type AdaptiveBufferPool struct {
	pools              map[int]*sync.Pool
	metrics           *NetworkMetrics
	currentOptimalSize int
	mutex             sync.RWMutex
	lastOptimization  time.Time
	optimizationInterval time.Duration
}

// NetworkMetrics tracks performance characteristics
type NetworkMetrics struct {
	ThroughputSamples []ThroughputSample
	LatencySamples    []time.Duration
	ErrorRate         float64
	LastUpdate        time.Time
	mutex             sync.RWMutex
}

// ThroughputSample represents a throughput measurement
type ThroughputSample struct {
	Timestamp  time.Time
	BytesPerSec int64
	BufferSize  int
}

// StreamingEngine provides unified streaming with adaptive optimization
type StreamingEngine struct {
	bufferPool       *AdaptiveBufferPool
	metrics         *NetworkMetrics
	resilienceManager *NetworkResilienceManager
	interfaceManager *MultiInterfaceManager
}

// ClientProfile stores optimization data per client
type ClientProfile struct {
	OptimalChunkSize  int64
	OptimalBufferSize int
	ReliabilityScore  float64
	AverageThroughput int64
	LastSeen         time.Time
	ConnectionType   string
	PreferredInterface string
	InterfaceHistory []InterfaceUsage
}

// InterfaceUsage tracks performance per network interface
type InterfaceUsage struct {
	InterfaceName     string
	LastUsed         time.Time
	AverageThroughput int64
	ReliabilityScore  float64
	OptimalBufferSize int
}

var (
	globalStreamingEngine *StreamingEngine
	clientProfiles       = make(map[string]*ClientProfile)
	clientProfilesMutex  sync.RWMutex
	multiInterfaceManager *MultiInterfaceManager
)

// Initialize the global streaming engine
func initStreamingEngine() {
	// Initialize multi-interface manager
	multiInterfaceManager = NewMultiInterfaceManager()
	
	globalStreamingEngine = &StreamingEngine{
		bufferPool:       NewAdaptiveBufferPool(),
		metrics:         NewNetworkMetrics(),
		interfaceManager: multiInterfaceManager,
	}
	
	// Start optimization routine
	go globalStreamingEngine.optimizationLoop()
	
	// Start multi-interface monitoring
	if conf.NetworkResilience.MultiInterfaceEnabled {
		go multiInterfaceManager.StartMonitoring()
		log.Info("Multi-interface monitoring enabled")
	}
	
	log.Info("Adaptive streaming engine with multi-interface support initialized")
}

// NewAdaptiveBufferPool creates a new adaptive buffer pool
func NewAdaptiveBufferPool() *AdaptiveBufferPool {
	pool := &AdaptiveBufferPool{
		pools:                make(map[int]*sync.Pool),
		metrics:              NewNetworkMetrics(),
		currentOptimalSize:   64 * 1024, // Start with 64KB
		optimizationInterval: 30 * time.Second,
	}
	
	// Initialize pools for different buffer sizes
	sizes := []int{
		16 * 1024,   // 16KB - for slow connections
		32 * 1024,   // 32KB - current default
		64 * 1024,   // 64KB - balanced
		128 * 1024,  // 128KB - fast connections
		256 * 1024,  // 256KB - very fast connections
		512 * 1024,  // 512KB - high-speed networks
		1024 * 1024, // 1MB - maximum for extreme cases
	}
	
	for _, size := range sizes {
		size := size // capture for closure
		pool.pools[size] = &sync.Pool{
			New: func() interface{} {
				buf := make([]byte, size)
				return &buf
			},
		}
	}
	
	return pool
}

// NewNetworkMetrics creates a new network metrics tracker
func NewNetworkMetrics() *NetworkMetrics {
	return &NetworkMetrics{
		ThroughputSamples: make([]ThroughputSample, 0, 100),
		LatencySamples:    make([]time.Duration, 0, 100),
	}
}

// GetOptimalBuffer returns the best buffer size for current conditions
func (abp *AdaptiveBufferPool) GetOptimalBuffer() (*[]byte, int) {
	abp.mutex.RLock()
	size := abp.currentOptimalSize
	abp.mutex.RUnlock()
	
	pool, exists := abp.pools[size]
	if !exists {
		// Fallback to 64KB if size not available
		size = 64 * 1024
		pool = abp.pools[size]
	}
	
	bufPtr := pool.Get().(*[]byte)
	return bufPtr, size
}

// PutBuffer returns a buffer to the appropriate pool
func (abp *AdaptiveBufferPool) PutBuffer(bufPtr *[]byte, size int) {
	if pool, exists := abp.pools[size]; exists {
		pool.Put(bufPtr)
	}
}

// StreamWithAdaptation performs streaming I/O with adaptive optimization
func (se *StreamingEngine) StreamWithAdaptation(
	dst io.Writer,
	src io.Reader,
	contentLength int64,
	sessionID string,
	clientIP string,
) (int64, error) {
	startTime := time.Now()
	
	// Get client profile for optimization
	profile := getClientProfile(clientIP)
	
	// Select optimal buffer size
	bufPtr, bufferSize := se.selectOptimalBuffer(contentLength, profile)
	defer se.bufferPool.PutBuffer(bufPtr, bufferSize)
	
	buf := *bufPtr
	var written int64
	var lastMetricUpdate time.Time
	
	for {
		// Check for network resilience signals
		if se.resilienceManager != nil {
			if uploadCtx := se.resilienceManager.GetUploadContext(sessionID); uploadCtx != nil {
				select {
				case <-uploadCtx.PauseChan:
					// Wait for resume signal
					<-uploadCtx.ResumeChan
				case <-uploadCtx.CancelChan:
					return written, fmt.Errorf("upload cancelled due to network issues")
				default:
					// Continue
				}
			}
		}
		
		// Read data
		n, readErr := src.Read(buf)
		if n > 0 {
			// Write data
			w, writeErr := dst.Write(buf[:n])
			written += int64(w)
			
			if writeErr != nil {
				se.recordError(clientIP, writeErr)
				return written, writeErr
			}
			
			// Update metrics periodically
			if time.Since(lastMetricUpdate) > time.Second {
				se.updateMetrics(written, startTime, bufferSize, clientIP)
				lastMetricUpdate = time.Now()
			}
		}
		
		if readErr != nil {
			if readErr == io.EOF {
				break
			}
			se.recordError(clientIP, readErr)
			return written, readErr
		}
	}
	
	// Final metrics update
	duration := time.Since(startTime)
	se.recordTransferComplete(written, duration, bufferSize, clientIP)
	
	return written, nil
}

// selectOptimalBuffer chooses the best buffer size based on various factors
func (se *StreamingEngine) selectOptimalBuffer(contentLength int64, profile *ClientProfile) (*[]byte, int) {
	// Start with current optimal size
	bufferSize := se.bufferPool.currentOptimalSize
	
	// Adjust based on file size
	if contentLength > 0 {
		if contentLength < 1024*1024 { // < 1MB
			bufferSize = minInt(bufferSize, 64*1024)
		} else if contentLength > 100*1024*1024 { // > 100MB
			bufferSize = maxInt(bufferSize, 256*1024)
		}
	}
	
	// Adjust based on client profile
	if profile != nil {
		if profile.OptimalBufferSize > 0 {
			bufferSize = profile.OptimalBufferSize
		}
		
		// Adjust for connection type
		switch profile.ConnectionType {
		case "mobile", "cellular":
			bufferSize = minInt(bufferSize, 64*1024)
		case "wifi":
			bufferSize = minInt(bufferSize, 256*1024)
		case "ethernet", "fiber":
			bufferSize = maxInt(bufferSize, 128*1024)
		}
	}
	
	return se.bufferPool.GetOptimalBuffer()
}

// updateMetrics records performance metrics
func (se *StreamingEngine) updateMetrics(bytesTransferred int64, startTime time.Time, bufferSize int, clientIP string) {
	duration := time.Since(startTime)
	if duration == 0 {
		return
	}
	
	throughput := bytesTransferred * int64(time.Second) / int64(duration)
	
	se.metrics.mutex.Lock()
	se.metrics.ThroughputSamples = append(se.metrics.ThroughputSamples, ThroughputSample{
		Timestamp:   time.Now(),
		BytesPerSec: throughput,
		BufferSize:  bufferSize,
	})
	
	// Keep only recent samples
	if len(se.metrics.ThroughputSamples) > 100 {
		se.metrics.ThroughputSamples = se.metrics.ThroughputSamples[1:]
	}
	
	se.metrics.LastUpdate = time.Now()
	se.metrics.mutex.Unlock()
	
	// Update client profile
	updateClientProfile(clientIP, throughput, bufferSize)
}

// recordTransferComplete records final transfer metrics
func (se *StreamingEngine) recordTransferComplete(bytesTransferred int64, duration time.Duration, bufferSize int, clientIP string) {
	if duration == 0 {
		return
	}
	
	throughput := bytesTransferred * int64(time.Second) / int64(duration)
	
	// Update global metrics
	se.updateMetrics(bytesTransferred, time.Now().Add(-duration), bufferSize, clientIP)
	
	// Log performance for large transfers
	if bytesTransferred > 10*1024*1024 {
		log.Debugf("Transfer complete: %s in %s (%.2f MB/s) using %dKB buffer",
			formatBytes(bytesTransferred),
			duration,
			float64(throughput)/(1024*1024),
			bufferSize/1024)
	}
}

// recordError records transfer errors
func (se *StreamingEngine) recordError(clientIP string, err error) {
	se.metrics.mutex.Lock()
	se.metrics.ErrorRate = se.metrics.ErrorRate*0.9 + 0.1 // Exponential moving average
	se.metrics.mutex.Unlock()
	
	log.Warnf("Transfer error for client %s: %v", clientIP, err)
}

// optimizationLoop continuously optimizes buffer sizes
func (se *StreamingEngine) optimizationLoop() {
	ticker := time.NewTicker(30 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-ticker.C:
			se.optimizeBufferSizes()
		}
	}
}

// optimizeBufferSizes analyzes performance and adjusts optimal buffer size
func (se *StreamingEngine) optimizeBufferSizes() {
	se.metrics.mutex.RLock()
	samples := make([]ThroughputSample, len(se.metrics.ThroughputSamples))
	copy(samples, se.metrics.ThroughputSamples)
	se.metrics.mutex.RUnlock()
	
	if len(samples) < 10 {
		return // Not enough data
	}
	
	// Analyze throughput by buffer size
	bufferPerformance := make(map[int][]int64)
	for _, sample := range samples {
		if time.Since(sample.Timestamp) < 5*time.Minute { // Only recent samples
			bufferPerformance[sample.BufferSize] = append(
				bufferPerformance[sample.BufferSize],
				sample.BytesPerSec,
			)
		}
	}
	
	// Find the buffer size with best average performance
	bestSize := se.bufferPool.currentOptimalSize
	bestPerformance := int64(0)
	
	for size, throughputs := range bufferPerformance {
		if len(throughputs) < 3 {
			continue // Not enough samples
		}
		
		var total int64
		for _, t := range throughputs {
			total += t
		}
		avg := total / int64(len(throughputs))
		
		if avg > bestPerformance {
			bestPerformance = avg
			bestSize = size
		}
	}
	
	// Update optimal size if significantly better
	if bestSize != se.bufferPool.currentOptimalSize {
		se.bufferPool.mutex.Lock()
		oldSize := se.bufferPool.currentOptimalSize
		se.bufferPool.currentOptimalSize = bestSize
		se.bufferPool.lastOptimization = time.Now()
		se.bufferPool.mutex.Unlock()
		
	log.Infof("Optimized buffer size: %dKB -> %dKB (%.2f%% improvement)",
		oldSize/1024,
		bestSize/1024,
		float64(bestPerformance-bestPerformance*int64(oldSize)/int64(bestSize))*100/float64(bestPerformance))
	}
}

// handleInterfaceSwitch handles network interface switching during transfers
func (se *StreamingEngine) handleInterfaceSwitch(oldInterface, newInterface string, reason SwitchReason) {
	log.Infof("Handling interface switch from %s to %s (reason: %s)", 
		oldInterface, newInterface, multiInterfaceManager.switchReasonString(reason))
	
	// Update client profiles with interface preference
	clientProfilesMutex.Lock()
	for clientIP, profile := range clientProfiles {
		// Update preferred interface if the new one performs better
		if profile.PreferredInterface == oldInterface {
			// Check if we have good performance data for the new interface
			for _, usage := range profile.InterfaceHistory {
				if usage.InterfaceName == newInterface && usage.ReliabilityScore > 0.8 {
					profile.PreferredInterface = newInterface
					log.Debugf("Updated preferred interface for client %s: %s -> %s", 
						clientIP, oldInterface, newInterface)
					break
				}
			}
		}
	}
	clientProfilesMutex.Unlock()
	
	// Adjust streaming parameters for the new interface if we have that data
	if se.interfaceManager != nil {
		if newIfaceInfo := se.interfaceManager.GetInterfaceInfo(newInterface); newIfaceInfo != nil {
			se.adjustParametersForInterface(newIfaceInfo)
		}
	}
	
	// Force buffer optimization on next transfer
	se.bufferPool.mutex.Lock()
	se.bufferPool.lastOptimization = time.Time{} // Force immediate re-optimization
	se.bufferPool.mutex.Unlock()
}

// adjustParametersForInterface adjusts streaming parameters based on interface type
func (se *StreamingEngine) adjustParametersForInterface(iface *NetworkInterface) {
	if iface == nil {
		return
	}
	
	// Adjust buffer pool optimal size based on interface type and quality
	var recommendedBufferSize int
	
	switch iface.Type {
	case InterfaceEthernet:
		recommendedBufferSize = 512 * 1024 // 512KB for Ethernet
		if iface.Quality != nil && iface.Quality.RTT < 10*time.Millisecond {
			recommendedBufferSize = 1024 * 1024 // 1MB for very fast Ethernet
		}
	case InterfaceWiFi:
		recommendedBufferSize = 256 * 1024 // 256KB for WiFi
		if iface.Quality != nil && iface.Quality.Stability > 0.9 {
			recommendedBufferSize = 512 * 1024 // 512KB for stable WiFi
		}
	case InterfaceLTE:
		recommendedBufferSize = 128 * 1024 // 128KB for LTE
		if iface.Quality != nil && iface.Quality.PacketLoss < 1.0 {
			recommendedBufferSize = 256 * 1024 // 256KB for good LTE
		}
	case InterfaceCellular:
		recommendedBufferSize = 64 * 1024 // 64KB for cellular
	case InterfaceVPN:
		recommendedBufferSize = 128 * 1024 // 128KB for VPN (account for overhead)
	default:
		recommendedBufferSize = 128 * 1024 // Default 128KB
	}
	
	// Update the adaptive buffer pool's optimal size
	se.bufferPool.mutex.Lock()
	se.bufferPool.currentOptimalSize = recommendedBufferSize
	se.bufferPool.mutex.Unlock()
	
	log.Debugf("Adjusted buffer size for interface %s (%s): %dKB", 
		iface.Name, multiInterfaceManager.interfaceTypeString(iface.Type), recommendedBufferSize/1024)
}// getClientProfile retrieves or creates a client profile
func getClientProfile(clientIP string) *ClientProfile {
	clientProfilesMutex.RLock()
	profile, exists := clientProfiles[clientIP]
	clientProfilesMutex.RUnlock()
	
	if exists {
		return profile
	}
	
	// Create new profile
	clientProfilesMutex.Lock()
	defer clientProfilesMutex.Unlock()
	
	// Double-check after acquiring write lock
	if profile, exists := clientProfiles[clientIP]; exists {
		return profile
	}
	
	profile = &ClientProfile{
		OptimalChunkSize:  2 * 1024 * 1024, // 2MB default
		OptimalBufferSize: 64 * 1024,       // 64KB default
		ReliabilityScore:  0.8,             // Assume good initially
		LastSeen:         time.Now(),
		ConnectionType:   "unknown",
	}
	
	clientProfiles[clientIP] = profile
	return profile
}

// updateClientProfile updates performance data for a client
func updateClientProfile(clientIP string, throughput int64, bufferSize int) {
	profile := getClientProfile(clientIP)
	
	clientProfilesMutex.Lock()
	defer clientProfilesMutex.Unlock()
	
	// Exponential moving average for throughput
	if profile.AverageThroughput == 0 {
		profile.AverageThroughput = throughput
	} else {
		profile.AverageThroughput = (profile.AverageThroughput*9 + throughput) / 10
	}
	
	// Update optimal buffer size if this performed well
	if throughput > profile.AverageThroughput*110/100 { // 10% better
		profile.OptimalBufferSize = bufferSize
	}
	
	// Track interface usage if multi-interface manager is available
	if multiInterfaceManager != nil {
		currentInterface := multiInterfaceManager.GetActiveInterface()
		if currentInterface != "" {
			updateInterfaceUsage(profile, currentInterface, throughput, bufferSize)
		}
	}
	
	profile.LastSeen = time.Now()
}

// updateInterfaceUsage updates interface-specific performance data
func updateInterfaceUsage(profile *ClientProfile, interfaceName string, throughput int64, bufferSize int) {
	// Find existing interface usage record
	var usage *InterfaceUsage
	for i := range profile.InterfaceHistory {
		if profile.InterfaceHistory[i].InterfaceName == interfaceName {
			usage = &profile.InterfaceHistory[i]
			break
		}
	}
	
	// Create new record if not found
	if usage == nil {
		profile.InterfaceHistory = append(profile.InterfaceHistory, InterfaceUsage{
			InterfaceName:     interfaceName,
			LastUsed:         time.Now(),
			AverageThroughput: throughput,
			ReliabilityScore:  0.8, // Start with good assumption
			OptimalBufferSize: bufferSize,
		})
	} else {
		// Update existing record
		usage.LastUsed = time.Now()
		usage.AverageThroughput = (usage.AverageThroughput*4 + throughput) / 5 // Faster adaptation
		
		// Update reliability score based on performance consistency
		if throughput > usage.AverageThroughput*90/100 { // Within 10% of average
			usage.ReliabilityScore = minFloat64(usage.ReliabilityScore+0.1, 1.0)
		} else {
			usage.ReliabilityScore = maxFloat64(usage.ReliabilityScore-0.1, 0.0)
		}
		
		// Update optimal buffer size if performance improved
		if throughput > usage.AverageThroughput {
			usage.OptimalBufferSize = bufferSize
		}
	}
	
	// Keep only recent interface history (last 10 interfaces)
	if len(profile.InterfaceHistory) > 10 {
		profile.InterfaceHistory = profile.InterfaceHistory[1:]
	}
	
	// Update preferred interface if this one is performing significantly better
	if usage != nil && (profile.PreferredInterface == "" || 
		usage.AverageThroughput > profile.AverageThroughput*120/100) {
		profile.PreferredInterface = interfaceName
	}
}

// detectConnectionType attempts to determine connection type from request
func detectConnectionType(r *http.Request) string {
	userAgent := r.Header.Get("User-Agent")
	
	// Simple heuristics - could be enhanced with more sophisticated detection
	if containsAny(userAgent, "Mobile", "Android", "iPhone", "iPad") {
		return "mobile"
	}
	
	// Check for specific client indicators
	if containsAny(userAgent, "curl", "wget", "HTTPie") {
		return "cli"
	}
	
	// Default assumption
	return "browser"
}

// containsAny checks if any of the substrings exist in the main string
func containsAny(s string, substrings ...string) bool {
	for _, substr := range substrings {
		if len(s) >= len(substr) {
			for i := 0; i <= len(s)-len(substr); i++ {
				if s[i:i+len(substr)] == substr {
					return true
				}
			}
		}
	}
	return false
}

// Helper functions for adaptive I/O
func minInt(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func maxInt(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func minFloat64(a, b float64) float64 {
	if a < b {
		return a
	}
	return b
}

func maxFloat64(a, b float64) float64 {
	if a > b {
		return a
	}
	return b
}

// Enhanced upload handler using the streaming engine
func handleUploadWithAdaptiveIO(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Standard authentication and validation...
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		uploadErrorsTotal.Inc()
		return
	}

	// Parse multipart form
	err := r.ParseMultipartForm(32 << 20) // 32MB max memory
	if err != nil {
		http.Error(w, fmt.Sprintf("Error parsing form: %v", err), http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	file, header, err := r.FormFile("file")
	if err != nil {
		http.Error(w, fmt.Sprintf("Error retrieving file: %v", err), http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}
	defer file.Close()

	// Generate filename and path...
	var filename string
	switch conf.Server.FileNaming {
	case "HMAC":
		h := hmac.New(sha256.New, []byte(conf.Security.Secret))
		h.Write([]byte(header.Filename + time.Now().String()))
		filename = hex.EncodeToString(h.Sum(nil)) + filepath.Ext(header.Filename)
	default:
		filename = header.Filename
	}

	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}
	absFilename := filepath.Join(storagePath, filename)

	// Create the file
	dst, err := os.Create(absFilename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error creating file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}
	defer dst.Close()

	// Use adaptive streaming engine
	clientIP := getClientIP(r)
	sessionID := generateSessionID()
	
	written, err := globalStreamingEngine.StreamWithAdaptation(
		dst,
		file,
		header.Size,
		sessionID,
		clientIP,
	)
	
	if err != nil {
		http.Error(w, fmt.Sprintf("Error saving file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		os.Remove(absFilename)
		return
	}

	// Update metrics
	duration := time.Since(startTime)
	uploadDuration.Observe(duration.Seconds())
	uploadsTotal.Inc()
	uploadSizeBytes.Observe(float64(written))

	// Return success response
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	response := map[string]interface{}{
		"success":  true,
		"filename": filename,
		"size":     written,
		"duration": duration.String(),
	}
	json.NewEncoder(w).Encode(response)

	log.Infof("Successfully uploaded %s (%s) in %s using adaptive I/O",
		filename, formatBytes(written), duration)
}

// Enhanced download handler with adaptive streaming
func handleDownloadWithAdaptiveIO(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Extract filename from URL path
	filename := filepath.Base(r.URL.Path)
	if filename == "." || filename == "/" {
		http.Error(w, "Invalid filename", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	// Construct full file path
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}
	absFilename := filepath.Join(storagePath, filename)

	// Sanitize the file path
	absFilename, err := sanitizeFilePath(storagePath, filename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Invalid file path: %v", err), http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	// Check if file exists
	fileInfo, err := os.Stat(absFilename)
	if os.IsNotExist(err) {
		http.Error(w, "File not found", http.StatusNotFound)
		downloadErrorsTotal.Inc()
		return
	}
	if err != nil {
		http.Error(w, fmt.Sprintf("Error accessing file: %v", err), http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}

	// Open the file
	file, err := os.Open(absFilename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error opening file: %v", err), http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}
	defer file.Close()

	// Set headers
	w.Header().Set("Content-Disposition", "attachment; filename=\""+filepath.Base(absFilename)+"\"")
	w.Header().Set("Content-Type", "application/octet-stream")
	w.Header().Set("Content-Length", fmt.Sprintf("%d", fileInfo.Size()))

	// Use adaptive streaming engine
	clientIP := getClientIP(r)
	sessionID := generateSessionID()
	
	n, err := globalStreamingEngine.StreamWithAdaptation(
		w,
		file,
		fileInfo.Size(),
		sessionID,
		clientIP,
	)
	
	if err != nil {
		log.Errorf("Error during download of %s: %v", absFilename, err)
		downloadErrorsTotal.Inc()
		return
	}

	// Update metrics
	duration := time.Since(startTime)
	downloadDuration.Observe(duration.Seconds())
	downloadsTotal.Inc()
	downloadSizeBytes.Observe(float64(n))

	log.Infof("Successfully downloaded %s (%s) in %s using adaptive I/O",
		filename, formatBytes(n), duration)
}

// MultiInterfaceManager handles multiple network interfaces for seamless switching
type MultiInterfaceManager struct {
	interfaces     map[string]*NetworkInterface
	activeInterface string
	mutex          sync.RWMutex
	switchHistory  []InterfaceSwitch
	config         *MultiInterfaceConfig
}

// NetworkInterface represents a network adapter
type NetworkInterface struct {
	Name            string
	Type            InterfaceType
	Priority        int
	Quality         *InterfaceQuality
	Active          bool
	Gateway         net.IP
	MTU             int
	LastSeen        time.Time
	ThroughputHistory []ThroughputSample
}

// InterfaceType represents different types of network connections
type InterfaceType int

const (
	InterfaceEthernet InterfaceType = iota
	InterfaceWiFi
	InterfaceLTE
	InterfaceCellular
	InterfaceVPN
	InterfaceUnknown
)

// InterfaceSwitch tracks interface switching events
type InterfaceSwitch struct {
	FromInterface  string
	ToInterface    string
	Timestamp      time.Time
	Reason         SwitchReason
	TransferStatus TransferStatus
	SessionID      string
}

// SwitchReason indicates why an interface switch occurred
type SwitchReason int

const (
	SwitchReasonQualityDegradation SwitchReason = iota
	SwitchReasonInterfaceDown
	SwitchReasonBetterAlternative
	SwitchReasonManual
	SwitchReasonTimeout
)

// TransferStatus indicates the status of transfers during switch
type TransferStatus int

const (
	TransferStatusContinuous TransferStatus = iota
	TransferStatusPaused
	TransferStatusFailed
	TransferStatusRetried
)

// MultiInterfaceConfig holds configuration for multi-interface support
type MultiInterfaceConfig struct {
	Enabled                    bool
	InterfacePriority         []string
	AutoSwitchEnabled         bool
	SwitchThresholdLatency    time.Duration
	SwitchThresholdPacketLoss float64
	QualityDegradationThreshold float64
	MaxSwitchAttempts         int
	SwitchDetectionInterval   time.Duration
}

// NewMultiInterfaceManager creates a new multi-interface manager
func NewMultiInterfaceManager() *MultiInterfaceManager {
	config := &MultiInterfaceConfig{
		Enabled:                    conf.NetworkResilience.MultiInterfaceEnabled,
		InterfacePriority:         []string{"eth0", "wlan0", "wwan0", "ppp0"},
		AutoSwitchEnabled:         true,
		SwitchThresholdLatency:    500 * time.Millisecond,
		SwitchThresholdPacketLoss: 5.0,
		QualityDegradationThreshold: 0.3,
		MaxSwitchAttempts:         3,
		SwitchDetectionInterval:   2 * time.Second,
	}
	
	return &MultiInterfaceManager{
		interfaces:    make(map[string]*NetworkInterface),
		switchHistory: make([]InterfaceSwitch, 0, 100),
		config:       config,
	}
}

// StartMonitoring begins monitoring all network interfaces
func (mim *MultiInterfaceManager) StartMonitoring() {
	ticker := time.NewTicker(mim.config.SwitchDetectionInterval)
	defer ticker.Stop()
	
	// Initial discovery
	mim.discoverInterfaces()
	
	for {
		select {
		case <-ticker.C:
			mim.updateInterfaceStatus()
			mim.evaluateInterfaceSwitching()
		}
	}
}

// discoverInterfaces discovers all available network interfaces
func (mim *MultiInterfaceManager) discoverInterfaces() {
	interfaces, err := net.Interfaces()
	if err != nil {
		log.Errorf("Failed to discover network interfaces: %v", err)
		return
	}
	
	mim.mutex.Lock()
	defer mim.mutex.Unlock()
	
	for _, iface := range interfaces {
		if iface.Flags&net.FlagUp != 0 && iface.Flags&net.FlagLoopback == 0 {
			netIface := &NetworkInterface{
				Name:     iface.Name,
				Type:     mim.detectInterfaceType(iface.Name),
				Priority: mim.getInterfacePriority(iface.Name),
				Active:   true,
				MTU:      iface.MTU,
				LastSeen: time.Now(),
				Quality:  &InterfaceQuality{
					Name:         iface.Name,
					Connectivity: ConnectivityUnknown,
				},
				ThroughputHistory: make([]ThroughputSample, 0, 50),
			}
			
			mim.interfaces[iface.Name] = netIface
			log.Infof("Discovered network interface: %s (type: %s, priority: %d)", 
				iface.Name, mim.interfaceTypeString(netIface.Type), netIface.Priority)
		}
	}
	
	// Set initial active interface
	if mim.activeInterface == "" {
		mim.activeInterface = mim.selectBestInterface()
	}
}

// detectInterfaceType determines the type of network interface
func (mim *MultiInterfaceManager) detectInterfaceType(name string) InterfaceType {
	switch {
	case strings.HasPrefix(name, "eth"), strings.HasPrefix(name, "en"):
		return InterfaceEthernet
	case strings.HasPrefix(name, "wlan"), strings.HasPrefix(name, "wl"):
		return InterfaceWiFi
	case strings.HasPrefix(name, "wwan"), strings.HasPrefix(name, "usb"):
		return InterfaceLTE
	case strings.HasPrefix(name, "ppp"), strings.HasPrefix(name, "rmnet"):
		return InterfaceCellular
	case strings.HasPrefix(name, "tun"), strings.HasPrefix(name, "tap"):
		return InterfaceVPN
	default:
		return InterfaceUnknown
	}
}

// GetActiveInterface returns the currently active network interface
func (mim *MultiInterfaceManager) GetActiveInterface() string {
	mim.mutex.RLock()
	defer mim.mutex.RUnlock()
	return mim.activeInterface
}

// selectBestInterface chooses the optimal network interface
func (mim *MultiInterfaceManager) selectBestInterface() string {
	mim.mutex.RLock()
	defer mim.mutex.RUnlock()
	
	var bestInterface *NetworkInterface
	var bestName string
	
	for name, iface := range mim.interfaces {
		if !iface.Active {
			continue
		}
		
		if bestInterface == nil || mim.isInterfaceBetter(iface, bestInterface) {
			bestInterface = iface
			bestName = name
		}
	}
	
	return bestName
}

// getInterfacePriority returns the priority of an interface (lower = higher priority)
func (mim *MultiInterfaceManager) getInterfacePriority(name string) int {
	for i, priority := range mim.config.InterfacePriority {
		if priority == name {
			return i
		}
	}
	
	// Default priority based on interface type
	interfaceType := mim.detectInterfaceType(name)
	switch interfaceType {
	case InterfaceEthernet:
		return 10
	case InterfaceWiFi:
		return 20
	case InterfaceLTE:
		return 30
	case InterfaceCellular:
		return 40
	case InterfaceVPN:
		return 50
	default:
		return 100
	}
}

// isInterfaceBetter determines if interface A is better than interface B
func (mim *MultiInterfaceManager) isInterfaceBetter(a, b *NetworkInterface) bool {
	// First check priority (lower number = higher priority)
	if a.Priority != b.Priority {
		return a.Priority < b.Priority
	}
	
	// Then check quality metrics if available
	if a.Quality != nil && b.Quality != nil {
		aScore := mim.calculateInterfaceScore(a)
		bScore := mim.calculateInterfaceScore(b)
		return aScore > bScore
	}
	
	// Fallback to priority only
	return a.Priority < b.Priority
}

// calculateInterfaceScore calculates a quality score for an interface
func (mim *MultiInterfaceManager) calculateInterfaceScore(iface *NetworkInterface) float64 {
	if iface.Quality == nil {
		return 0.0
	}
	
	score := 100.0 // Base score
	
	// Penalize high latency
	if iface.Quality.RTT > 100*time.Millisecond {
		score -= float64(iface.Quality.RTT.Milliseconds()) * 0.1
	}
	
	// Penalize packet loss
	score -= iface.Quality.PacketLoss * 10
	
	// Reward stability
	score += iface.Quality.Stability * 50
	
	// Adjust for interface type
	switch iface.Type {
	case InterfaceEthernet:
		score += 20 // Prefer wired connections
	case InterfaceWiFi:
		score += 10
	case InterfaceLTE:
		score += 5
	case InterfaceCellular:
		score += 0
	case InterfaceVPN:
		score -= 10 // VPN adds overhead
	}
	
	return maxFloat64(score, 0.0)
}

// updateInterfaceStatus updates the status of all interfaces
func (mim *MultiInterfaceManager) updateInterfaceStatus() {
	interfaces, err := net.Interfaces()
	if err != nil {
		log.Errorf("Failed to update interface status: %v", err)
		return
	}
	
	mim.mutex.Lock()
	defer mim.mutex.Unlock()
	
	// Mark all interfaces as potentially inactive
	for _, iface := range mim.interfaces {
		iface.Active = false
	}
	
	// Update active interfaces
	for _, iface := range interfaces {
		if iface.Flags&net.FlagUp != 0 && iface.Flags&net.FlagLoopback == 0 {
			if netIface, exists := mim.interfaces[iface.Name]; exists {
				netIface.Active = true
				netIface.LastSeen = time.Now()
				netIface.MTU = iface.MTU
			}
		}
	}
}

// evaluateInterfaceSwitching determines if an interface switch is needed
func (mim *MultiInterfaceManager) evaluateInterfaceSwitching() {
	if !mim.config.AutoSwitchEnabled {
		return
	}
	
	currentInterface := mim.GetActiveInterface()
	if currentInterface == "" {
		return
	}
	
	bestInterface := mim.selectBestInterface()
	
	if bestInterface != currentInterface && bestInterface != "" {
		reason := mim.determineSwitchReason(currentInterface, bestInterface)
		mim.switchToInterface(bestInterface, reason)
	}
}

// determineSwitchReason determines why an interface switch is needed
func (mim *MultiInterfaceManager) determineSwitchReason(current, target string) SwitchReason {
	mim.mutex.RLock()
	defer mim.mutex.RUnlock()
	
	currentIface := mim.interfaces[current]
	
	if currentIface == nil || !currentIface.Active {
		return SwitchReasonInterfaceDown
	}
	
	// Check if current interface quality has degraded
	if currentIface.Quality != nil {
		if currentIface.Quality.PacketLoss > mim.config.SwitchThresholdPacketLoss {
			return SwitchReasonQualityDegradation
		}
		if currentIface.Quality.RTT > mim.config.SwitchThresholdLatency {
			return SwitchReasonQualityDegradation
		}
	}
	
	return SwitchReasonBetterAlternative
}

// switchToInterface performs the actual interface switch
func (mim *MultiInterfaceManager) switchToInterface(newInterface string, reason SwitchReason) {
	mim.mutex.Lock()
	oldInterface := mim.activeInterface
	mim.activeInterface = newInterface
	mim.mutex.Unlock()
	
	// Record the switch
	switchEvent := InterfaceSwitch{
		FromInterface:  oldInterface,
		ToInterface:    newInterface,
		Timestamp:      time.Now(),
		Reason:         reason,
		TransferStatus: TransferStatusContinuous,
	}
	
	mim.mutex.Lock()
	mim.switchHistory = append(mim.switchHistory, switchEvent)
	if len(mim.switchHistory) > 100 {
		mim.switchHistory = mim.switchHistory[1:]
	}
	mim.mutex.Unlock()
	
	log.Infof("Switched network interface: %s -> %s (reason: %s)",
		oldInterface, newInterface, mim.switchReasonString(reason))
	
	// Notify active transfers about the switch
	if globalStreamingEngine != nil {
		go globalStreamingEngine.handleInterfaceSwitch(oldInterface, newInterface, reason)
	}
}

// interfaceTypeString returns a string representation of interface type
func (mim *MultiInterfaceManager) interfaceTypeString(t InterfaceType) string {
	switch t {
	case InterfaceEthernet:
		return "Ethernet"
	case InterfaceWiFi:
		return "WiFi"
	case InterfaceLTE:
		return "LTE"
	case InterfaceCellular:
		return "Cellular"
	case InterfaceVPN:
		return "VPN"
	default:
		return "Unknown"
	}
}

// switchReasonString returns a string representation of switch reason
func (mim *MultiInterfaceManager) switchReasonString(r SwitchReason) string {
	switch r {
	case SwitchReasonQualityDegradation:
		return "Quality Degradation"
	case SwitchReasonInterfaceDown:
		return "Interface Down"
	case SwitchReasonBetterAlternative:
		return "Better Alternative"
	case SwitchReasonManual:
		return "Manual"
	case SwitchReasonTimeout:
		return "Timeout"
	default:
		return "Unknown"
	}
}

// GetInterfaceInfo retrieves information about a specific network interface
func (mim *MultiInterfaceManager) GetInterfaceInfo(interfaceName string) *NetworkInterface {
	mim.mutex.RLock()
	defer mim.mutex.RUnlock()
	
	for _, iface := range mim.interfaces {
		if iface.Name == interfaceName {
			return iface
		}
	}
	return nil
}
