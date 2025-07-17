// integration.go - Integration layer to add new features without modifying core

package main

import (
	"net/http"
	"path/filepath"
	"time"
)

// InitializeUploadResilience initializes the upload resilience system
func InitializeUploadResilience() {
	// Initialize upload session store
	tempDir := filepath.Join(conf.Server.StoragePath, ".upload_sessions")
	uploadSessionStore = NewUploadSessionStore(tempDir)
	
	// Initialize network resilience
	InitializeNetworkResilience()
	
	log.Info("Upload resilience system initialized")
}

// EnhanceExistingRouter adds new routes without modifying existing setupRouter function
func EnhanceExistingRouter(mux *http.ServeMux) {
	// Add chunked upload endpoints
	mux.HandleFunc("/upload/chunked", ResilientHTTPHandler(handleChunkedUpload, networkManager))
	mux.HandleFunc("/upload/status", handleUploadStatus)
	
	// Wrap existing upload handlers with resilience (optional)
	if conf.Uploads.ChunkedUploadsEnabled {
		log.Info("Enhanced upload endpoints added:")
		log.Info("  POST/PUT /upload/chunked - Chunked/resumable uploads")
		log.Info("  GET /upload/status - Upload status check")
	}
}

// UpdateConfigurationDefaults suggests better defaults without forcing changes
func UpdateConfigurationDefaults() {
	log.Info("Network resilience recommendations:")
	
	// Log current settings vs recommended
	recommendations := map[string]string{
		"ReadTimeout":   "300s (current: " + conf.Timeouts.Read + ")",
		"WriteTimeout":  "300s (current: " + conf.Timeouts.Write + ")",
		"IdleTimeout":   "600s (current: " + conf.Timeouts.Idle + ")",
		"ChunkSize":     "5MB for mobile networks",
		"RetryAttempts": "3-5 for network switching scenarios",
	}
	
	log.Info("Recommended configuration changes for network switching resilience:")
	for setting, recommendation := range recommendations {
		log.Infof("  %s: %s", setting, recommendation)
	}
}

// MonitorUploadPerformance provides additional metrics without modifying existing metrics
func MonitorUploadPerformance() {
	ticker := time.NewTicker(60 * time.Second)
	defer ticker.Stop()
	
	for {
		select {
		case <-ticker.C:
			// Log upload session statistics
			if uploadSessionStore != nil {
				uploadSessionStore.mutex.RLock()
				activeSessionsCount := len(uploadSessionStore.sessions)
				uploadSessionStore.mutex.RUnlock()
				
				if activeSessionsCount > 0 {
					log.Infof("Active upload sessions: %d", activeSessionsCount)
				}
			}
			
			// Log network resilience status
			if networkManager != nil {
				networkManager.mutex.RLock()
				activeUploadsCount := len(networkManager.activeUploads)
				isPaused := networkManager.isPaused
				networkManager.mutex.RUnlock()
				
				if activeUploadsCount > 0 {
					status := "active"
					if isPaused {
						status = "paused"
					}
					log.Infof("Network resilience: %d uploads %s", activeUploadsCount, status)
				}
			}
		}
	}
}

// GetResilienceStatus returns current resilience system status (for monitoring)
func GetResilienceStatus() map[string]interface{} {
	status := map[string]interface{}{
		"upload_sessions_enabled": uploadSessionStore != nil,
		"network_monitoring":      networkManager != nil,
		"active_sessions":         0,
		"active_uploads":         0,
		"network_paused":         false,
	}
	
	if uploadSessionStore != nil {
		uploadSessionStore.mutex.RLock()
		status["active_sessions"] = len(uploadSessionStore.sessions)
		uploadSessionStore.mutex.RUnlock()
	}
	
	if networkManager != nil {
		networkManager.mutex.RLock()
		status["active_uploads"] = len(networkManager.activeUploads)
		status["network_paused"] = networkManager.isPaused
		networkManager.mutex.RUnlock()
	}
	
	return status
}

// Non-intrusive initialization function to be called from main()
func InitializeEnhancements() {
	// Only initialize if chunked uploads are enabled
	if conf.Uploads.ChunkedUploadsEnabled {
		InitializeUploadResilience()
		
		// Start performance monitoring
		go MonitorUploadPerformance()
		
		// Log configuration recommendations
		UpdateConfigurationDefaults()
	} else {
		log.Info("Chunked uploads disabled. Enable 'chunkeduploadsenabled = true' for network resilience features")
	}
}
