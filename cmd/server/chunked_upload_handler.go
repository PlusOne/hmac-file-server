// chunked_upload_handler.go - New chunked upload handler without modifying existing ones

package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// Global upload session store
var uploadSessionStore *UploadSessionStore

// handleChunkedUpload handles chunked/resumable uploads
func handleChunkedUpload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Only allow PUT and POST methods
	if r.Method != http.MethodPut && r.Method != http.MethodPost {
		http.Error(w, "Method not allowed for chunked uploads", http.StatusMethodNotAllowed)
		uploadErrorsTotal.Inc()
		return
	}

	// BASIC HMAC VALIDATION - same as original handleUpload
	if conf.Security.EnableJWT {
		_, err := validateJWTFromRequest(r, conf.Security.JWTSecret)
		if err != nil {
			http.Error(w, fmt.Sprintf("JWT Authentication failed: %v", err), http.StatusUnauthorized)
			uploadErrorsTotal.Inc()
			return
		}
		log.Debugf("JWT authentication successful for chunked upload: %s", r.URL.Path)
	} else {
		err := validateHMAC(r, conf.Security.Secret)
		if err != nil {
			http.Error(w, fmt.Sprintf("HMAC Authentication failed: %v", err), http.StatusUnauthorized)
			uploadErrorsTotal.Inc()
			return
		}
		log.Debugf("HMAC authentication successful for chunked upload: %s", r.URL.Path)
	}

	// Extract headers for chunked upload
	sessionID := r.Header.Get("X-Upload-Session-ID")
	chunkNumberStr := r.Header.Get("X-Chunk-Number")
	filename := r.Header.Get("X-Filename")

	// Handle session creation for new uploads
	if sessionID == "" {
		// This is a new upload session
		totalSizeStr := r.Header.Get("X-Total-Size")
		if totalSizeStr == "" || filename == "" {
			http.Error(w, "Missing required headers for new upload session", http.StatusBadRequest)
			uploadErrorsTotal.Inc()
			return
		}

		totalSize, err := strconv.ParseInt(totalSizeStr, 10, 64)
		if err != nil {
			http.Error(w, "Invalid total size", http.StatusBadRequest)
			uploadErrorsTotal.Inc()
			return
		}

		// Validate file size against max_upload_size if configured
		if conf.Server.MaxUploadSize != "" {
			maxSizeBytes, err := parseSize(conf.Server.MaxUploadSize)
			if err != nil {
				log.Errorf("Invalid max_upload_size configuration: %v", err)
				http.Error(w, "Server configuration error", http.StatusInternalServerError)
				uploadErrorsTotal.Inc()
				return
			}
			if totalSize > maxSizeBytes {
				http.Error(w, fmt.Sprintf("File size %s exceeds maximum allowed size %s", 
					formatBytes(totalSize), conf.Server.MaxUploadSize), http.StatusRequestEntityTooLarge)
				uploadErrorsTotal.Inc()
				return
			}
		}

		// Authentication (reuse existing logic)
		if conf.Security.EnableJWT {
			_, err := validateJWTFromRequest(r, conf.Security.JWTSecret)
			if err != nil {
				http.Error(w, fmt.Sprintf("JWT Authentication failed: %v", err), http.StatusUnauthorized)
				uploadErrorsTotal.Inc()
				return
			}
		} else {
			err := validateHMAC(r, conf.Security.Secret)
			if err != nil {
				http.Error(w, fmt.Sprintf("HMAC Authentication failed: %v", err), http.StatusUnauthorized)
				uploadErrorsTotal.Inc()
				return
			}
		}

		// Create new session
		clientIP := getClientIP(r)
		session := uploadSessionStore.CreateSession(filename, totalSize, clientIP)

		// Return session info
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusCreated)
		response := map[string]interface{}{
			"session_id":    session.ID,
			"chunk_size":    session.ChunkSize,
			"total_chunks":  (totalSize + session.ChunkSize - 1) / session.ChunkSize,
		}
		writeJSONResponse(w, response)
		return
	}

	// Handle chunk upload
	session, exists := uploadSessionStore.GetSession(sessionID)
	if !exists {
		http.Error(w, "Upload session not found", http.StatusNotFound)
		uploadErrorsTotal.Inc()
		return
	}

	// Parse chunk number
	chunkNumber, err := strconv.Atoi(chunkNumberStr)
	if err != nil {
		http.Error(w, "Invalid chunk number", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	// Check if chunk already uploaded
	if chunkInfo, exists := session.Chunks[chunkNumber]; exists && chunkInfo.Completed {
		w.WriteHeader(http.StatusOK)
		writeJSONResponse(w, map[string]interface{}{
			"message": "Chunk already uploaded",
			"chunk":   chunkNumber,
		})
		return
	}

	// Create chunk file
	chunkPath := filepath.Join(session.TempDir, fmt.Sprintf("chunk_%d", chunkNumber))
	chunkFile, err := os.Create(chunkPath)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error creating chunk file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}
	defer chunkFile.Close()

	// Copy chunk data with progress tracking
	log.Printf("DEBUG: Processing chunk %d for session %s (content-length: %d)", chunkNumber, sessionID, r.ContentLength)
	written, err := copyChunkWithResilience(chunkFile, r.Body, r.ContentLength, sessionID, chunkNumber)
	if err != nil {
		log.Printf("ERROR: Failed to save chunk %d for session %s: %v", chunkNumber, sessionID, err)
		http.Error(w, fmt.Sprintf("Error saving chunk: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		os.Remove(chunkPath) // Clean up failed chunk
		return
	}

	// Update session
	err = uploadSessionStore.UpdateSession(sessionID, chunkNumber, written)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error updating session: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}

	// Get updated session for completion check
	session, _ = uploadSessionStore.GetSession(sessionID)
	progress := float64(session.UploadedBytes) / float64(session.TotalSize)
	
	// Debug logging for large files
	if session.TotalSize > 50*1024*1024 { // Log for files > 50MB
		log.Debugf("Chunk %d uploaded for %s: %d/%d bytes (%.1f%%)", 
			chunkNumber, session.Filename, session.UploadedBytes, session.TotalSize, progress*100)
	}

	// Check if upload is complete
	isComplete := uploadSessionStore.IsSessionComplete(sessionID)
	log.Printf("DEBUG: Session %s completion check: %v (uploaded: %d, total: %d, progress: %.1f%%)", 
		sessionID, isComplete, session.UploadedBytes, session.TotalSize, progress*100)
	
	if isComplete {
		log.Printf("DEBUG: Starting file assembly for session %s", sessionID)
		// Assemble final file
		finalPath, err := uploadSessionStore.AssembleFile(sessionID)
		if err != nil {
			log.Printf("ERROR: File assembly failed for session %s: %v", sessionID, err)
			http.Error(w, fmt.Sprintf("Error assembling file: %v", err), http.StatusInternalServerError)
			uploadErrorsTotal.Inc()
			return
		}
		log.Printf("DEBUG: File assembly completed for session %s: %s", sessionID, finalPath)

		// Handle deduplication if enabled (reuse existing logic)
		if conf.Server.DeduplicationEnabled {
			// Note: This calls the existing deduplication function without modification
			err = handleDeduplication(r.Context(), finalPath)
			if err != nil {
				log.Warnf("Deduplication failed for %s: %v", finalPath, err)
			}
		}

		// Update metrics
		duration := time.Since(startTime)
		uploadDuration.Observe(duration.Seconds())
		uploadsTotal.Inc()
		uploadSizeBytes.Observe(float64(session.TotalSize))

		// Return success response
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		response := map[string]interface{}{
			"success":   true,
			"filename":  session.Filename,
			"size":      session.TotalSize,
			"duration":  duration.String(),
			"completed": true,
		}
		writeJSONResponse(w, response)
		
		log.Infof("Successfully completed chunked upload %s (%s) in %s", 
			session.Filename, formatBytes(session.TotalSize), duration)
	} else {
		// Return partial success
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		response := map[string]interface{}{
			"success":        true,
			"chunk":          chunkNumber,
			"uploaded_bytes": session.UploadedBytes,
			"total_size":     session.TotalSize,
			"progress":       float64(session.UploadedBytes) / float64(session.TotalSize),
			"completed":      false,
		}
		writeJSONResponse(w, response)
	}
}

// copyChunkWithResilience copies chunk data with network resilience
func copyChunkWithResilience(dst io.Writer, src io.Reader, contentLength int64, sessionID string, chunkNumber int) (int64, error) {
	// Register with network resilience manager if available
	var uploadCtx *UploadContext
	if networkManager != nil {
		uploadCtx = networkManager.RegisterUpload(fmt.Sprintf("%s_chunk_%d", sessionID, chunkNumber))
		defer networkManager.UnregisterUpload(fmt.Sprintf("%s_chunk_%d", sessionID, chunkNumber))
	}

	// Use buffered copying with pause/resume capability
	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)
	buf := *bufPtr

	var written int64
	for {
		// Check for pause signals
		if uploadCtx != nil {
			select {
			case <-uploadCtx.PauseChan:
				// Wait for resume signal
				<-uploadCtx.ResumeChan
			default:
				// Continue
			}
		}

		// Read chunk of data
		n, readErr := src.Read(buf)
		if n > 0 {
			// Write to destination
			w, writeErr := dst.Write(buf[:n])
			written += int64(w)
			if writeErr != nil {
				return written, writeErr
			}
		}

		if readErr != nil {
			if readErr == io.EOF {
				break
			}
			return written, readErr
		}
	}

	return written, nil
}

// handleUploadStatus returns the status of an upload session
func handleUploadStatus(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	sessionID := r.URL.Query().Get("session_id")
	if sessionID == "" {
		http.Error(w, "Missing session_id parameter", http.StatusBadRequest)
		return
	}

	session, exists := uploadSessionStore.GetSession(sessionID)
	if !exists {
		http.Error(w, "Session not found", http.StatusNotFound)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	response := map[string]interface{}{
		"session_id":     session.ID,
		"filename":       session.Filename,
		"total_size":     session.TotalSize,
		"uploaded_bytes": session.UploadedBytes,
		"progress":       float64(session.UploadedBytes) / float64(session.TotalSize),
		"completed":      uploadSessionStore.IsSessionComplete(sessionID),
		"last_activity":  session.LastActivity,
		"chunks":         len(session.Chunks),
	}
	writeJSONResponse(w, response)
}

// Helper functions
func getClientIP(r *http.Request) string {
	// Check X-Forwarded-For header first
	if xff := r.Header.Get("X-Forwarded-For"); xff != "" {
		parts := strings.Split(xff, ",")
		return strings.TrimSpace(parts[0])
	}
	
	// Check X-Real-IP header
	if xri := r.Header.Get("X-Real-IP"); xri != "" {
		return xri
	}
	
	// Fall back to remote address
	host, _, _ := strings.Cut(r.RemoteAddr, ":")
	return host
}

func writeJSONResponse(w http.ResponseWriter, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	if jsonBytes, err := json.Marshal(data); err == nil {
		w.Write(jsonBytes)
	} else {
		http.Error(w, "Error encoding JSON response", http.StatusInternalServerError)
	}
}
