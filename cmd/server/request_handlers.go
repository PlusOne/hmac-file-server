// request_handlers.go — HTTP handler functions for upload and download endpoints.
package main

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
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

// handleUpload handles POST multipart upload requests.
func handleUpload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Enhanced session handling for multi-upload scenarios (Gajim fix)
	sessionID := r.Header.Get("X-Session-ID")
	if sessionID == "" {
		// Generate session ID for multi-upload tracking
		sessionID = generateUploadSessionID("upload", r.Header.Get("User-Agent"), getClientIP(r))
	}

	// Set session headers for client continuation
	w.Header().Set("X-Session-ID", sessionID)
	w.Header().Set("X-Upload-Session-Timeout", "3600") // 1 hour

	// Only allow POST method
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		uploadErrorsTotal.Inc()
		return
	}

	// ENHANCED AUTHENTICATION with network switching support
	var bearerClaims *BearerTokenClaims
	authHeader := r.Header.Get("Authorization")

	if strings.HasPrefix(authHeader, "Bearer ") {
		// Bearer token authentication with session recovery for network switching
		// Store response writer in context for session headers
		ctx := context.WithValue(r.Context(), responseWriterKey, w)
		r = r.WithContext(ctx)

		claims, err := validateBearerTokenWithSession(r, conf.Security.Secret)
		if err != nil {
			// Enhanced error logging for network switching scenarios
			clientIP := getClientIP(r)
			userAgent := r.Header.Get("User-Agent")
			sessionID := getSessionIDFromRequest(r)
			log.Warnf("🔴 Authentication failed for IP %s, User-Agent: %s, Session: %s, Error: %v",
				clientIP, userAgent, sessionID, err)

			// Check if this might be a network switching scenario and provide helpful response
			if strings.Contains(err.Error(), "expired") || strings.Contains(err.Error(), "invalid") {
				w.Header().Set("X-Network-Switch-Detected", "true")
				w.Header().Set("X-Retry-After", "30") // Suggest retry after 30 seconds
				w.Header().Set("X-Session-Recovery", "available")
				if sessionID != "" {
					w.Header().Set("X-Session-ID", sessionID)
				}
			}

			AuditAuthFailure(r, "bearer_token", err.Error())
			http.Error(w, fmt.Sprintf("Bearer Token Authentication failed: %v", err), http.StatusUnauthorized)
			uploadErrorsTotal.Inc()
			return
		}
		AuditAuthSuccess(r, claims.User, "bearer_token")
		bearerClaims = claims
		log.Infof("✅ Bearer token authentication successful: user=%s, file=%s, IP=%s",
			claims.User, claims.Filename, getClientIP(r))

		// Add comprehensive response headers for audit logging and client tracking
		w.Header().Set("X-Authenticated-User", claims.User)
		w.Header().Set("X-Auth-Method", "Bearer-Token")
		w.Header().Set("X-Client-IP", getClientIP(r))
		w.Header().Set("X-Network-Switch-Support", "enabled")
	} else if conf.Security.EnableJWT {
		// JWT authentication
		_, err := validateJWTFromRequest(r, conf.Security.JWTSecret)
		if err != nil {
			log.Warnf("🔴 JWT Authentication failed for IP %s: %v", getClientIP(r), err)
			AuditAuthFailure(r, "jwt", err.Error())
			http.Error(w, fmt.Sprintf("JWT Authentication failed: %v", err), http.StatusUnauthorized)
			uploadErrorsTotal.Inc()
			return
		}
		AuditAuthSuccess(r, "", "jwt")
		log.Infof("✅ JWT authentication successful for upload request: %s", r.URL.Path)
		w.Header().Set("X-Auth-Method", "JWT")
	} else {
		// HMAC authentication with enhanced network switching support
		err := validateHMAC(r, conf.Security.Secret)
		if err != nil {
			log.Warnf("🔴 HMAC Authentication failed for IP %s: %v", getClientIP(r), err)
			AuditAuthFailure(r, "hmac", err.Error())
			http.Error(w, fmt.Sprintf("HMAC Authentication failed: %v", err), http.StatusUnauthorized)
			uploadErrorsTotal.Inc()
			return
		}
		AuditAuthSuccess(r, "", "hmac")
		log.Infof("✅ HMAC authentication successful for upload request: %s", r.URL.Path)
		w.Header().Set("X-Auth-Method", "HMAC")
	}

	// ENHANCED CLIENT MULTI-INTERFACE TRACKING with network switching detection
	var clientSession *ClientSession
	if clientTracker != nil && conf.ClientNetwork.SessionBasedTracking {
		// Enhanced session ID extraction from multiple sources
		sessionID := r.Header.Get("X-Upload-Session-ID")
		if sessionID == "" {
			sessionID = r.FormValue("session_id")
		}
		if sessionID == "" {
			sessionID = r.URL.Query().Get("session_id")
		}
		if sessionID == "" {
			// Generate new session ID with enhanced entropy
			sessionID = generateSessionID("", "")
		}

		clientIP := getClientIP(r)

		// Detect potential network switching
		xForwardedFor := r.Header.Get("X-Forwarded-For")
		xRealIP := r.Header.Get("X-Real-IP")
		networkSwitchIndicators := xForwardedFor != "" || xRealIP != ""

		if networkSwitchIndicators {
			log.Infof("🔄 Network switching indicators detected: session=%s, client_ip=%s, x_forwarded_for=%s, x_real_ip=%s",
				sessionID, clientIP, xForwardedFor, xRealIP)
			w.Header().Set("X-Network-Switch-Detected", "true")
		}

		clientSession = clientTracker.TrackClientSession(sessionID, clientIP, r)

		// Enhanced session response headers for client coordination
		w.Header().Set("X-Upload-Session-ID", sessionID)
		w.Header().Set("X-Session-IP-Count", fmt.Sprintf("%d", len(clientSession.ClientIPs)))
		w.Header().Set("X-Connection-Type", clientSession.ConnectionType)

		log.Infof("🔗 Client session tracking: %s from IP %s (connection: %s, total_ips: %d)",
			sessionID, clientIP, clientSession.ConnectionType, len(clientSession.ClientIPs))

		// Add user context for Bearer token authentication
		if bearerClaims != nil {
			log.Infof("👤 Session associated with XMPP user: %s", bearerClaims.User)
			w.Header().Set("X-XMPP-User", bearerClaims.User)
		}
	}

	// Parse multipart form with enhanced error handling
	err := r.ParseMultipartForm(32 << 20) // 32MB max memory
	if err != nil {
		log.Errorf("🔴 Error parsing multipart form from IP %s: %v", getClientIP(r), err)
		http.Error(w, fmt.Sprintf("Error parsing multipart form: %v", err), http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	// Get file from form with enhanced validation
	file, header, err := r.FormFile("file")
	if err != nil {
		log.Errorf("🔴 Error getting file from form (IP: %s): %v", getClientIP(r), err)
		http.Error(w, fmt.Sprintf("Error getting file from form: %v", err), http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}
	defer file.Close()

	// Get user JID for quota and audit tracking
	var userJID string
	if bearerClaims != nil {
		userJID = bearerClaims.User
	}
	r.Header.Set("X-User-JID", userJID)
	r.Header.Set("X-File-Name", header.Filename)

	// Check quota before upload
	if qm := GetQuotaManager(); qm != nil && qm.config.Enabled && userJID != "" {
		canUpload, _ := qm.CanUpload(r.Context(), userJID, header.Size)
		if !canUpload {
			used, limit, _ := qm.GetUsage(r.Context(), userJID)
			AuditQuotaExceeded(r, userJID, used, limit, header.Size)
			w.Header().Set("Content-Type", "application/json")
			w.Header().Set("X-Quota-Used", fmt.Sprintf("%d", used))
			w.Header().Set("X-Quota-Limit", fmt.Sprintf("%d", limit))
			w.WriteHeader(http.StatusRequestEntityTooLarge)
			_ = json.NewEncoder(w).Encode(map[string]interface{}{
				"error":     "quota_exceeded",
				"message":   "Storage quota exceeded",
				"used":      used,
				"limit":     limit,
				"requested": header.Size,
			})
			uploadErrorsTotal.Inc()
			return
		}
	}

	// Content type validation using magic bytes
	var fileReader io.Reader = file
	declaredContentType := header.Header.Get("Content-Type")
	detectedContentType := declaredContentType

	if validator := GetContentValidator(); validator != nil && validator.config.CheckMagicBytes {
		validatedReader, detected, validErr := validator.ValidateContent(file, declaredContentType, header.Size)
		if validErr != nil {
			if valErr, ok := validErr.(*ValidationError); ok {
				AuditValidationFailure(r, userJID, header.Filename, declaredContentType, detected, valErr.Code)
				WriteValidationError(w, valErr)
			} else {
				http.Error(w, validErr.Error(), http.StatusUnsupportedMediaType)
			}
			uploadErrorsTotal.Inc()
			return
		}
		fileReader = validatedReader
		detectedContentType = detected
	}

	// Validate file size against max_upload_size if configured
	if conf.Server.MaxUploadSize != "" {
		maxSizeBytes, err := parseSize(conf.Server.MaxUploadSize)
		if err != nil {
			log.Errorf("🔴 Invalid max_upload_size configuration: %v", err)
			http.Error(w, "Server configuration error", http.StatusInternalServerError)
			uploadErrorsTotal.Inc()
			return
		}
		if header.Size > maxSizeBytes {
			log.Warnf("⚠️  File size %s exceeds maximum allowed size %s (IP: %s)",
				formatBytes(header.Size), conf.Server.MaxUploadSize, getClientIP(r))
			http.Error(w, fmt.Sprintf("File size %s exceeds maximum allowed size %s",
				formatBytes(header.Size), conf.Server.MaxUploadSize), http.StatusRequestEntityTooLarge)
			uploadErrorsTotal.Inc()
			return
		}
	}

	// Validate file extension if configured
	if len(conf.Uploads.AllowedExtensions) > 0 {
		ext := strings.ToLower(filepath.Ext(header.Filename))
		allowed := false
		for _, allowedExt := range conf.Uploads.AllowedExtensions {
			if ext == allowedExt {
				allowed = true
				break
			}
		}
		if !allowed {
			log.Warnf("⚠️  File extension %s not allowed (IP: %s, file: %s)", ext, getClientIP(r), header.Filename)
			http.Error(w, fmt.Sprintf("File extension %s not allowed", ext), http.StatusBadRequest)
			uploadErrorsTotal.Inc()
			return
		}
	}

	// Generate filename based on configuration
	var filename string
	switch conf.Server.FileNaming {
	case "HMAC":
		// Generate HMAC-based filename with enhanced entropy
		h := hmac.New(sha256.New, []byte(conf.Security.Secret))
		h.Write([]byte(header.Filename + time.Now().String() + getClientIP(r)))
		filename = hex.EncodeToString(h.Sum(nil)) + filepath.Ext(header.Filename)
	default: // "original" or "None"
		filename = header.Filename
	}

	// Create full file path
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	absFilename := filepath.Join(storagePath, filename)

	// Pre-upload deduplication check: if file already exists and deduplication is enabled, return success immediately
	if conf.Server.DeduplicationEnabled {
		if existingFileInfo, err := os.Stat(absFilename); err == nil {
			// File already exists - return success immediately for deduplication hit
			duration := time.Since(startTime)
			uploadDuration.Observe(duration.Seconds())
			uploadsTotal.Inc()
			uploadSizeBytes.Observe(float64(existingFileInfo.Size()))
			filesDeduplicatedTotal.Inc()

			w.Header().Set("Content-Type", "application/json")
			w.Header().Set("X-Deduplication-Hit", "true")
			w.WriteHeader(http.StatusOK)
			response := map[string]interface{}{
				"success":     true,
				"filename":    filename,
				"size":        existingFileInfo.Size(),
				"message":     "File already exists (deduplication hit)",
				"upload_time": duration.String(),
			}
			_ = json.NewEncoder(w).Encode(response)

			log.Infof("💾 Deduplication hit: file %s already exists (%s), returning success immediately (IP: %s)",
				filename, formatBytes(existingFileInfo.Size()), getClientIP(r))
			return
		}
	}

	// Create the file with enhanced error handling
	dst, err := os.Create(absFilename)
	if err != nil {
		log.Errorf("🔴 Error creating file %s (IP: %s): %v", absFilename, getClientIP(r), err)
		http.Error(w, fmt.Sprintf("Error creating file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}
	defer dst.Close()

	// Register upload with network resilience manager for WLAN/5G switching support
	var uploadCtx *UploadContext
	var networkSessionID string
	if networkManager != nil {
		networkSessionID = r.Header.Get("X-Upload-Session-ID")
		if networkSessionID == "" {
			networkSessionID = fmt.Sprintf("upload_%s_%d", getClientIP(r), time.Now().UnixNano())
		}
		uploadCtx = networkManager.RegisterUpload(networkSessionID)
		defer networkManager.UnregisterUpload(networkSessionID)
		log.Infof("🌐 Registered upload with network resilience: session=%s, IP=%s", networkSessionID, getClientIP(r))

		// Add network resilience headers
		w.Header().Set("X-Network-Resilience", "enabled")
		w.Header().Set("X-Upload-Context-ID", networkSessionID)
	}

	// Copy file content with network resilience support and enhanced progress tracking
	// Use fileReader which may be wrapped with content validation
	written, err := copyWithNetworkResilience(dst, fileReader, uploadCtx)
	if err != nil {
		log.Errorf("🔴 Error saving file %s (IP: %s, session: %s): %v", filename, getClientIP(r), sessionID, err)
		http.Error(w, fmt.Sprintf("Error saving file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		// Clean up partial file
		os.Remove(absFilename)
		// Audit the failure
		AuditUploadFailure(r, userJID, header.Filename, header.Size, err.Error())
		return
	}

	// Update quota after successful upload
	if qm := GetQuotaManager(); qm != nil && qm.config.Enabled && userJID != "" {
		if err := qm.RecordUpload(r.Context(), userJID, absFilename, written); err != nil {
			log.Warnf("⚠️  Failed to update quota for user %s: %v", userJID, err)
		}
	}

	// Audit successful upload
	AuditUploadSuccess(r, userJID, filename, written, detectedContentType)

	// CRITICAL FIX: Send immediate success response for large files (>1GB)
	// This prevents client timeouts while server does post-processing
	isLargeFile := header.Size > 1024*1024*1024 // 1GB threshold

	if isLargeFile {
		log.Infof("🚀 Large file detected (%s), sending immediate success response", formatBytes(header.Size))

		// Send immediate success response to client
		duration := time.Since(startTime)
		uploadDuration.Observe(duration.Seconds())
		uploadsTotal.Inc()
		uploadSizeBytes.Observe(float64(written))

		w.Header().Set("Content-Type", "application/json")
		w.Header().Set("X-Upload-Success", "true")
		w.Header().Set("X-Upload-Duration", duration.String())
		w.Header().Set("X-Large-File-Processing", "async")
		w.Header().Set("X-Post-Processing", "background")
		w.WriteHeader(http.StatusOK)

		response := map[string]interface{}{
			"success":         true,
			"filename":        filename,
			"size":            written,
			"duration":        duration.String(),
			"client_ip":       getClientIP(r),
			"timestamp":       time.Now().Unix(),
			"post_processing": "background",
		}

		// Add session information if available
		if clientSession != nil {
			response["session_id"] = clientSession.SessionID
			response["connection_type"] = clientSession.ConnectionType
			response["ip_count"] = len(clientSession.ClientIPs)
		}

		// Add user information if available
		if bearerClaims != nil {
			response["user"] = bearerClaims.User
		}

		// Send response immediately
		if jsonBytes, err := json.Marshal(response); err == nil {
			_, _ = w.Write(jsonBytes)
		} else {
			fmt.Fprintf(w, `{"success": true, "filename": "%s", "size": %d, "post_processing": "background"}`, filename, written)
		}

		log.Infof("✅ Immediate response sent for large file %s (%s) in %s from IP %s",
			filename, formatBytes(written), duration, getClientIP(r))

		// Process deduplication asynchronously for large files
		go func() {
			if conf.Server.DeduplicationEnabled {
				log.Infof("🔄 Starting background deduplication for large file: %s", filename)
				ctx := context.Background()
				err := handleDeduplication(ctx, absFilename)
				if err != nil {
					log.Warnf("⚠️  Background deduplication failed for %s: %v", absFilename, err)
				} else {
					log.Infof("✅ Background deduplication completed for %s", filename)
				}
			}

			// Add to scan queue for virus scanning if enabled
			if conf.ClamAV.ClamAVEnabled && len(conf.ClamAV.ScanFileExtensions) > 0 {
				ext := strings.ToLower(filepath.Ext(header.Filename))
				shouldScan := false
				for _, scanExt := range conf.ClamAV.ScanFileExtensions {
					if ext == strings.ToLower(scanExt) {
						shouldScan = true
						break
					}
				}
				if shouldScan {
					log.Infof("🔍 Starting background virus scan for large file: %s", filename)
					err := scanFileWithClamAV(absFilename)
					if err != nil {
						log.Warnf("⚠️  Background virus scan failed for %s: %v", filename, err)
					} else {
						log.Infof("✅ Background virus scan completed for %s", filename)
					}
				}
			}
		}()

		return
	}

	// Standard processing for small files (synchronous)
	// Handle deduplication if enabled
	if conf.Server.DeduplicationEnabled {
		ctx := context.Background()
		err = handleDeduplication(ctx, absFilename)
		if err != nil {
			log.Warnf("⚠️  Deduplication failed for %s (IP: %s): %v", absFilename, getClientIP(r), err)
		} else {
			log.Debugf("💾 Deduplication processed for %s", absFilename)
		}
	}

	// Update metrics
	duration := time.Since(startTime)
	uploadDuration.Observe(duration.Seconds())
	uploadsTotal.Inc()
	uploadSizeBytes.Observe(float64(written))

	// Enhanced success response with comprehensive metadata
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("X-Upload-Success", "true")
	w.Header().Set("X-Upload-Duration", duration.String())
	w.WriteHeader(http.StatusOK)

	response := map[string]interface{}{
		"success":   true,
		"filename":  filename,
		"size":      written,
		"duration":  duration.String(),
		"client_ip": getClientIP(r),
		"timestamp": time.Now().Unix(),
	}

	// Add session information if available
	if clientSession != nil {
		response["session_id"] = clientSession.SessionID
		response["connection_type"] = clientSession.ConnectionType
		response["ip_count"] = len(clientSession.ClientIPs)
	}

	// Add user information if available
	if bearerClaims != nil {
		response["user"] = bearerClaims.User
	}

	// Create JSON response
	if jsonBytes, err := json.Marshal(response); err == nil {
		_, _ = w.Write(jsonBytes)
	} else {
		fmt.Fprintf(w, `{"success": true, "filename": "%s", "size": %d}`, filename, written)
	}

	log.Infof("✅ Successfully uploaded %s (%s) in %s from IP %s (session: %s)",
		filename, formatBytes(written), duration, getClientIP(r), sessionID)
}

// handleDownload handles file downloads.
// ENHANCED FOR 100% WIFI ↔ LTE SWITCHING AND STANDBY RECOVERY RELIABILITY
func handleDownload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Enhanced Authentication with network switching tolerance
	if conf.Security.EnableJWT {
		_, err := validateJWTFromRequest(r, conf.Security.JWTSecret)
		if err != nil {
			log.Warnf("🔴 JWT Authentication failed for download from IP %s: %v", getClientIP(r), err)
			AuditAuthFailure(r, "jwt", err.Error())
			http.Error(w, fmt.Sprintf("JWT Authentication failed: %v", err), http.StatusUnauthorized)
			downloadErrorsTotal.Inc()
			return
		}
		AuditAuthSuccess(r, "", "jwt")
		log.Infof("✅ JWT authentication successful for download request: %s", r.URL.Path)
		w.Header().Set("X-Auth-Method", "JWT")
	} else {
		err := validateHMAC(r, conf.Security.Secret)
		if err != nil {
			log.Warnf("🔴 HMAC Authentication failed for download from IP %s: %v", getClientIP(r), err)
			AuditAuthFailure(r, "hmac", err.Error())
			http.Error(w, fmt.Sprintf("HMAC Authentication failed: %v", err), http.StatusUnauthorized)
			downloadErrorsTotal.Inc()
			return
		}
		AuditAuthSuccess(r, "", "hmac")
		log.Infof("✅ HMAC authentication successful for download request: %s", r.URL.Path)
		w.Header().Set("X-Auth-Method", "HMAC")
	}

	// Extract filename with enhanced path handling
	filename := strings.TrimPrefix(r.URL.Path, "/download/")
	if filename == "" {
		log.Warnf("⚠️  No filename specified in download request from IP %s", getClientIP(r))
		http.Error(w, "Filename not specified", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	// Enhanced file path validation and construction
	var absFilename string
	var err error

	// Use storage path or ISO mount point
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	absFilename, err = sanitizeFilePath(storagePath, filename)
	if err != nil {
		log.Warnf("🔴 Invalid file path requested from IP %s: %s, error: %v", getClientIP(r), filename, err)
		http.Error(w, fmt.Sprintf("Invalid file path: %v", err), http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	// Enhanced file existence and accessibility check
	fileInfo, err := os.Stat(absFilename)
	if os.IsNotExist(err) {
		log.Warnf("🔴 File not found: %s (requested by IP %s)", absFilename, getClientIP(r))

		// Enhanced 404 response with network switching hints
		w.Header().Set("X-File-Not-Found", "true")
		w.Header().Set("X-Client-IP", getClientIP(r))
		w.Header().Set("X-Network-Switch-Support", "enabled")

		// Check if this might be a network switching issue
		userAgent := r.Header.Get("User-Agent")
		isMobileXMPP := strings.Contains(strings.ToLower(userAgent), "conversations") ||
			strings.Contains(strings.ToLower(userAgent), "dino") ||
			strings.Contains(strings.ToLower(userAgent), "gajim") ||
			strings.Contains(strings.ToLower(userAgent), "android") ||
			strings.Contains(strings.ToLower(userAgent), "mobile") ||
			strings.Contains(strings.ToLower(userAgent), "xmpp")

		if isMobileXMPP {
			w.Header().Set("X-Mobile-Client-Detected", "true")
			w.Header().Set("X-Retry-Suggestion", "30") // Suggest retry after 30 seconds
			log.Infof("📱 Mobile XMPP client file not found - may be network switching issue: %s", userAgent)
		}

		http.Error(w, "File not found", http.StatusNotFound)
		downloadErrorsTotal.Inc()
		return
	}
	if err != nil {
		log.Errorf("🔴 Error accessing file %s from IP %s: %v", absFilename, getClientIP(r), err)
		http.Error(w, fmt.Sprintf("Error accessing file: %v", err), http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}

	if fileInfo.IsDir() {
		log.Warnf("⚠️  Attempt to download directory %s from IP %s", absFilename, getClientIP(r))
		http.Error(w, "Cannot download a directory", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	// Enhanced file opening with retry logic for network switching scenarios
	var file *os.File
	maxRetries := 3
	for attempt := 1; attempt <= maxRetries; attempt++ {
		file, err = os.Open(absFilename)
		if err == nil {
			break
		}

		if attempt < maxRetries {
			log.Warnf("⚠️  Attempt %d/%d: Error opening file %s from IP %s: %v (retrying...)",
				attempt, maxRetries, absFilename, getClientIP(r), err)
			time.Sleep(time.Duration(attempt) * time.Second) // Progressive backoff
		} else {
			log.Errorf("🔴 Failed to open file %s after %d attempts from IP %s: %v",
				absFilename, maxRetries, getClientIP(r), err)
			http.Error(w, fmt.Sprintf("Error opening file: %v", err), http.StatusInternalServerError)
			downloadErrorsTotal.Inc()
			return
		}
	}
	defer file.Close()

	// Enhanced response headers with network switching support
	w.Header().Set("Content-Disposition", "attachment; filename=\""+filepath.Base(absFilename)+"\"")
	w.Header().Set("Content-Type", "application/octet-stream")
	w.Header().Set("Content-Length", fmt.Sprintf("%d", fileInfo.Size()))
	w.Header().Set("X-Client-IP", getClientIP(r))
	w.Header().Set("X-Network-Switch-Support", "enabled")
	w.Header().Set("X-File-Path", filename)
	w.Header().Set("X-Download-Start-Time", fmt.Sprintf("%d", time.Now().Unix()))

	// Add cache control headers for mobile network optimization
	userAgent := r.Header.Get("User-Agent")
	isMobileXMPP := strings.Contains(strings.ToLower(userAgent), "conversations") ||
		strings.Contains(strings.ToLower(userAgent), "dino") ||
		strings.Contains(strings.ToLower(userAgent), "gajim") ||
		strings.Contains(strings.ToLower(userAgent), "android") ||
		strings.Contains(strings.ToLower(userAgent), "mobile") ||
		strings.Contains(strings.ToLower(userAgent), "xmpp")

	if isMobileXMPP {
		w.Header().Set("X-Mobile-Client-Detected", "true")
		w.Header().Set("Cache-Control", "public, max-age=86400") // 24 hours cache for mobile
		w.Header().Set("X-Mobile-Optimized", "true")
		log.Infof("📱 Mobile XMPP client download detected, applying mobile optimizations")
	}

	// Enhanced file transfer with buffered copy and progress tracking
	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)
	buf := *bufPtr

	// Track download progress for large files
	if fileInfo.Size() > 10*1024*1024 { // Log progress for files > 10MB
		log.Infof("📥 Starting download of %s (%.1f MiB) for IP %s",
			filepath.Base(absFilename), float64(fileInfo.Size())/(1024*1024), getClientIP(r))
	}

	// Enhanced copy with network resilience
	n, err := copyWithProgressTracking(w, file, buf, fileInfo.Size(), getClientIP(r))
	if err != nil {
		log.Errorf("🔴 Error during download of %s for IP %s: %v", absFilename, getClientIP(r), err)
		// Don't write http.Error here if headers already sent
		downloadErrorsTotal.Inc()
		return
	}

	// Update metrics and log success
	duration := time.Since(startTime)
	downloadDuration.Observe(duration.Seconds())
	downloadsTotal.Inc()
	downloadSizeBytes.Observe(float64(n))

	// Audit successful download
	AuditDownloadSuccess(r, "", filepath.Base(absFilename), n)

	log.Infof("✅ Successfully downloaded %s (%s) in %s for IP %s (session complete)",
		filepath.Base(absFilename), formatBytes(n), duration, getClientIP(r))
}

// handleV3Upload handles PUT requests for v3 protocol (mod_http_upload_external).
func handleV3Upload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Only allow PUT method for v3
	if r.Method != http.MethodPut {
		http.Error(w, "Method not allowed for v3 uploads", http.StatusMethodNotAllowed)
		uploadErrorsTotal.Inc()
		return
	}

	// Validate v3 HMAC signature
	err := validateV3HMAC(r, conf.Security.Secret)
	if err != nil {
		http.Error(w, fmt.Sprintf("v3 Authentication failed: %v", err), http.StatusUnauthorized)
		uploadErrorsTotal.Inc()
		return
	}
	log.Debugf("v3 HMAC authentication successful for upload request: %s", r.URL.Path)

	// Extract filename from the URL path
	// Path format: /uuid/subdir/filename.ext
	pathParts := strings.Split(strings.Trim(r.URL.Path, "/"), "/")
	if len(pathParts) < 1 {
		http.Error(w, "Invalid upload path", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	// Use the last part as filename
	originalFilename := pathParts[len(pathParts)-1]
	if originalFilename == "" {
		http.Error(w, "No filename specified", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	// Validate file extension if configured
	if len(conf.Uploads.AllowedExtensions) > 0 {
		ext := strings.ToLower(filepath.Ext(originalFilename))
		allowed := false
		for _, allowedExt := range conf.Uploads.AllowedExtensions {
			if ext == allowedExt {
				allowed = true
				break
			}
		}
		if !allowed {
			http.Error(w, fmt.Sprintf("File extension %s not allowed", ext), http.StatusBadRequest)
			uploadErrorsTotal.Inc()
			return
		}
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
		if r.ContentLength > maxSizeBytes {
			http.Error(w, fmt.Sprintf("File size %s exceeds maximum allowed size %s",
				formatBytes(r.ContentLength), conf.Server.MaxUploadSize), http.StatusRequestEntityTooLarge)
			uploadErrorsTotal.Inc()
			return
		}
	}

	// Generate filename based on configuration
	var filename string
	switch conf.Server.FileNaming {
	case "HMAC":
		// Generate HMAC-based filename
		h := hmac.New(sha256.New, []byte(conf.Security.Secret))
		h.Write([]byte(originalFilename + time.Now().String()))
		filename = hex.EncodeToString(h.Sum(nil)) + filepath.Ext(originalFilename)
	default: // "original" or "None"
		filename = originalFilename
	}

	// Create full file path
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	absFilename := filepath.Join(storagePath, filename)

	// Pre-upload deduplication check: if file already exists and deduplication is enabled, return success immediately
	if conf.Server.DeduplicationEnabled {
		if existingFileInfo, err := os.Stat(absFilename); err == nil {
			// File already exists - return success immediately for deduplication hit
			duration := time.Since(startTime)
			uploadDuration.Observe(duration.Seconds())
			uploadsTotal.Inc()
			uploadSizeBytes.Observe(float64(existingFileInfo.Size()))
			filesDeduplicatedTotal.Inc()

			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusOK)
			response := map[string]interface{}{
				"success":  true,
				"filename": filename,
				"size":     existingFileInfo.Size(),
				"message":  "File already exists (deduplication hit)",
			}
			_ = json.NewEncoder(w).Encode(response)

			log.Infof("Deduplication hit: file %s already exists (%s), returning success immediately",
				filename, formatBytes(existingFileInfo.Size()))
			return
		}
	}

	// Create the file
	dst, err := os.Create(absFilename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error creating file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}
	defer dst.Close()

	// Copy file content from request body
	written, err := io.Copy(dst, r.Body)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error saving file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		// Clean up partial file
		os.Remove(absFilename)
		return
	}

	// CRITICAL FIX: Send immediate success response for large files (>1GB)
	// This prevents client timeouts while server does post-processing
	isLargeFile := written > 1024*1024*1024 // 1GB threshold

	if isLargeFile {
		log.Infof("🚀 Large file detected (%s), sending immediate success response (v3)", formatBytes(written))

		// Send immediate success response to client
		duration := time.Since(startTime)
		uploadDuration.Observe(duration.Seconds())
		uploadsTotal.Inc()
		uploadSizeBytes.Observe(float64(written))

		w.Header().Set("Content-Type", "application/json")
		w.Header().Set("X-Upload-Success", "true")
		w.Header().Set("X-Upload-Duration", duration.String())
		w.Header().Set("X-Large-File-Processing", "async")
		w.Header().Set("X-Post-Processing", "background")
		w.WriteHeader(http.StatusOK)

		response := map[string]interface{}{
			"success":         true,
			"filename":        filename,
			"size":            written,
			"duration":        duration.String(),
			"protocol":        "v3",
			"post_processing": "background",
		}

		// Send response immediately
		if jsonBytes, err := json.Marshal(response); err == nil {
			_, _ = w.Write(jsonBytes)
		} else {
			fmt.Fprintf(w, `{"success": true, "filename": "%s", "size": %d, "post_processing": "background"}`, filename, written)
		}

		log.Infof("✅ Immediate response sent for large file %s (%s) in %s via v3 protocol",
			filename, formatBytes(written), duration)

		// Process deduplication asynchronously for large files
		go func() {
			if conf.Server.DeduplicationEnabled {
				log.Infof("🔄 Starting background deduplication for large file (v3): %s", filename)
				ctx := context.Background()
				err := handleDeduplication(ctx, absFilename)
				if err != nil {
					log.Warnf("⚠️  Background deduplication failed for %s: %v", absFilename, err)
				} else {
					log.Infof("✅ Background deduplication completed for %s (v3)", filename)
				}
			}

			// Add to scan queue for virus scanning if enabled
			if conf.ClamAV.ClamAVEnabled && len(conf.ClamAV.ScanFileExtensions) > 0 {
				ext := strings.ToLower(filepath.Ext(originalFilename))
				shouldScan := false
				for _, scanExt := range conf.ClamAV.ScanFileExtensions {
					if ext == strings.ToLower(scanExt) {
						shouldScan = true
						break
					}
				}
				if shouldScan {
					log.Infof("🔍 Starting background virus scan for large file (v3): %s", filename)
					err := scanFileWithClamAV(absFilename)
					if err != nil {
						log.Warnf("⚠️  Background virus scan failed for %s: %v", filename, err)
					} else {
						log.Infof("✅ Background virus scan completed for %s (v3)", filename)
					}
				}
			}
		}()

		return
	}

	// Standard processing for small files (synchronous)
	// Handle deduplication if enabled
	if conf.Server.DeduplicationEnabled {
		ctx := context.Background()
		err = handleDeduplication(ctx, absFilename)
		if err != nil {
			log.Warnf("Deduplication failed for %s: %v", absFilename, err)
		}
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

	// Create JSON response
	if jsonBytes, err := json.Marshal(response); err == nil {
		_, _ = w.Write(jsonBytes)
	} else {
		fmt.Fprintf(w, `{"success": true, "filename": "%s", "size": %d}`, filename, written)
	}

	log.Infof("Successfully uploaded %s via v3 protocol (%s) in %s", filename, formatBytes(written), duration)
}

// handleLegacyUpload handles PUT requests for legacy protocols (v, v2, token).
// Debug artifacts removed.
func handleLegacyUpload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Enhanced session handling for multi-upload scenarios (Gajim XMPP fix)
	sessionID := r.Header.Get("X-Session-ID")
	if sessionID == "" {
		// Generate session ID for XMPP multi-upload tracking
		sessionID = generateUploadSessionID("legacy", r.Header.Get("User-Agent"), getClientIP(r))
	}

	// Set session headers for XMPP client continuation
	w.Header().Set("X-Session-ID", sessionID)
	w.Header().Set("X-Upload-Session-Timeout", "3600") // 1 hour
	w.Header().Set("X-Upload-Type", "legacy-xmpp")

	log.Debugf("handleLegacyUpload: Processing request to %s with query: %s", r.URL.Path, r.URL.RawQuery)

	// Only allow PUT method for legacy uploads
	if r.Method != http.MethodPut {
		http.Error(w, "Method not allowed for legacy uploads", http.StatusMethodNotAllowed)
		uploadErrorsTotal.Inc()
		return
	}

	// Validate legacy HMAC signature
	err := validateHMAC(r, conf.Security.Secret)
	if err != nil {
		http.Error(w, fmt.Sprintf("Legacy Authentication failed: %v", err), http.StatusUnauthorized)
		uploadErrorsTotal.Inc()
		return
	}

	log.Debugf("HMAC validation passed for: %s", r.URL.Path)

	// Extract filename from the URL path
	fileStorePath := strings.TrimPrefix(r.URL.Path, "/")
	if fileStorePath == "" {
		log.Debugf("No filename specified")
		http.Error(w, "No filename specified", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}

	log.Debugf("File path extracted: %s", fileStorePath)

	// Validate file extension if configured
	if len(conf.Uploads.AllowedExtensions) > 0 {
		ext := strings.ToLower(filepath.Ext(fileStorePath))
		allowed := false
		for _, allowedExt := range conf.Uploads.AllowedExtensions {
			if ext == allowedExt {
				allowed = true
				break
			}
		}
		if !allowed {
			http.Error(w, fmt.Sprintf("File extension %s not allowed", ext), http.StatusBadRequest)
			uploadErrorsTotal.Inc()
			return
		}
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
		if r.ContentLength > maxSizeBytes {
			http.Error(w, fmt.Sprintf("File size %s exceeds maximum allowed size %s",
				formatBytes(r.ContentLength), conf.Server.MaxUploadSize), http.StatusRequestEntityTooLarge)
			uploadErrorsTotal.Inc()
			return
		}
	}

	// Create full file path
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	// Generate filename based on configuration
	var absFilename string
	var filename string
	switch conf.Server.FileNaming {
	case "HMAC":
		// Generate HMAC-based filename
		h := hmac.New(sha256.New, []byte(conf.Security.Secret))
		h.Write([]byte(fileStorePath + time.Now().String()))
		filename = hex.EncodeToString(h.Sum(nil)) + filepath.Ext(fileStorePath)
		absFilename = filepath.Join(storagePath, filename)
	default: // "original" or "None"
		// Preserve full directory structure for legacy XMPP compatibility
		var sanitizeErr error
		absFilename, sanitizeErr = sanitizeFilePath(storagePath, fileStorePath)
		if sanitizeErr != nil {
			http.Error(w, fmt.Sprintf("Invalid file path: %v", sanitizeErr), http.StatusBadRequest)
			uploadErrorsTotal.Inc()
			return
		}
		filename = filepath.Base(fileStorePath) // For logging purposes
	}

	// Create directory structure if it doesn't exist
	if err := os.MkdirAll(filepath.Dir(absFilename), 0755); err != nil {
		http.Error(w, fmt.Sprintf("Error creating directory: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}

	// Pre-upload deduplication check: if file already exists and deduplication is enabled, return success immediately
	if conf.Server.DeduplicationEnabled {
		if existingFileInfo, err := os.Stat(absFilename); err == nil {
			// File already exists - return success immediately for deduplication hit
			duration := time.Since(startTime)
			uploadDuration.Observe(duration.Seconds())
			uploadsTotal.Inc()
			uploadSizeBytes.Observe(float64(existingFileInfo.Size()))
			filesDeduplicatedTotal.Inc()

			w.WriteHeader(http.StatusCreated) // 201 Created for legacy compatibility
			log.Infof("Deduplication hit: file %s already exists (%s), returning success immediately",
				filename, formatBytes(existingFileInfo.Size()))
			return
		}
	}

	// Create the file
	dst, err := os.Create(absFilename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error creating file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		return
	}
	defer dst.Close()

	// Log upload start for large files
	if r.ContentLength > 10*1024*1024 { // Log for files > 10MB
		log.Infof("Starting upload of %s (%.1f MiB)", filename, float64(r.ContentLength)/(1024*1024))
	}

	// Copy file content from request body with progress reporting
	written, err := copyWithProgress(dst, r.Body, r.ContentLength, filename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error saving file: %v", err), http.StatusInternalServerError)
		uploadErrorsTotal.Inc()
		// Clean up partial file
		os.Remove(absFilename)
		return
	}

	// CRITICAL FIX: Send immediate success response for large files (>1GB)
	// This prevents client timeouts while server does post-processing
	isLargeFile := written > 1024*1024*1024 // 1GB threshold

	if isLargeFile {
		log.Infof("🚀 Large file detected (%s), sending immediate success response (legacy)", formatBytes(written))

		// Send immediate success response to client
		duration := time.Since(startTime)
		uploadDuration.Observe(duration.Seconds())
		uploadsTotal.Inc()
		uploadSizeBytes.Observe(float64(written))

		// Return success response (201 Created for legacy compatibility)
		w.Header().Set("X-Upload-Success", "true")
		w.Header().Set("X-Upload-Duration", duration.String())
		w.Header().Set("X-Large-File-Processing", "async")
		w.Header().Set("X-Post-Processing", "background")
		w.WriteHeader(http.StatusCreated)

		log.Infof("✅ Immediate response sent for large file %s (%s) in %s via legacy protocol",
			filename, formatBytes(written), duration)

		// Process deduplication asynchronously for large files
		go func() {
			if conf.Server.DeduplicationEnabled {
				log.Infof("🔄 Starting background deduplication for large file (legacy): %s", filename)
				ctx := context.Background()
				err := handleDeduplication(ctx, absFilename)
				if err != nil {
					log.Warnf("⚠️  Background deduplication failed for %s: %v", absFilename, err)
				} else {
					log.Infof("✅ Background deduplication completed for %s (legacy)", filename)
				}
			}

			// Add to scan queue for virus scanning if enabled
			if conf.ClamAV.ClamAVEnabled && len(conf.ClamAV.ScanFileExtensions) > 0 {
				ext := strings.ToLower(filepath.Ext(fileStorePath))
				shouldScan := false
				for _, scanExt := range conf.ClamAV.ScanFileExtensions {
					if ext == strings.ToLower(scanExt) {
						shouldScan = true
						break
					}
				}
				if shouldScan {
					log.Infof("🔍 Starting background virus scan for large file (legacy): %s", filename)
					err := scanFileWithClamAV(absFilename)
					if err != nil {
						log.Warnf("⚠️  Background virus scan failed for %s: %v", filename, err)
					} else {
						log.Infof("✅ Background virus scan completed for %s (legacy)", filename)
					}
				}
			}
		}()

		return
	}

	// Standard processing for small files (synchronous)
	// Handle deduplication if enabled
	if conf.Server.DeduplicationEnabled {
		ctx := context.Background()
		err = handleDeduplication(ctx, absFilename)
		if err != nil {
			log.Warnf("Deduplication failed for %s: %v", absFilename, err)
		}
	}

	// Update metrics
	duration := time.Since(startTime)
	uploadDuration.Observe(duration.Seconds())
	uploadsTotal.Inc()
	uploadSizeBytes.Observe(float64(written))

	// Return success response (201 Created for legacy compatibility)
	w.WriteHeader(http.StatusCreated)

	log.Infof("Successfully uploaded %s via legacy protocol (%s) in %s", filename, formatBytes(written), duration)
}

// handleLegacyDownload handles GET/HEAD requests for legacy downloads.
func handleLegacyDownload(w http.ResponseWriter, r *http.Request) {
	startTime := time.Now()
	activeConnections.Inc()
	defer activeConnections.Dec()

	// Extract filename from the URL path
	fileStorePath := strings.TrimPrefix(r.URL.Path, "/")
	if fileStorePath == "" {
		http.Error(w, "No filename specified", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	// Create full file path
	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	absFilename := filepath.Join(storagePath, fileStorePath)

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

	if fileInfo.IsDir() {
		http.Error(w, "Cannot download a directory", http.StatusBadRequest)
		downloadErrorsTotal.Inc()
		return
	}

	// Set appropriate headers
	contentType := GetContentType(fileStorePath)
	w.Header().Set("Content-Type", contentType)
	w.Header().Set("Content-Length", strconv.FormatInt(fileInfo.Size(), 10))

	// For HEAD requests, only send headers
	if r.Method == http.MethodHead {
		w.WriteHeader(http.StatusOK)
		downloadsTotal.Inc()
		return
	}

	// For GET requests, serve the file
	file, err := os.Open(absFilename)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error opening file: %v", err), http.StatusInternalServerError)
		downloadErrorsTotal.Inc()
		return
	}
	defer file.Close()

	// Use a pooled buffer for copying
	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)
	buf := *bufPtr

	n, err := io.CopyBuffer(w, file, buf)
	if err != nil {
		log.Errorf("Error during download of %s: %v", absFilename, err)
		downloadErrorsTotal.Inc()
		return
	}

	duration := time.Since(startTime)
	downloadDuration.Observe(duration.Seconds())
	downloadsTotal.Inc()
	downloadSizeBytes.Observe(float64(n))
	log.Infof("Successfully downloaded %s (%s) in %s", absFilename, formatBytes(n), duration)
}
