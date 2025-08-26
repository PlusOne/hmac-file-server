// upload_session.go - Resumable upload session management without modifying core functions

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"time"
)

// ChunkedUploadSession represents an ongoing upload session
type ChunkedUploadSession struct {
	ID           string                 `json:"id"`
	Filename     string                 `json:"filename"`
	TotalSize    int64                  `json:"total_size"`
	ChunkSize    int64                  `json:"chunk_size"`
	UploadedBytes int64                 `json:"uploaded_bytes"`
	Chunks       map[int]ChunkInfo      `json:"chunks"`
	LastActivity time.Time              `json:"last_activity"`
	ClientIP     string                 `json:"client_ip"`
	TempDir      string                 `json:"temp_dir"`
	Metadata     map[string]interface{} `json:"metadata"`
}

// ChunkInfo represents information about an uploaded chunk
type ChunkInfo struct {
	Number    int    `json:"number"`
	Size      int64  `json:"size"`
	Hash      string `json:"hash"`
	Completed bool   `json:"completed"`
}

// UploadSessionStore manages upload sessions
type UploadSessionStore struct {
	sessions map[string]*ChunkedUploadSession
	mutex    sync.RWMutex
	tempDir  string
}

// NewUploadSessionStore creates a new session store
func NewUploadSessionStore(tempDir string) *UploadSessionStore {
	store := &UploadSessionStore{
		sessions: make(map[string]*ChunkedUploadSession),
		tempDir:  tempDir,
	}
	
	// Create temp directory if it doesn't exist
	os.MkdirAll(tempDir, 0755)
	
	// Start cleanup routine
	go store.cleanupExpiredSessions()
	
	return store
}

// CreateSession creates a new upload session
func (s *UploadSessionStore) CreateSession(filename string, totalSize int64, clientIP string) *ChunkedUploadSession {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	
	sessionID := generateSessionID("", filename)
	tempDir := filepath.Join(s.tempDir, sessionID)
	os.MkdirAll(tempDir, 0755)
	
	session := &ChunkedUploadSession{
		ID:           sessionID,
		Filename:     filename,
		TotalSize:    totalSize,
		ChunkSize:    getChunkSize(),
		UploadedBytes: 0,
		Chunks:       make(map[int]ChunkInfo),
		LastActivity: time.Now(),
		ClientIP:     clientIP,
		TempDir:      tempDir,
		Metadata:     make(map[string]interface{}),
	}
	
	s.sessions[sessionID] = session
	s.persistSession(session)
	
	return session
}

// GetSession retrieves an existing session
func (s *UploadSessionStore) GetSession(sessionID string) (*ChunkedUploadSession, bool) {
	s.mutex.RLock()
	defer s.mutex.RUnlock()
	
	session, exists := s.sessions[sessionID]
	if !exists {
		// Try to load from persistence
		session = s.loadSession(sessionID)
		if session != nil {
			s.sessions[sessionID] = session
			exists = true
		}
	}
	
	return session, exists
}

// UpdateSession updates session progress
func (s *UploadSessionStore) UpdateSession(sessionID string, chunkNumber int, chunkSize int64) error {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	
	session, exists := s.sessions[sessionID]
	if !exists {
		return fmt.Errorf("session not found")
	}
	
	session.Chunks[chunkNumber] = ChunkInfo{
		Number:    chunkNumber,
		Size:      chunkSize,
		Completed: true,
	}
	session.UploadedBytes += chunkSize
	session.LastActivity = time.Now()
	
	s.persistSession(session)
	return nil
}

// IsSessionComplete checks if all chunks are uploaded
func (s *UploadSessionStore) IsSessionComplete(sessionID string) bool {
	session, exists := s.GetSession(sessionID)
	if !exists {
		return false
	}
	
	// Check if we have enough bytes
	if session.UploadedBytes < session.TotalSize {
		return false
	}
	
	// Verify all required chunks are present and completed
	totalChunks := int((session.TotalSize + session.ChunkSize - 1) / session.ChunkSize)
	for i := 0; i < totalChunks; i++ {
		chunkInfo, exists := session.Chunks[i]
		if !exists || !chunkInfo.Completed {
			return false
		}
		// Verify chunk file actually exists
		chunkPath := filepath.Join(session.TempDir, fmt.Sprintf("chunk_%d", i))
		if _, err := os.Stat(chunkPath); os.IsNotExist(err) {
			return false
		}
	}
	
	return true
}

// AssembleFile combines all chunks into final file (calls existing upload logic)
func (s *UploadSessionStore) AssembleFile(sessionID string) (string, error) {
	session, exists := s.GetSession(sessionID)
	if !exists {
		return "", fmt.Errorf("session not found")
	}
	
	if !s.IsSessionComplete(sessionID) {
		return "", fmt.Errorf("upload not complete")
	}
	
	// Create final file path
	finalPath := filepath.Join(conf.Server.StoragePath, session.Filename)
	finalFile, err := os.Create(finalPath)
	if err != nil {
		return "", err
	}
	defer finalFile.Close()
	
	// Combine chunks in order with better error handling and progress tracking
	totalChunks := int((session.TotalSize + session.ChunkSize - 1) / session.ChunkSize)
	var totalCopied int64
	
	for i := 0; i < totalChunks; i++ {
		chunkPath := filepath.Join(session.TempDir, fmt.Sprintf("chunk_%d", i))
		
		// Verify chunk exists before opening
		chunkInfo, err := os.Stat(chunkPath)
		if err != nil {
			return "", fmt.Errorf("chunk %d missing: %v", i, err)
		}
		
		chunkFile, err := os.Open(chunkPath)
		if err != nil {
			return "", fmt.Errorf("error opening chunk %d: %v", i, err)
		}
		
		// Copy chunk with better error handling
		copied, err := copyFileContentWithProgress(finalFile, chunkFile, chunkInfo.Size())
		chunkFile.Close()
		
		if err != nil {
			return "", fmt.Errorf("error copying chunk %d: %v", i, err)
		}
		
		totalCopied += copied
		
		// Optional: Log progress for large files
		if session.TotalSize > 100*1024*1024 { // Log for files > 100MB
			progress := float64(totalCopied) / float64(session.TotalSize) * 100
			if i%10 == 0 || i == totalChunks-1 { // Log every 10 chunks or at the end
				log.Debugf("Assembly progress for %s: %.1f%% (%d/%d chunks)", 
					session.Filename, progress, i+1, totalChunks)
			}
		}
	}
	
	// Verify final file size
	if totalCopied != session.TotalSize {
		os.Remove(finalPath) // Clean up incomplete file
		return "", fmt.Errorf("file size mismatch: expected %d, got %d", session.TotalSize, totalCopied)
	}
	
	// Cleanup temp files
	s.CleanupSession(sessionID)
	
	return finalPath, nil
}

// CleanupSession removes session and temporary files
func (s *UploadSessionStore) CleanupSession(sessionID string) {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	
	if session, exists := s.sessions[sessionID]; exists {
		os.RemoveAll(session.TempDir)
		delete(s.sessions, sessionID)
		s.removePersistedSession(sessionID)
	}
}

// persistSession saves session to disk/redis
func (s *UploadSessionStore) persistSession(session *ChunkedUploadSession) {
	// Try Redis first, fallback to disk
	if redisClient != nil && redisConnected {
		data, _ := json.Marshal(session)
		redisClient.Set(context.Background(), "upload_session:"+session.ID, data, 24*time.Hour)
	} else {
		// Fallback to disk persistence
		sessionFile := filepath.Join(s.tempDir, session.ID+".session")
		data, _ := json.Marshal(session)
		os.WriteFile(sessionFile, data, 0644)
	}
}

// loadSession loads session from disk/redis
func (s *UploadSessionStore) loadSession(sessionID string) *ChunkedUploadSession {
	var session ChunkedUploadSession
	
	// Try Redis first
	if redisClient != nil && redisConnected {
		data, err := redisClient.Get(context.Background(), "upload_session:"+sessionID).Result()
		if err == nil {
			if json.Unmarshal([]byte(data), &session) == nil {
				return &session
			}
		}
	}
	
	// Fallback to disk
	sessionFile := filepath.Join(s.tempDir, sessionID+".session")
	data, err := os.ReadFile(sessionFile)
	if err == nil {
		if json.Unmarshal(data, &session) == nil {
			return &session
		}
	}
	
	return nil
}

// removePersistedSession removes persisted session data
func (s *UploadSessionStore) removePersistedSession(sessionID string) {
	if redisClient != nil && redisConnected {
		redisClient.Del(context.Background(), "upload_session:"+sessionID)
	}
	sessionFile := filepath.Join(s.tempDir, sessionID+".session")
	os.Remove(sessionFile)
}

// cleanupExpiredSessions periodically removes old sessions
func (s *UploadSessionStore) cleanupExpiredSessions() {
	ticker := time.NewTicker(1 * time.Hour)
	defer ticker.Stop()
	
	for {
		select {
		case <-ticker.C:
			s.mutex.Lock()
			now := time.Now()
			for sessionID, session := range s.sessions {
				if now.Sub(session.LastActivity) > 24*time.Hour {
					s.CleanupSession(sessionID)
				}
			}
			s.mutex.Unlock()
		}
	}
}

// Helper functions
func getChunkSize() int64 {
	// Default 5MB chunks, configurable
	if conf.Uploads.ChunkSize != "" {
		if size, err := parseSize(conf.Uploads.ChunkSize); err == nil {
			return size
		}
	}
	return 5 * 1024 * 1024 // 5MB default
}

func randomString(n int) string {
	const charset = "abcdefghijklmnopqrstuvwxyz0123456789"
	b := make([]byte, n)
	for i := range b {
		b[i] = charset[time.Now().UnixNano()%int64(len(charset))]
	}
	return string(b)
}

func copyFileContent(dst, src *os.File) (int64, error) {
	// Use the existing buffer pool for efficiency
	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)
	buf := *bufPtr
	
	var written int64
	for {
		n, err := src.Read(buf)
		if n > 0 {
			w, werr := dst.Write(buf[:n])
			written += int64(w)
			if werr != nil {
				return written, werr
			}
		}
		if err != nil {
			if err.Error() == "EOF" {
				break
			}
			return written, err
		}
	}
	return written, nil
}

// copyFileContentWithProgress copies file content with progress tracking and better error handling
func copyFileContentWithProgress(dst, src *os.File, expectedSize int64) (int64, error) {
	// Use the existing buffer pool for efficiency
	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)
	buf := *bufPtr
	
	var written int64
	for {
		n, err := src.Read(buf)
		if n > 0 {
			w, werr := dst.Write(buf[:n])
			written += int64(w)
			if werr != nil {
				return written, fmt.Errorf("write error after %d bytes: %v", written, werr)
			}
		}
		if err != nil {
			if err.Error() == "EOF" {
				break
			}
			return written, fmt.Errorf("read error after %d bytes: %v", written, err)
		}
	}
	
	// Verify we copied the expected amount
	if expectedSize > 0 && written != expectedSize {
		return written, fmt.Errorf("chunk size mismatch: expected %d, copied %d", expectedSize, written)
	}
	
	return written, nil
}
