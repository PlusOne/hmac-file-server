# Network Switching Improvements for HMAC File Server

## Issues Identified

### 1. No Resumable Upload Support
- Current uploads fail completely on network interruption
- No chunked upload implementation despite configuration option
- File deletion on any upload error loses all progress

### 2. Aggressive Connection Timeouts
- ReadTimeout/WriteTimeout too short for large uploads over mobile networks
- IdleConnTimeout too aggressive for network switching scenarios
- No retry mechanisms for temporary network failures

### 3. No Connection State Management
- No detection of network changes
- No graceful handling of connection switches
- No upload session persistence

## Recommended Improvements

### 1. Implement Chunked/Resumable Uploads

```go
// Add to upload configuration
type ChunkedUploadSession struct {
    ID          string
    Filename    string
    TotalSize   int64
    ChunkSize   int64
    UploadedBytes int64
    Chunks      map[int]bool  // Track completed chunks
    LastActivity time.Time
    ClientIP    string
}

// New upload handler for chunked uploads
func handleChunkedUpload(w http.ResponseWriter, r *http.Request) {
    // Check for existing session
    sessionID := r.Header.Get("X-Upload-Session-ID")
    chunkNumber := r.Header.Get("X-Chunk-Number")
    
    // Resume logic here
}
```

### 2. Enhanced Connection Management

```go
// Improved HTTP client configuration
dualStackClient = &http.Client{
    Transport: &http.Transport{
        DialContext:           dialer.DialContext,
        IdleConnTimeout:       300 * time.Second, // 5 minutes for mobile
        MaxIdleConns:          50,
        MaxIdleConnsPerHost:   20,                // More connections per host
        TLSHandshakeTimeout:   30 * time.Second,  // Longer for mobile networks
        ResponseHeaderTimeout: 60 * time.Second,  // Account for network switches
        DisableKeepAlives:     false,             // Enable keep-alives
        MaxConnsPerHost:       30,                // Allow more concurrent connections
    },
    Timeout: 0, // No overall timeout - let individual operations timeout
}

// Enhanced server timeouts
server := &http.Server{
    ReadTimeout:    5 * time.Minute,   // Allow for slow mobile uploads
    WriteTimeout:   5 * time.Minute,   // Allow for slow responses
    IdleTimeout:    10 * time.Minute,  // Keep connections alive longer
}
```

### 3. Network Change Detection and Handling

```go
// Enhanced network monitoring
func monitorNetworkChanges(ctx context.Context) {
    ticker := time.NewTicker(5 * time.Second) // More frequent checking
    defer ticker.Stop()
    
    var lastInterfaces []net.Interface
    
    for {
        select {
        case <-ctx.Done():
            return
        case <-ticker.C:
            currentInterfaces, err := net.Interfaces()
            if err != nil {
                continue
            }
            
            // Detect interface changes
            if hasNetworkChanges(lastInterfaces, currentInterfaces) {
                log.Info("Network change detected - pausing active uploads")
                pauseActiveUploads()
                
                // Wait for network stabilization
                time.Sleep(2 * time.Second)
                
                log.Info("Resuming uploads after network change")
                resumeActiveUploads()
            }
            
            lastInterfaces = currentInterfaces
        }
    }
}
```

### 4. Upload Session Persistence

```go
// Store upload sessions in Redis or local cache
type UploadSessionStore struct {
    sessions map[string]*ChunkedUploadSession
    mutex    sync.RWMutex
}

func (s *UploadSessionStore) SaveSession(session *ChunkedUploadSession) {
    s.mutex.Lock()
    defer s.mutex.Unlock()
    
    // Store in Redis if available, otherwise in-memory
    if redisClient != nil {
        data, _ := json.Marshal(session)
        redisClient.Set(ctx, "upload:"+session.ID, data, 24*time.Hour)
    } else {
        s.sessions[session.ID] = session
    }
}
```

### 5. Client-Side Retry Logic (for mobile apps/browsers)

```javascript
// Client-side upload with retry logic
class ResilientUploader {
    constructor(file, endpoint, options = {}) {
        this.file = file;
        this.endpoint = endpoint;
        this.chunkSize = options.chunkSize || 5 * 1024 * 1024; // 5MB chunks
        this.maxRetries = options.maxRetries || 5;
        this.retryDelay = options.retryDelay || 2000;
    }
    
    async upload() {
        const totalChunks = Math.ceil(this.file.size / this.chunkSize);
        const sessionId = this.generateSessionId();
        
        for (let i = 0; i < totalChunks; i++) {
            await this.uploadChunk(i, sessionId);
        }
    }
    
    async uploadChunk(chunkIndex, sessionId, retryCount = 0) {
        try {
            const start = chunkIndex * this.chunkSize;
            const end = Math.min(start + this.chunkSize, this.file.size);
            const chunk = this.file.slice(start, end);
            
            const response = await fetch(this.endpoint, {
                method: 'PUT',
                headers: {
                    'X-Upload-Session-ID': sessionId,
                    'X-Chunk-Number': chunkIndex,
                    'X-Total-Chunks': totalChunks,
                    'Content-Range': `bytes ${start}-${end-1}/${this.file.size}`
                },
                body: chunk
            });
            
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            
        } catch (error) {
            if (retryCount < this.maxRetries) {
                // Exponential backoff with jitter
                const delay = this.retryDelay * Math.pow(2, retryCount) + Math.random() * 1000;
                await new Promise(resolve => setTimeout(resolve, delay));
                return this.uploadChunk(chunkIndex, sessionId, retryCount + 1);
            }
            throw error;
        }
    }
}
```

## Implementation Priority

1. **High Priority**: Implement chunked uploads with session persistence
2. **High Priority**: Adjust connection timeouts for mobile scenarios
3. **Medium Priority**: Add network change detection and upload pausing
4. **Medium Priority**: Implement retry logic in upload handlers
5. **Low Priority**: Add client-side SDK with built-in resilience

## Configuration Changes Needed

```toml
[uploads]
resumableuploadsenabled = true    # Enable the feature
chunkeduploadsenabled = true      # Already exists but not implemented
chunksize = "5MB"                 # Smaller chunks for mobile
sessiontimeout = "24h"            # How long to keep upload sessions
maxretries = 5                    # Server-side retry attempts

[timeouts]
readtimeout = "300s"              # 5 minutes for mobile uploads
writetimeout = "300s"             # 5 minutes for responses  
idletimeout = "600s"              # 10 minutes idle timeout
uploadtimeout = "3600s"           # 1 hour total upload timeout

[network]
networkchangedetection = true     # Enable network monitoring
uploadpauseonchange = true        # Pause uploads during network changes
reconnectdelay = "2s"             # Wait time after network change
keepaliveinterval = "30s"         # TCP keep-alive interval
```

This comprehensive approach will make uploads much more resilient to network switching scenarios common with mobile devices using multiple network interfaces.
