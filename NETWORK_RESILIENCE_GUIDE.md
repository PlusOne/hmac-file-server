# Network Resilience Implementation Guide

## Overview

This implementation adds network switching resilience to the HMAC File Server **without modifying any core functions**. The solution uses a wrapper/middleware approach that enhances existing functionality while maintaining backward compatibility.

## New Features Added

### 1. Chunked/Resumable Uploads
- **Endpoint**: `POST/PUT /upload/chunked`
- **Status Check**: `GET /upload/status?session_id=<id>`
- Breaks large uploads into smaller chunks (5MB default)
- Resumes interrupted uploads automatically
- Survives network switching scenarios

### 2. Network Change Detection
- Monitors network interface status every 5 seconds
- Automatically pauses uploads during network changes
- Resumes uploads after network stabilization (2-second delay)
- Logs network events for monitoring

### 3. Upload Session Persistence
- Stores upload state in Redis (if available) or local disk
- Sessions persist across server restarts
- Automatic cleanup of expired sessions (24-hour default)
- Tracks chunk completion status

### 4. Enhanced Connection Management
- Configurable timeouts optimized for mobile networks
- Retry logic with exponential backoff
- Better keep-alive settings for mobile scenarios

## Configuration

Add these settings to your `config.toml`:

```toml
[uploads]
chunkeduploadsenabled = true      # Enable chunked uploads
resumableuploadsenabled = true    # Enable resumable functionality  
chunksize = "5MB"                 # Chunk size (smaller for mobile)
sessiontimeout = "24h"            # Session persistence time
maxretries = 5                    # Retry attempts

[timeouts]
readtimeout = "300s"              # 5 minutes (vs 30s default)
writetimeout = "300s"             # 5 minutes (vs 30s default)
idletimeout = "600s"              # 10 minutes (vs 120s default)

[server]
networkevents = true              # Enable network monitoring
```

## API Usage

### Traditional Upload (existing, unchanged)
```bash
curl -X POST -F 'file=@large_file.zip' \
  -H "X-Signature: HMAC_SIGNATURE" \
  http://localhost:8080/upload
```

### New Chunked Upload

#### 1. Start Upload Session
```bash
curl -X POST \
  -H "X-Filename: large_file.zip" \
  -H "X-Total-Size: 104857600" \
  -H "X-Signature: HMAC_SIGNATURE" \
  http://localhost:8080/upload/chunked

# Response:
{
  "session_id": "1234567890_abc123",
  "chunk_size": 5242880,
  "total_chunks": 20
}
```

#### 2. Upload Chunks
```bash
# Upload chunk 0
curl -X PUT \
  -H "X-Upload-Session-ID: 1234567890_abc123" \
  -H "X-Chunk-Number: 0" \
  -H "Content-Type: application/octet-stream" \
  --data-binary @chunk_0.bin \
  http://localhost:8080/upload/chunked

# Response:
{
  "success": true,
  "chunk": 0,
  "uploaded_bytes": 5242880,
  "total_size": 104857600,
  "progress": 0.05,
  "completed": false
}
```

#### 3. Check Status
```bash
curl "http://localhost:8080/upload/status?session_id=1234567890_abc123"

# Response:
{
  "session_id": "1234567890_abc123",
  "filename": "large_file.zip",
  "total_size": 104857600,
  "uploaded_bytes": 52428800,
  "progress": 0.5,
  "completed": false,
  "chunks": 10
}
```

## Client-Side JavaScript Example

```javascript
class NetworkResilientUploader {
    constructor(file, endpoint, options = {}) {
        this.file = file;
        this.endpoint = endpoint;
        this.chunkSize = options.chunkSize || 5 * 1024 * 1024; // 5MB
        this.maxRetries = options.maxRetries || 5;
    }
    
    async upload() {
        // Start session
        const session = await this.startSession();
        console.log('Upload session started:', session.session_id);
        
        // Upload chunks
        const totalChunks = Math.ceil(this.file.size / this.chunkSize);
        for (let i = 0; i < totalChunks; i++) {
            await this.uploadChunk(session.session_id, i);
            console.log(`Chunk ${i + 1}/${totalChunks} uploaded`);
        }
        
        console.log('Upload completed!');
    }
    
    async startSession() {
        const response = await fetch(this.endpoint, {
            method: 'POST',
            headers: {
                'X-Filename': this.file.name,
                'X-Total-Size': this.file.size.toString(),
                'X-Signature': 'YOUR_HMAC_SIGNATURE'
            }
        });
        return response.json();
    }
    
    async uploadChunk(sessionId, chunkNumber, retryCount = 0) {
        try {
            const start = chunkNumber * this.chunkSize;
            const end = Math.min(start + this.chunkSize, this.file.size);
            const chunk = this.file.slice(start, end);
            
            const response = await fetch(this.endpoint, {
                method: 'PUT',
                headers: {
                    'X-Upload-Session-ID': sessionId,
                    'X-Chunk-Number': chunkNumber.toString()
                },
                body: chunk
            });
            
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            return response.json();
            
        } catch (error) {
            if (retryCount < this.maxRetries) {
                const delay = Math.pow(2, retryCount) * 1000 + Math.random() * 1000;
                await new Promise(resolve => setTimeout(resolve, delay));
                return this.uploadChunk(sessionId, chunkNumber, retryCount + 1);
            }
            throw error;
        }
    }
}

// Usage
const uploader = new NetworkResilientUploader(fileInput.files[0], '/upload/chunked');
uploader.upload().catch(console.error);
```

## Benefits for Network Switching

### Before (Problems)
- ❌ Upload fails completely on network interruption
- ❌ User loses all progress when switching WiFi/WLAN
- ❌ Large files problematic on mobile networks
- ❌ No recovery from temporary connection issues

### After (Solutions)
- ✅ Upload resumes automatically after network switch
- ✅ Progress preserved during network interruptions  
- ✅ Optimal chunk sizes for mobile networks
- ✅ Automatic retry with exponential backoff
- ✅ Network change detection and graceful handling
- ✅ Session persistence across server restarts

## Monitoring

Check logs for network resilience status:
```bash
# Network change detection
2025-07-17 12:34:56 INFO Network change detected
2025-07-17 12:34:56 INFO Pausing all active uploads due to network change
2025-07-17 12:34:58 INFO Resuming all paused uploads

# Upload session activity  
2025-07-17 12:35:00 INFO Active upload sessions: 3
2025-07-17 12:35:00 INFO Network resilience: 5 uploads active
```

## Implementation Notes

### Non-Intrusive Design
- **No core function modifications**: All existing upload handlers remain unchanged
- **Backward compatibility**: Traditional uploads continue to work exactly as before
- **Optional features**: Network resilience only activates when `chunkeduploadsenabled = true`
- **Gradual adoption**: Users can test chunked uploads alongside existing uploads

### File Structure
```
cmd/server/
├── main.go                    # Core server (minimal changes)
├── helpers.go                 # Router setup (minimal changes)
├── upload_session.go          # NEW: Session management
├── network_resilience.go      # NEW: Network monitoring
├── chunked_upload_handler.go  # NEW: Chunked upload API
└── integration.go             # NEW: Non-intrusive integration
```

This approach ensures that your existing upload functionality remains stable while adding robust network switching capabilities for users who need them.
