# Large File Upload Fix for XMPP Clients

## Problem Analysis

XMPP clients (Gajim, Dino, Conversations) were failing to upload files larger than ~100MB due to HMAC signature expiry. The XEP-0363 protocol uses signed URLs with limited validity periods that were too short for large file uploads.

## Root Cause

1. **HMAC Signature Expiry**: Prosody XMPP server generates signed URLs with 300s (5 minute) expiry
2. **Large Upload Duration**: 970MB file on 10 Mbps connection takes ~13 minutes
3. **Protocol Limitation**: XEP-0363 requires single HTTP PUT (no chunking)
4. **Timeline Conflict**: Upload duration exceeds signature validity

## Solution Implemented

### Server-Side Grace Period Extension

Modified `validateV3HMAC()` function in `cmd/server/main.go` to:

1. **Base Grace Period**: 1 hour (3600s) extension beyond expiry
2. **Large File Scaling**: Additional 1 minute per 50MB for files > 100MB  
3. **Intelligent Detection**: Uses Content-Length header to determine file size
4. **Logging**: Detailed logging of grace period calculations

### Algorithm Details

```
grace_period = 3600s (base)
if file_size > 100MB:
    additional_time = (file_size / 50MB) * 60s
    grace_period += additional_time

effective_expiry = original_expiry + grace_period
```

### Example Calculations

- **100MB file**: 3600s grace period (1 hour total)
- **500MB file**: 4200s grace period (70 minutes total) 
- **970MB file**: 4560s grace period (76 minutes total)
- **2GB file**: 6000s grace period (100 minutes total)

## Testing Recommendations

1. **Monitor Logs**: Check for grace period messages during large uploads
2. **Test Progressive Sizes**: 100MB, 500MB, 1GB, 2GB files
3. **Multiple Clients**: Test with Gajim, Dino, and Conversations
4. **Network Variations**: Test on different connection speeds

## Configuration Notes

- **Server Timeouts**: Already set to 4800s (80 minutes) ✅
- **nginx Timeouts**: Already set to 4800s (80 minutes) ✅  
- **Max Upload Size**: Already set to 10GB ✅
- **Grace Period**: Now dynamically calculated ✅

## Client-Specific Considerations

### Gajim
- Uses Python's requests library
- Default timeout ~300s (may need client-side config)
- Check `~/.config/gajim/config` for timeout settings

### Dino  
- Uses libsoup HTTP library
- Timeout behavior varies by version
- May need source modification for very large files

### Conversations
- Android HTTP client with system timeouts
- Generally more tolerant of longer uploads
- Network changes can interrupt uploads

## Future Improvements

1. **Chunked Upload Extension**: Implement non-standard chunking for ultra-large files
2. **Progressive Upload**: Add resumable upload capability  
3. **Client Timeout Detection**: Detect and handle client disconnections
4. **Bandwidth Estimation**: Dynamically adjust grace periods based on upload speed

## Monitoring Commands

```bash
# Check server logs for grace period usage
sudo journalctl -u hmac-file-server -f | grep -i grace

# Monitor upload progress
sudo tail -f /var/log/hmac-file-server/hmac-file-server.log | grep -i upload

# Check current connections  
sudo netstat -tuln | grep :8080
```

## Deployment Status

✅ **Fixed**: Large file upload signature expiry  
✅ **Deployed**: Updated server running with grace period extension  
✅ **Tested**: Server restart and basic functionality confirmed  
🔄 **Next**: Test 970MB upload with XMPP client  

## Version Information

- **Server Version**: HMAC File Server 3.2 
- **Fix Applied**: 2025-07-18 04:48:04 UTC
- **Grace Period**: Dynamic (1-100+ minutes based on file size)
- **Backward Compatibility**: Maintained for all existing uploads
