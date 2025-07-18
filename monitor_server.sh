#!/bin/bash

# Terminal 2: HMAC Server Monitoring Script
echo "=== HMAC SERVER LOG MONITOR ==="
echo "Monitoring: /var/log/hmac-file-server/hmac-file-server.log"
echo "Press Ctrl+C to stop"
echo ""
echo "Waiting for upload activity..."
echo "$(date): Monitor started"
echo ""

# Monitor server logs with filtering and highlighting
sudo tail -f /var/log/hmac-file-server/hmac-file-server.log | while read line; do
    # Skip debug worker messages unless they're important
    if echo "$line" | grep -q "DEBUG.*Worker\|NumWorkers\|NumScanWorkers" && ! echo "$line" | grep -q "upload\|error\|fail"; then
        continue
    fi
    
    if [[ -n "$line" ]]; then
        echo "[$(date '+%H:%M:%S')] SERVER: $line"
        
        # Highlight upload-related activity
        if echo "$line" | grep -qi "upload\|PUT\|POST"; then
            echo "📤 UPLOAD ACTIVITY DETECTED"
        fi
        
        # Highlight HMAC validation
        if echo "$line" | grep -qi "hmac\|auth\|signature"; then
            echo "🔐 HMAC VALIDATION ACTIVITY"
        fi
        
        # Highlight deduplication
        if echo "$line" | grep -qi "dedup"; then
            echo "🔗 DEDUPLICATION ACTIVITY"
        fi
        
        # Highlight errors
        if echo "$line" | grep -qi "error\|fail\|fatal"; then
            echo "❌ ERROR DETECTED ❌"
        fi
        
        # Highlight success
        if echo "$line" | grep -qi "success"; then
            echo "✅ SUCCESS DETECTED ✅"
        fi
        
        # Highlight file operations
        if echo "$line" | grep -qi "file.*created\|file.*stored\|file.*saved"; then
            echo "💾 FILE STORAGE ACTIVITY"
        fi
    fi
done
