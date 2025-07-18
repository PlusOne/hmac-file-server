#!/bin/bash

# Monitor script to watch for XMPP upload activity
# This will help verify that our performance optimizations are working

echo "=== HMAC File Server Upload Monitor ==="
echo "Watching for upload activity on share.uuxo.net..."
echo "Press Ctrl+C to stop"
echo ""

# Function to show current configuration status
show_status() {
    echo "Current Configuration Status:"
    echo "- Max Upload Size: $(grep max_upload_size /etc/hmac-file-server/config.toml | cut -d'"' -f2)"
    echo "- ClamAV Enabled: $(grep clamavenabled /etc/hmac-file-server/config.toml | cut -d'=' -f2 | tr -d ' ')"
    echo "- Deduplication: $(grep deduplication_enabled /etc/hmac-file-server/config.toml | cut -d'=' -f2 | tr -d ' ')"
    echo "- File Naming: $(grep file_naming /etc/hmac-file-server/config.toml | cut -d'"' -f2)"
    echo ""
}

# Function to monitor logs
monitor_logs() {
    echo "Starting real-time log monitoring..."
    echo "Monitoring multiple log sources:"
    echo "1. HMAC Server logs (/var/log/hmac-file-server/hmac-file-server.log)"
    echo "2. Share nginx access logs (/var/log/nginx/share_access.log)"
    echo "3. Share nginx error logs (/var/log/nginx/share_error.log)"
    echo ""
    
    # Run tail on multiple files simultaneously
    sudo tail -f /var/log/hmac-file-server/hmac-file-server.log \
                /var/log/nginx/share_access.log \
                /var/log/nginx/share_error.log 2>/dev/null | \
    while read line; do
        timestamp=$(date '+%H:%M:%S')
        echo "[$timestamp] $line"
        
        # Highlight important upload events
        if echo "$line" | grep -qi "PUT\|upload\|POST"; then
            echo "*** UPLOAD ACTIVITY DETECTED ***"
        fi
        
        if echo "$line" | grep -qi "error\|failed\|timeout"; then
            echo "!!! ERROR/ISSUE DETECTED !!!"
        fi
        
        if echo "$line" | grep -qi "clamav\|scan"; then
            echo ">>> ClamAV ACTIVITY <<<"
        fi
        
        if echo "$line" | grep -qi "dedup"; then
            echo ">>> DEDUPLICATION ACTIVITY <<<"
        fi
    done
}

# Show current status
show_status

# Start monitoring
monitor_logs
