#!/bin/bash

# Terminal 1: nginx Monitoring Script
echo "=== NGINX ACCESS LOG MONITOR ==="
echo "Monitoring: /var/log/nginx/share_access.log"
echo "Press Ctrl+C to stop"
echo ""
echo "Waiting for upload requests..."
echo "$(date): Monitor started"
echo ""

# Monitor nginx access logs with timestamps
sudo tail -f /var/log/nginx/share_access.log | while read line; do
    if [[ -n "$line" ]]; then
        echo "[$(date '+%H:%M:%S')] NGINX: $line"
        
        # Highlight important patterns
        if echo "$line" | grep -q "PUT"; then
            echo "*** PUT REQUEST DETECTED ***"
        fi
        
        if echo "$line" | grep -q " 401 "; then
            echo "!!! AUTH FAILURE (401) !!!"
        fi
        
        if echo "$line" | grep -q " 200 "; then
            echo "✅ SUCCESS (200) ✅"
        fi
        
        if echo "$line" | grep -q " 40[0-9] \| 50[0-9] "; then
            echo "❌ ERROR RESPONSE ❌"
        fi
    fi
done
