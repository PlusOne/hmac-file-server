#!/bin/bash
# Test script to trace 400 errors in HMAC file server uploads

# Test URL from the logs
TEST_URL="https://share.uuxo.net/c184288b79f8b7a6f7d87ac7f1fb1ce6dcf49a80/test/test.mp4?v=test123"

echo "Testing with a simple small file..."

# Create a small test file
echo "Test content for upload debugging" > /tmp/test_upload.mp4

echo "Attempting upload with curl..."
curl -X PUT \
     -H "Content-Type: video/mp4" \
     -H "User-Agent: TestDebug/1.0" \
     --data-binary "@/tmp/test_upload.mp4" \
     "$TEST_URL" \
     -v \
     -w "\n\nResponse Code: %{http_code}\nTotal Time: %{time_total}s\nSize Uploaded: %{size_upload} bytes\n" \
     2>&1

echo -e "\n\nNow checking the logs for this specific request..."

# Wait a moment for logs to be written
sleep 2

# Check recent logs
echo "=== HMAC File Server Logs ==="
tail -10 /opt/hmac-file-server/data/logs/hmac-file-server.log | grep -v "Interface\|RTT\|Loss"

echo -e "\n=== Nginx Access Log ==="
tail -5 /var/log/nginx/access.log | grep PUT

echo -e "\n=== Nginx Error Log ==="
tail -5 /var/log/nginx/upload_errors.log

# Clean up
rm -f /tmp/test_upload.mp4
