#!/bin/bash

# Simple test to debug the 49% upload stop issue

set -e

echo "[DEBUG-TEST] Starting server..."
./hmac-file-server --config config-network-resilience.toml > debug_server.log 2>&1 &
SERVER_PID=$!

# Wait for server to start
sleep 3

# Check if server is running
if ! kill -0 $SERVER_PID 2>/dev/null; then
    echo "[ERROR] Server failed to start"
    cat debug_server.log
    exit 1
fi

cleanup() {
    echo "[DEBUG-TEST] Cleaning up..."
    kill $SERVER_PID 2>/dev/null || true
    rm -f debug_server.log
}

trap cleanup EXIT

echo "[DEBUG-TEST] Testing 50MB chunked upload..."

# Calculate HMAC signature
SECRET="your-super-secret-hmac-key-minimum-32-characters-long"
MESSAGE="/chunked-upload"
SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "$SECRET" | cut -d' ' -f2)

# Start session
echo "[DEBUG-TEST] Creating session..."
SESSION_RESPONSE=$(curl -s -X POST \
    -H "X-Filename: test_50mb.bin" \
    -H "X-Total-Size: 52428800" \
    -H "X-Signature: $SIGNATURE" \
    http://localhost:8080/chunked-upload)

echo "[DEBUG-TEST] Session response: $SESSION_RESPONSE"

SESSION_ID=$(echo "$SESSION_RESPONSE" | grep -o '"session_id":"[^"]*"' | cut -d'"' -f4)
if [ -z "$SESSION_ID" ]; then
    echo "[ERROR] Failed to get session ID"
    exit 1
fi

echo "[DEBUG-TEST] Session ID: $SESSION_ID"

# Upload first few chunks to see what happens
CHUNK_SIZE=5242880  # 5MB
for i in {0..12}; do  # Upload first 13 chunks (65MB worth, should trigger completion)
    OFFSET=$((i * CHUNK_SIZE))
    
    echo "[DEBUG-TEST] Creating chunk $i..."
    dd if=test_50mb.bin of=chunk_$i.bin bs=$CHUNK_SIZE skip=$i count=1 2>/dev/null || {
        # Handle the last chunk
        REMAINING=$((52428800 - OFFSET))
        if [ $REMAINING -gt 0 ]; then
            dd if=test_50mb.bin of=chunk_$i.bin bs=1 skip=$OFFSET count=$REMAINING 2>/dev/null
        else
            echo "[DEBUG-TEST] No more data for chunk $i"
            break
        fi
    }
    
    CHUNK_SIZE_ACTUAL=$(stat -f%z chunk_$i.bin 2>/dev/null || stat -c%s chunk_$i.bin 2>/dev/null)
    echo "[DEBUG-TEST] Uploading chunk $i (size: $CHUNK_SIZE_ACTUAL bytes)..."
    
    UPLOAD_RESPONSE=$(curl -s -w "\n%{http_code}" -X PUT \
        -H "X-Upload-Session-ID: $SESSION_ID" \
        -H "X-Chunk-Number: $i" \
        --data-binary @chunk_$i.bin \
        http://localhost:8080/chunked-upload)
    
    echo "[DEBUG-TEST] Upload response for chunk $i:"
    echo "$UPLOAD_RESPONSE"
    echo "---"
    
    # Check server logs for debug output
    echo "[DEBUG-TEST] Recent server logs:"
    tail -5 debug_server.log
    echo "---"
    
    # Check if complete
    if echo "$UPLOAD_RESPONSE" | grep -q '"complete":true'; then
        echo "[DEBUG-TEST] ✅ Upload completed at chunk $i"
        rm -f chunk_*.bin
        exit 0
    fi
    
    rm -f chunk_$i.bin
    sleep 1
done

echo "[DEBUG-TEST] Upload did not complete. Checking status..."
STATUS_RESPONSE=$(curl -s "http://localhost:8080/upload-status?session_id=$SESSION_ID")
echo "[DEBUG-TEST] Final status: $STATUS_RESPONSE"

echo "[DEBUG-TEST] Full server logs:"
cat debug_server.log
