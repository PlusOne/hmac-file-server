#!/bin/bash
# Advanced test to diagnose XMPP upload issues

echo "=== HMAC File Server Upload Debugging ==="
echo ""

# First, let's simulate exactly what we see in the logs
# Using a real path from the failed uploads
BASE_PATH="c184288b79f8b7a6f7d87ac7f1fb1ce6dcf49a80"
SUB_PATH="testdebug"
FILENAME="test.mp4"
FULL_PATH="$BASE_PATH/$SUB_PATH/$FILENAME"

# Create test file
TEST_FILE="/tmp/test_debug.mp4"
echo "Creating test content..." > "$TEST_FILE"
FILE_SIZE=$(stat -c%s "$TEST_FILE")

echo "Test file: $TEST_FILE"
echo "File size: $FILE_SIZE bytes"
echo "Upload path: $FULL_PATH"
echo ""

# Let's calculate the HMAC like the server does
# For v protocol: fileStorePath + "\x20" + contentLength
SECRET="f6g4ldPvQM7O2UTFeBEUUj33VrXypDAcsDt0yqKrLiOr5oQW"

# Method 1: Calculate HMAC using the file size
HMAC_MESSAGE="$FULL_PATH $(printf '\x20')$FILE_SIZE"
HMAC_CALC=$(echo -n "$HMAC_MESSAGE" | openssl dgst -sha256 -hmac "$SECRET" | cut -d' ' -f2)

echo "HMAC calculation:"
echo "Message: '$FULL_PATH\\x20$FILE_SIZE'"
echo "HMAC: $HMAC_CALC"
echo ""

# Test 1: Upload with correct HMAC
echo "=== Test 1: Upload with calculated HMAC ==="
curl -X PUT \
     -H "Content-Type: video/mp4" \
     -H "User-Agent: TestDebugCorrect/1.0" \
     --data-binary "@$TEST_FILE" \
     "https://share.uuxo.net/$FULL_PATH?v=$HMAC_CALC" \
     -v \
     -w "\nResponse: %{http_code}, Time: %{time_total}s\n" \
     2>&1 | grep -E "(Response|HTTP/|Content-Length|User-Agent)"

echo ""

# Test 2: Upload with Content-Length: 0 (simulating potential XMPP issue)
echo "=== Test 2: Upload with Content-Length: 0 ==="
curl -X PUT \
     -H "Content-Type: video/mp4" \
     -H "Content-Length: 0" \
     -H "User-Agent: TestDebugZeroLength/1.0" \
     --data-binary "@$TEST_FILE" \
     "https://share.uuxo.net/$FULL_PATH?v=$HMAC_CALC" \
     -v \
     -w "\nResponse: %{http_code}, Time: %{time_total}s\n" \
     2>&1 | grep -E "(Response|HTTP/|Content-Length|User-Agent)"

echo ""

# Test 3: Upload without Content-Length header
echo "=== Test 3: Upload using chunked transfer (no Content-Length) ==="
curl -X PUT \
     -H "Content-Type: video/mp4" \
     -H "Transfer-Encoding: chunked" \
     -H "User-Agent: TestDebugChunked/1.0" \
     --data-binary "@$TEST_FILE" \
     "https://share.uuxo.net/$FULL_PATH?v=$HMAC_CALC" \
     -v \
     -w "\nResponse: %{http_code}, Time: %{time_total}s\n" \
     2>&1 | grep -E "(Response|HTTP/|Transfer-Encoding|User-Agent)"

echo ""

# Test 4: Calculate HMAC with ContentLength 0 (what might be happening)
HMAC_MESSAGE_ZERO="$FULL_PATH $(printf '\x20')0"
HMAC_CALC_ZERO=$(echo -n "$HMAC_MESSAGE_ZERO" | openssl dgst -sha256 -hmac "$SECRET" | cut -d' ' -f2)

echo "=== Test 4: Upload with HMAC calculated for ContentLength=0 ==="
echo "HMAC for zero length: $HMAC_CALC_ZERO"

curl -X PUT \
     -H "Content-Type: video/mp4" \
     -H "User-Agent: TestDebugZeroHMAC/1.0" \
     --data-binary "@$TEST_FILE" \
     "https://share.uuxo.net/$FULL_PATH?v=$HMAC_CALC_ZERO" \
     -v \
     -w "\nResponse: %{http_code}, Time: %{time_total}s\n" \
     2>&1 | grep -E "(Response|HTTP/|Content-Length|User-Agent)"

echo ""
echo "=== Recent server logs ==="
sleep 2
tail -15 /opt/hmac-file-server/data/logs/hmac-file-server.log | grep -v "Interface\|RTT\|Loss" | tail -10

# Cleanup
rm -f "$TEST_FILE"
