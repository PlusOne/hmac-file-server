#!/bin/bash
# Corrected HMAC calculation test

# Configuration
BASE_PATH="c184288b79f8b7a6f7d87ac7f1fb1ce6dcf49a80"
SUB_PATH="debugfixed"
FILENAME="test.mp4"
FULL_PATH="$BASE_PATH/$SUB_PATH/$FILENAME"
SECRET="f6g4ldPvQM7O2UTFeBEUUj33VrXypDAcsDt0yqKrLiOr5oQW"

# Create test file
TEST_FILE="/tmp/test_fixed.mp4"
echo -n "Test content for HMAC debugging" > "$TEST_FILE"
FILE_SIZE=$(stat -c%s "$TEST_FILE")

echo "=== Corrected HMAC Test ==="
echo "File: $TEST_FILE ($FILE_SIZE bytes)"
echo "Path: $FULL_PATH"
echo ""

# Correct HMAC calculation (using actual space character, not literal \x20)
# The server does: fileStorePath + "\x20" + contentLength
# In bash, \x20 means actual space character (0x20)
HMAC_MESSAGE="$FULL_PATH $FILE_SIZE"
echo "HMAC message: '$HMAC_MESSAGE'"

# Calculate HMAC
HMAC_CALC=$(printf "%s" "$HMAC_MESSAGE" | openssl dgst -sha256 -hmac "$SECRET" | cut -d' ' -f2)
echo "Calculated HMAC: $HMAC_CALC"
echo ""

# Test the upload
echo "=== Testing Upload ==="
curl -X PUT \
     -H "Content-Type: video/mp4" \
     -H "User-Agent: TestFixed/1.0" \
     --data-binary "@$TEST_FILE" \
     "https://share.uuxo.net/$FULL_PATH?v=$HMAC_CALC" \
     -v \
     -s \
     -w "\nFinal Response: %{http_code}\n" \
     2>&1 | grep -E "(PUT|HTTP/2|Final Response|Content-Length:|User-Agent:)"

echo ""
echo "=== Server Logs ==="
sleep 2
tail -10 /opt/hmac-file-server/data/logs/hmac-file-server.log | grep -E "(handleLegacyUpload|validateHMAC|protocol.*calculated|successful)" | tail -5

# Clean up
rm -f "$TEST_FILE"
