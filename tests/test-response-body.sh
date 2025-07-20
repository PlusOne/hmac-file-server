#!/bin/bash
# Test with full response body capture

BASE_PATH="c184288b79f8b7a6f7d87ac7f1fb1ce6dcf49a80"
SUB_PATH="responsebody"
FILENAME="test.mp4"
FULL_PATH="$BASE_PATH/$SUB_PATH/$FILENAME"
SECRET="f6g4ldPvQM7O2UTFeBEUUj33VrXypDAcsDt0yqKrLiOr5oQW"

TEST_FILE="/tmp/test_response.mp4"
echo -n "Response body test" > "$TEST_FILE"
FILE_SIZE=$(stat -c%s "$TEST_FILE")

HMAC_MESSAGE="$FULL_PATH $FILE_SIZE"
HMAC_CALC=$(printf "%s" "$HMAC_MESSAGE" | openssl dgst -sha256 -hmac "$SECRET" | cut -d' ' -f2)

echo "=== Testing with Full Response Capture ==="
echo "Path: $FULL_PATH"
echo "HMAC: $HMAC_CALC"
echo ""

# Capture full response including body
RESPONSE=$(curl -X PUT \
     -H "Content-Type: video/mp4" \
     -H "User-Agent: TestResponseBody/1.0" \
     --data-binary "@$TEST_FILE" \
     "https://share.uuxo.net/$FULL_PATH?v=$HMAC_CALC" \
     -s \
     -w "CURL_STATUS:%{http_code}\nCURL_SIZE:%{size_upload}\n" \
     2>&1)

echo "=== Full Response ==="
echo "$RESPONSE"
echo ""

# Extract just the response body (everything before CURL_STATUS)
RESPONSE_BODY=$(echo "$RESPONSE" | sed '/CURL_STATUS:/,$d')
echo "=== Response Body Only ==="
echo "'$RESPONSE_BODY'"
echo ""

# Check response length
RESPONSE_LENGTH=${#RESPONSE_BODY}
echo "Response body length: $RESPONSE_LENGTH characters"

if [ $RESPONSE_LENGTH -eq 32 ]; then
    echo "✅ Response is exactly 32 characters (matches Nginx logs)"
elif [ $RESPONSE_LENGTH -eq 0 ]; then
    echo "⚠️ Empty response body"
else
    echo "ℹ️ Different response length than expected"
fi

# Clean up
rm -f "$TEST_FILE"
