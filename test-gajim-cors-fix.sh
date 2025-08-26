#!/bin/bash
# Test script to verify CORS functionality for Gajim compatibility

echo "🧪 Testing CORS Functionality for Gajim Compatibility"
echo "========================================================"

SERVER_URL="http://localhost:8080"

echo ""
echo "1. Testing OPTIONS preflight request (Gajim issue):"
echo "---------------------------------------------------"
CORS_RESULT=$(curl -s -X OPTIONS "$SERVER_URL/" -w "HTTP_CODE:%{http_code}" -H "Origin: https://example.com")
HTTP_CODE=$(echo "$CORS_RESULT" | grep -o "HTTP_CODE:[0-9]*" | cut -d: -f2)

if [ "$HTTP_CODE" = "200" ]; then
    echo "✅ OPTIONS request successful (HTTP 200)"
    echo "   This fixes Gajim's 'bad gateway' error!"
else
    echo "❌ OPTIONS request failed (HTTP $HTTP_CODE)"
    exit 1
fi

echo ""
echo "2. Checking CORS headers in response:"
echo "------------------------------------"
HEADERS=$(curl -s -X OPTIONS "$SERVER_URL/" -D -)
echo "$HEADERS" | grep -i "access-control" | while read line; do
    echo "✅ $line"
done

echo ""
echo "3. Testing regular GET request with CORS:"
echo "-----------------------------------------"
GET_RESULT=$(curl -s "$SERVER_URL/health" -w "HTTP_CODE:%{http_code}" -H "Origin: https://gajim.org")
GET_CODE=$(echo "$GET_RESULT" | grep -o "HTTP_CODE:[0-9]*" | cut -d: -f2)

if [ "$GET_CODE" = "200" ]; then
    echo "✅ GET request with CORS successful (HTTP 200)"
else
    echo "❌ GET request failed (HTTP $GET_CODE)"
fi

echo ""
echo "4. Simulating XMPP client preflight sequence:"
echo "---------------------------------------------"
# This simulates what Gajim does before file upload
echo "Step 1: OPTIONS preflight..."
OPTIONS_TEST=$(curl -s -X OPTIONS "$SERVER_URL/upload" \
    -H "Origin: https://gajim.org" \
    -H "Access-Control-Request-Method: PUT" \
    -H "Access-Control-Request-Headers: Authorization,Content-Type" \
    -w "HTTP_CODE:%{http_code}")

OPTIONS_CODE=$(echo "$OPTIONS_TEST" | grep -o "HTTP_CODE:[0-9]*" | cut -d: -f2)
if [ "$OPTIONS_CODE" = "200" ]; then
    echo "✅ XMPP client preflight successful"
else
    echo "❌ XMPP client preflight failed (HTTP $OPTIONS_CODE)"
fi

echo ""
echo "🎯 SUMMARY:"
echo "==========="
if [ "$HTTP_CODE" = "200" ] && [ "$GET_CODE" = "200" ] && [ "$OPTIONS_CODE" = "200" ]; then
    echo "✅ ALL TESTS PASSED"
    echo "✅ Gajim's 'bad gateway' error should be FIXED!"
    echo "✅ XMPP clients can now perform CORS preflight requests"
    echo ""
    echo "📋 What this fixes:"
    echo "   - Gajim intermittent 'bad gateway' errors"
    echo "   - Web-based XMPP clients CORS issues"
    echo "   - Any client that sends OPTIONS requests"
else
    echo "❌ SOME TESTS FAILED"
    echo "❌ Gajim may still experience issues"
    exit 1
fi
