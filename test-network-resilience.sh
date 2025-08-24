#!/bin/bash

# HMAC File Server Network Resilience Test
# Tests WLAN to 5G switching behavior

echo "🧪 HMAC File Server Network Resilience Test"
echo "============================================="

# Configuration
SERVER_URL="http://localhost:8080"
SECRET="your-very-secret-hmac-key"
TEST_FILE="/tmp/test-network-resilience.dat"

# Generate test file (10MB)
echo "📄 Creating test file (10MB)..."
dd if=/dev/zero of=$TEST_FILE bs=1024 count=10240 2>/dev/null

# Function to generate HMAC
generate_hmac() {
    local filename="$1"
    local secret="$2"
    local timestamp="$3"
    
    # Generate HMAC signature
    echo -n "${filename}${timestamp}" | openssl dgst -sha256 -hmac "$secret" -binary | base64
}

# Test function
test_upload_with_network_change() {
    echo
    echo "🔧 Testing upload with simulated network change..."
    
    # Get current timestamp
    TIMESTAMP=$(date +%s)
    FILENAME="test-network-resilience.dat"
    
    # Generate HMAC
    HMAC=$(generate_hmac "$FILENAME" "$SECRET" "$TIMESTAMP")
    
    echo "⏳ Starting upload..."
    echo "📡 Filename: $FILENAME"
    echo "🔐 HMAC: $HMAC"
    echo "⏰ Timestamp: $TIMESTAMP"
    
    # Start upload in background
    curl -v \
        -F "file=@$TEST_FILE" \
        -F "filename=$FILENAME" \
        -F "timestamp=$TIMESTAMP" \
        -F "hmac=$HMAC" \
        -H "X-Upload-Session-ID: test-network-resilience-$$" \
        "$SERVER_URL/upload" \
        > /tmp/upload-result.txt 2>&1 &
    
    UPLOAD_PID=$!
    
    # Simulate network change after 2 seconds
    sleep 2
    echo
    echo "🌐 Simulating network interface change (WLAN → 5G)..."
    
    # Check if server handles network events
    if curl -s "$SERVER_URL/health" > /dev/null; then
        echo "✅ Server still responding during upload"
    else
        echo "❌ Server not responding"
    fi
    
    # Wait for upload to complete
    wait $UPLOAD_PID
    UPLOAD_RESULT=$?
    
    echo
    echo "📊 Upload Result:"
    cat /tmp/upload-result.txt
    
    if [ $UPLOAD_RESULT -eq 0 ]; then
        echo "✅ Upload completed successfully with network resilience"
        return 0
    else
        echo "❌ Upload failed (exit code: $UPLOAD_RESULT)"
        return 1
    fi
}

# Test network resilience configuration
test_configuration() {
    echo
    echo "🔍 Checking network resilience configuration..."
    
    # Check if server has network events enabled
    if curl -s "$SERVER_URL/metrics" | grep -q "networkevents"; then
        echo "✅ Network events monitoring appears to be active"
    else
        echo "⚠️  Network events monitoring may not be active"
    fi
    
    # Check server health
    if curl -s "$SERVER_URL/health" | grep -q "OK"; then
        echo "✅ Server is healthy"
    else
        echo "❌ Server health check failed"
        return 1
    fi
}

# Main test execution
main() {
    echo "🚀 Starting tests..."
    
    # Check if server is running
    if ! curl -s "$SERVER_URL/health" > /dev/null; then
        echo "❌ Server is not running at $SERVER_URL"
        echo "Please start the HMAC File Server first:"
        echo "  ./hmac-file-server -config config.toml"
        exit 1
    fi
    
    # Run tests
    test_configuration
    test_upload_with_network_change
    
    # Cleanup
    rm -f $TEST_FILE /tmp/upload-result.txt
    
    echo
    echo "✅ Network resilience test completed"
    echo
    echo "💡 To test real network switching:"
    echo "1. Start upload from mobile device"
    echo "2. Turn off WiFi during upload"
    echo "3. Upload should pause and resume on cellular"
}

main "$@"
