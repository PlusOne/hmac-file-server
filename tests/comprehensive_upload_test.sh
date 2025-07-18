#!/bin/bash

# Comprehensive XMPP Upload Test Script
# Tests multiple upload scenarios with real-time debugging

echo "=== COMPREHENSIVE UPLOAD TEST SCRIPT ==="
echo "This script will test multiple upload scenarios while monitoring logs"
echo "Date: $(date)"
echo ""

# Configuration
SERVER_URL="https://share.uuxo.net"
LOCAL_URL="http://localhost:8080"
SECRET="f6g4ldPvQM7O2UTFeBEUUj33VrXypDAcsDt0yqKrLiOr5oQW"
TEST_DIR="/tmp/upload_tests"

# Create test directory
mkdir -p "$TEST_DIR"
cd "$TEST_DIR"

# Function to generate HMAC signature for v3 protocol
generate_v3_signature() {
    local method="$1"
    local expires="$2"
    local path="$3"
    local message="${method}\n${expires}\n${path}"
    echo -n "$message" | openssl dgst -sha256 -hmac "$SECRET" -hex | cut -d' ' -f2
}

# Function to start log monitoring
start_monitoring() {
    echo "Starting log monitoring in background..."
    
    # Kill any existing monitoring
    pkill -f "tail.*hmac-file-server" 2>/dev/null
    pkill -f "tail.*nginx.*share" 2>/dev/null
    
    # Start nginx monitoring
    echo "=== NGINX ACCESS LOG ===" > /tmp/nginx_monitor.log
    sudo tail -f /var/log/nginx/share_access.log >> /tmp/nginx_monitor.log 2>&1 &
    NGINX_PID=$!
    
    # Start server monitoring  
    echo "=== HMAC SERVER LOG ===" > /tmp/server_monitor.log
    sudo tail -f /var/log/hmac-file-server/hmac-file-server.log >> /tmp/server_monitor.log 2>&1 &
    SERVER_PID=$!
    
    sleep 1
    echo "Monitoring started (nginx PID: $NGINX_PID, server PID: $SERVER_PID)"
}

# Function to stop monitoring and show results
stop_monitoring() {
    echo "Stopping monitors..."
    kill $NGINX_PID $SERVER_PID 2>/dev/null
    sleep 1
    
    echo ""
    echo "=== NGINX LOG RESULTS ==="
    tail -10 /tmp/nginx_monitor.log 2>/dev/null || echo "No nginx activity detected"
    
    echo ""
    echo "=== SERVER LOG RESULTS ==="
    tail -10 /tmp/server_monitor.log 2>/dev/null || echo "No server activity detected"
    echo ""
}

# Function to create test files
create_test_files() {
    echo "Creating test files..."
    
    # Small file (1KB)
    echo "This is a small test file for upload testing" > small_test.txt
    echo "Content: Basic text file" >> small_test.txt
    
    # Medium file (1MB) 
    dd if=/dev/zero of=medium_test.bin bs=1024 count=1024 2>/dev/null
    
    # Large file (5MB)
    dd if=/dev/zero of=large_test.bin bs=1024 count=5120 2>/dev/null
    
    # Video file simulation (1MB with .mp4 extension)
    cp medium_test.bin test_video.mp4
    
    echo "Test files created:"
    ls -lh *.txt *.bin *.mp4 2>/dev/null
    echo ""
}

# Function to test different upload protocols
test_upload_protocol() {
    local protocol="$1"
    local filename="$2"
    local description="$3"
    
    echo "--- Testing $protocol Protocol: $description ---"
    
    # Generate test parameters
    local expires=$(date -d "+1 hour" +%s)
    local path="/test_${protocol}/${filename}"
    local url=""
    local signature=""
    
    case "$protocol" in
        "v3")
            signature=$(generate_v3_signature "PUT" "$expires" "$path")
            url="${SERVER_URL}${path}?v3=${signature}&expires=${expires}"
            ;;
        "v2")
            signature=$(echo -n "PUT${path}" | openssl dgst -sha256 -hmac "$SECRET" -hex | cut -d' ' -f2)
            url="${SERVER_URL}${path}?v2=${signature}"
            ;;
        "v1")
            signature=$(echo -n "PUT${path}" | openssl dgst -sha256 -hmac "$SECRET" -hex | cut -d' ' -f2)
            url="${SERVER_URL}${path}?v=${signature}"
            ;;
        "token")
            signature=$(echo -n "PUT${path}" | openssl dgst -sha256 -hmac "$SECRET" -hex | cut -d' ' -f2)
            url="${SERVER_URL}${path}?token=${signature}"
            ;;
    esac
    
    echo "URL: $url"
    echo "File: $filename ($(stat -f%z "$filename" 2>/dev/null || stat -c%s "$filename")bytes)"
    
    # Start monitoring for this test
    echo "Starting upload test..."
    
    # Perform upload
    local start_time=$(date +%s.%N)
    local response=$(curl -s -w "HTTPSTATUS:%{http_code};TIME:%{time_total}" \
                          -X PUT \
                          --data-binary "@$filename" \
                          -H "User-Agent: XMPP-Upload-Test/1.0" \
                          -H "Content-Type: application/octet-stream" \
                          "$url" 2>&1)
    local end_time=$(date +%s.%N)
    
    # Parse response
    local http_code=$(echo "$response" | grep -o "HTTPSTATUS:[0-9]*" | cut -d: -f2)
    local time_total=$(echo "$response" | grep -o "TIME:[0-9.]*" | cut -d: -f2)
    local body=$(echo "$response" | sed 's/HTTPSTATUS:[0-9]*;TIME:[0-9.]*$//')
    
    # Calculate duration
    local duration=$(echo "$end_time - $start_time" | bc 2>/dev/null || echo "N/A")
    
    echo "Result: HTTP $http_code (${time_total}s)"
    if [[ "$http_code" =~ ^[45] ]]; then
        echo "Error body: $body"
    elif [[ "$http_code" == "200" ]]; then
        echo "✅ SUCCESS: Upload completed"
        echo "Response: $body"
    else
        echo "Response: $body"
    fi
    
    echo "Duration: ${duration}s"
    echo ""
    
    # Brief pause to separate log entries
    sleep 2
}

# Function to test deduplication
test_deduplication() {
    echo "--- Testing Deduplication ---"
    echo "Uploading the same file twice to test deduplication logic"
    
    # First upload
    echo "1. First upload (should create new file):"
    test_upload_protocol "v3" "small_test.txt" "Dedup Test #1"
    
    # Second upload (should deduplicate)
    echo "2. Second upload (should deduplicate):"
    test_upload_protocol "v3" "small_test.txt" "Dedup Test #2"
}

# Function to test storage scenarios
test_storage_scenarios() {
    echo "--- Testing Different Storage Scenarios ---"
    
    # Test small file
    test_upload_protocol "v3" "small_test.txt" "Small File (1KB)"
    
    # Test medium file  
    test_upload_protocol "v3" "medium_test.bin" "Medium File (1MB)"
    
    # Test video file
    test_upload_protocol "v3" "test_video.mp4" "Video File (.mp4)"
    
    # Test large file
    test_upload_protocol "v3" "large_test.bin" "Large File (5MB)"
}

# Function to test all protocols
test_all_protocols() {
    echo "--- Testing All XEP-0363 Protocol Variants ---"
    
    test_upload_protocol "v3" "small_test.txt" "XEP-0363 v3 (mod_http_upload_external)"
    test_upload_protocol "v2" "small_test.txt" "XEP-0363 v2 (extended)"  
    test_upload_protocol "v1" "small_test.txt" "XEP-0363 v1 (basic)"
    test_upload_protocol "token" "small_test.txt" "XEP-0363 token (alternative)"
}

# Function to show current configuration
show_configuration() {
    echo "=== Current Server Configuration ==="
    echo "Deduplication: $(sudo grep deduplication_enabled /etc/hmac-file-server/config.toml | cut -d'=' -f2 | tr -d ' ')"
    echo "Max Upload: $(sudo grep max_upload_size /etc/hmac-file-server/config.toml | cut -d'"' -f2)"
    echo "ClamAV: $(sudo grep clamavenabled /etc/hmac-file-server/config.toml | cut -d'=' -f2 | tr -d ' ')"
    echo "Global Extensions: $(sudo grep global_extensions /etc/hmac-file-server/config.toml | cut -d'[' -f2 | cut -d']' -f1)"
    echo "Log Level: $(sudo grep 'level =' /etc/hmac-file-server/config.toml | cut -d'"' -f2)"
    echo "Server Status: $(systemctl is-active hmac-file-server)"
    echo ""
}

# Function to cleanup
cleanup() {
    echo "Cleaning up..."
    stop_monitoring
    rm -rf "$TEST_DIR" 2>/dev/null
    echo "Cleanup complete"
}

# Trap for cleanup on exit
trap cleanup EXIT

# Main execution
main() {
    show_configuration
    create_test_files
    start_monitoring
    
    echo "=== STARTING COMPREHENSIVE UPLOAD TESTS ==="
    echo "Monitor logs in real-time:"
    echo "  nginx: tail -f /tmp/nginx_monitor.log"
    echo "  server: tail -f /tmp/server_monitor.log"
    echo ""
    
    # Test 1: Protocol variants
    echo "🔄 TEST 1: All Protocol Variants"
    test_all_protocols
    
    # Test 2: Storage scenarios
    echo "🔄 TEST 2: Storage Scenarios"  
    test_storage_scenarios
    
    # Test 3: Deduplication
    echo "🔄 TEST 3: Deduplication"
    test_deduplication
    
    echo "=== TEST SUMMARY ==="
    echo "All tests completed. Check the results above."
    echo "If you see HTTP 401 errors, that's expected (HMAC signature validation)."
    echo "If you see HTTP 200 responses, uploads are working!"
    echo "If you see no nginx log entries, requests aren't reaching the server."
    echo ""
    
    stop_monitoring
    
    echo "Log files saved to:"
    echo "  nginx: /tmp/nginx_monitor.log"
    echo "  server: /tmp/server_monitor.log"
}

# Run main function
main "$@"
