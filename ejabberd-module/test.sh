#!/bin/bash

# HMAC File Server - Ejabberd Integration Test Script
# Tests Bearer token authentication and upload functionality

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_test() {
    echo -e "${BLUE}🧪 TEST: $1${NC}"
}

print_pass() {
    echo -e "${GREEN}✅ PASS: $1${NC}"
}

print_fail() {
    echo -e "${RED}❌ FAIL: $1${NC}"
}

print_info() {
    echo -e "${YELLOW}ℹ️  INFO: $1${NC}"
}

# Test configuration
HMAC_SERVER_URL="http://localhost:8080"
TEST_USER="testuser@example.org"
TEST_FILENAME="test-upload.txt"
TEST_CONTENT="Hello from ejabberd module test!"
SHARED_SECRET="test-secret-123"

generate_bearer_token() {
    local user="$1"
    local filename="$2"
    local size="$3"
    local timestamp="$4"
    
    # Create payload: user + filename + size + timestamp
    local payload="${user}\x00${filename}\x00${size}\x00${timestamp}"
    
    # Generate HMAC and encode as base64
    echo -n "$payload" | openssl dgst -sha256 -hmac "$SHARED_SECRET" -binary | base64 -w 0
}

test_bearer_token_generation() {
    print_test "Bearer token generation"
    
    local timestamp=$(date +%s)
    local size=${#TEST_CONTENT}
    
    TOKEN=$(generate_bearer_token "$TEST_USER" "$TEST_FILENAME" "$size" "$timestamp")
    
    if [ -n "$TOKEN" ]; then
        print_pass "Token generated: ${TOKEN:0:20}..."
        echo "TOKEN=$TOKEN"
        echo "TIMESTAMP=$timestamp"
        echo "SIZE=$size"
        return 0
    else
        print_fail "Token generation failed"
        return 1
    fi
}

test_hmac_server_health() {
    print_test "HMAC server health check"
    
    if curl -s "$HMAC_SERVER_URL/health" >/dev/null 2>&1; then
        print_pass "HMAC server is running"
        return 0
    else
        print_fail "HMAC server is not responding"
        return 1
    fi
}

test_bearer_token_upload() {
    print_test "Bearer token upload simulation"
    
    local timestamp=$(date +%s)
    local expiry=$((timestamp + 3600))
    local size=${#TEST_CONTENT}
    local uuid=$(uuidgen 2>/dev/null || echo "test-uuid-12345")
    
    TOKEN=$(generate_bearer_token "$TEST_USER" "$TEST_FILENAME" "$size" "$timestamp")
    
    # Create upload URL with Bearer token parameters
    local upload_url="${HMAC_SERVER_URL}/upload/${uuid}/${TEST_FILENAME}?token=${TOKEN}&user=${TEST_USER}&expiry=${expiry}"
    
    print_info "Upload URL: $upload_url"
    print_info "Token: ${TOKEN:0:30}..."
    
    # Test upload with Bearer token
    local response=$(curl -s -w "%{http_code}" \
        -X PUT \
        -H "Authorization: Bearer $TOKEN" \
        -H "Content-Type: text/plain" \
        -H "Content-Length: $size" \
        -d "$TEST_CONTENT" \
        "$upload_url" 2>/dev/null || echo "000")
    
    local http_code="${response: -3}"
    
    if [ "$http_code" = "201" ] || [ "$http_code" = "200" ]; then
        print_pass "Bearer token upload successful (HTTP $http_code)"
        return 0
    else
        print_fail "Bearer token upload failed (HTTP $http_code)"
        print_info "Response: ${response%???}"  # Remove last 3 chars (HTTP code)
        return 1
    fi
}

test_xep0363_slot_request() {
    print_test "XEP-0363 slot request simulation"
    
    # This would normally be handled by ejabberd module
    # We'll simulate the XML response format
    
    local timestamp=$(date +%s)
    local expiry=$((timestamp + 3600))
    local size=1024
    local uuid=$(uuidgen 2>/dev/null || echo "test-uuid-67890")
    
    TOKEN=$(generate_bearer_token "$TEST_USER" "$TEST_FILENAME" "$size" "$timestamp")
    
    local put_url="${HMAC_SERVER_URL}/upload/${uuid}/${TEST_FILENAME}?token=${TOKEN}&user=${TEST_USER}&expiry=${expiry}"
    local get_url="${HMAC_SERVER_URL}/download/${uuid}/${TEST_FILENAME}"
    
    # Generate XEP-0363 slot response XML
    cat << EOF
<slot xmlns='urn:xmpp:http:upload:0'>
  <put url='$put_url'>
    <header name='Authorization'>Bearer $TOKEN</header>
    <header name='Content-Type'>text/plain</header>
  </put>
  <get url='$get_url'/>
</slot>
EOF

    print_pass "XEP-0363 slot response generated"
    return 0
}

test_ejabberd_module() {
    print_test "Ejabberd module status"
    
    if command -v ejabberdctl &> /dev/null; then
        if ejabberdctl status >/dev/null 2>&1; then
            print_pass "ejabberd is running"
            
            # Check if our module is available
            if ejabberdctl modules 2>/dev/null | grep -q "mod_http_upload"; then
                print_pass "HTTP upload module detected"
            else
                print_info "No HTTP upload module detected (manual check required)"
            fi
        else
            print_fail "ejabberd is not running"
            return 1
        fi
    else
        print_info "ejabberdctl not found (ejabberd may not be installed)"
        return 1
    fi
}

run_integration_test() {
    print_test "Full integration test"
    
    echo -e "${BLUE}Step 1: Generate token${NC}"
    test_bearer_token_generation
    
    echo -e "${BLUE}Step 2: Test server health${NC}"
    test_hmac_server_health
    
    echo -e "${BLUE}Step 3: Simulate XEP-0363 slot${NC}"
    test_xep0363_slot_request
    
    echo -e "${BLUE}Step 4: Test Bearer upload${NC}"
    test_bearer_token_upload
    
    print_pass "Integration test completed"
}

print_usage() {
    echo "Usage: $0 [test_name]"
    echo
    echo "Available tests:"
    echo "  token     - Test Bearer token generation"
    echo "  health    - Test HMAC server health"
    echo "  upload    - Test Bearer token upload"
    echo "  slot      - Test XEP-0363 slot generation"
    echo "  ejabberd  - Test ejabberd module status"
    echo "  all       - Run all tests (default)"
    echo
    echo "Environment variables:"
    echo "  HMAC_SERVER_URL  - HMAC server URL (default: http://localhost:8080)"
    echo "  SHARED_SECRET    - Shared secret (default: test-secret-123)"
    echo "  TEST_USER        - Test user JID (default: testuser@example.org)"
}

main() {
    echo -e "${BLUE}╔══════════════════════════════════════════╗"
    echo -e "║    HMAC File Server - Ejabberd Tests    ║"
    echo -e "╚══════════════════════════════════════════╝${NC}"
    echo
    
    case "${1:-all}" in
        "token")
            test_bearer_token_generation
            ;;
        "health")
            test_hmac_server_health
            ;;
        "upload")
            test_bearer_token_upload
            ;;
        "slot")
            test_xep0363_slot_request
            ;;
        "ejabberd")
            test_ejabberd_module
            ;;
        "all")
            run_integration_test
            ;;
        "help"|"-h"|"--help")
            print_usage
            ;;
        *)
            print_fail "Unknown test: $1"
            print_usage
            exit 1
            ;;
    esac
}

main "$@"
