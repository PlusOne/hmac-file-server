#!/bin/bash
# HMAC File Server - Build Script

set -e

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

print_status() {
    echo -e "${GREEN}[BUILD]${NC} $1"
}

print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if Go is installed
if ! command -v go &> /dev/null; then
    print_error "Go is not installed or not in PATH"
    exit 1
fi

# Build the application
print_status "Building HMAC File Server v3.2 with Network Resilience..."

# Check if new network resilience files exist
NEW_FILES=""
if [ -f "cmd/server/upload_session.go" ]; then
    NEW_FILES="$NEW_FILES cmd/server/upload_session.go"
    print_info "Found network resilience: upload_session.go"
fi
if [ -f "cmd/server/network_resilience.go" ]; then
    NEW_FILES="$NEW_FILES cmd/server/network_resilience.go"
    print_info "Found network resilience: network_resilience.go"
fi
if [ -f "cmd/server/chunked_upload_handler.go" ]; then
    NEW_FILES="$NEW_FILES cmd/server/chunked_upload_handler.go"
    print_info "Found network resilience: chunked_upload_handler.go"
fi
if [ -f "cmd/server/integration.go" ]; then
    NEW_FILES="$NEW_FILES cmd/server/integration.go"
    print_info "Found network resilience: integration.go"
fi

# Build with core files and any available network resilience files
go build -o hmac-file-server cmd/server/main.go cmd/server/helpers.go cmd/server/config_validator.go cmd/server/config_test_scenarios.go $NEW_FILES

if [ $? -eq 0 ]; then
    print_status "Build successful! Binary created: ./hmac-file-server"
    
    # Check binary size
    SIZE=$(du -h hmac-file-server | cut -f1)
    print_info "Binary size: $SIZE"
    
    # Show help to verify it works
    print_info "Testing binary functionality..."
    ./hmac-file-server --help > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        print_status "Binary is functional!"
    else
        print_error "Binary test failed"
        exit 1
    fi
else
    print_error "Build failed!"
    exit 1
fi

# Create test file for manual testing
print_info "Creating test file..."
echo "Hello, HMAC File Server! $(date)" > test_upload.txt

# Generate HMAC signature for manual testing
print_info "HMAC signature generation for testing:"
SECRET="hmac-file-server-is-the-win"
MESSAGE="/upload"

# Check if openssl is available
if command -v openssl &> /dev/null; then
    SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "$SECRET" | cut -d' ' -f2)
    echo "Secret: $SECRET"
    echo "Message: $MESSAGE"
    echo "Signature: $SIGNATURE"
    echo ""
    echo "Test with curl (requires server running on localhost:8080):"
    echo "curl -v -X POST -H \"X-Signature: $SIGNATURE\" -F \"file=@test_upload.txt\" http://localhost:8080/upload"
else
    print_info "OpenSSL not found. You can generate HMAC manually or use the Go tests."
    echo "To start server: ./hmac-file-server"
    echo "For testing, check the test/ directory for Go test files."
fi

print_status "Build complete! Ready to run: ./hmac-file-server"
