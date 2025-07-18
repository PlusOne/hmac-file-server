#!/bin/bash
# HMAC File Server - Multi-Architecture Build Script

set -e

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
CYAN='\033[0;36m'
NC='\033[0m'

print_status() {
    echo -e "${GREEN}[BUILD]${NC} $1"
}

print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_menu() {
    echo -e "${CYAN}[MENU]${NC} $1"
}

# Check if Go is installed
if ! command -v go &> /dev/null; then
    print_error "Go is not installed or not in PATH"
    exit 1
fi

# Architecture selection menu
print_status "HMAC File Server v3.2 - Multi-Architecture Build"
echo ""
print_menu "Select target architecture:"
echo "  1) amd64   (x86_64 - Intel/AMD 64-bit)"
echo "  2) arm64   (ARM 64-bit - Apple M1/M2, Raspberry Pi 4+)"
echo "  3) arm32   (ARM 32-bit - Raspberry Pi 3 and older)"
echo "  4) all     (Build all architectures)"
echo "  5) native  (Build for current system)"
echo ""

# Get user choice
read -p "Enter your choice (1-5): " choice

case $choice in
    1)
        GOOS="linux"
        GOARCH="amd64"
        SUFFIX="_amd64"
        print_info "Selected: AMD64 (x86_64)"
        ;;
    2)
        GOOS="linux"
        GOARCH="arm64"
        SUFFIX="_arm64"
        print_info "Selected: ARM64"
        ;;
    3)
        GOOS="linux"
        GOARCH="arm"
        GOARM="7"
        SUFFIX="_arm32"
        print_info "Selected: ARM32 (ARMv7)"
        ;;
    4)
        print_info "Selected: Build all architectures"
        BUILD_ALL=true
        ;;
    5)
        print_info "Selected: Native build (current system)"
        SUFFIX=""
        ;;
    *)
        print_error "Invalid choice. Exiting."
        exit 1
        ;;
esac

# Function to build for a specific architecture
build_for_arch() {
    local goos=$1
    local goarch=$2
    local goarm=$3
    local suffix=$4
    local output_name="hmac-file-server${suffix}"
    
    print_status "Building for ${goos}/${goarch}${goarm:+v$goarm}..."
    
    # Set environment variables
    export GOOS=$goos
    export GOARCH=$goarch
    if [ -n "$goarm" ]; then
        export GOARM=$goarm
    else
        unset GOARM
    fi
    
    # Build with core files and any available network resilience files
    go build -o "$output_name" cmd/server/main.go cmd/server/helpers.go cmd/server/config_validator.go cmd/server/config_test_scenarios.go $NEW_FILES
    
    if [ $? -eq 0 ]; then
        print_status "Build successful! Binary created: ./$output_name"
        
        # Check binary size
        SIZE=$(du -h "$output_name" | cut -f1)
        print_info "Binary size: $SIZE"
        
        # Only test functionality for native builds
        if [ "$goos" == "$(go env GOOS)" ] && [ "$goarch" == "$(go env GOARCH)" ]; then
            print_info "Testing binary functionality..."
            ./"$output_name" --help > /dev/null 2>&1
            if [ $? -eq 0 ]; then
                print_status "Binary is functional!"
            else
                print_warning "Binary test failed (may be cross-compiled)"
            fi
        else
            print_info "Cross-compiled binary created (functionality test skipped)"
        fi
    else
        print_error "Build failed for ${goos}/${goarch}!"
        return 1
    fi
    
    # Reset environment
    unset GOOS GOARCH GOARM
}

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

echo ""

# Build based on selection
if [ "$BUILD_ALL" = true ]; then
    print_status "Building all architectures..."
    echo ""
    
    # Build AMD64
    build_for_arch "linux" "amd64" "" "_amd64"
    echo ""
    
    # Build ARM64
    build_for_arch "linux" "arm64" "" "_arm64"
    echo ""
    
    # Build ARM32
    build_for_arch "linux" "arm" "7" "_arm32"
    echo ""
    
    print_status "All builds completed!"
    echo ""
    print_info "Created binaries:"
    ls -la hmac-file-server_*
    
elif [ -n "$GOOS" ] && [ -n "$GOARCH" ]; then
    # Single architecture build
    build_for_arch "$GOOS" "$GOARCH" "$GOARM" "$SUFFIX"
else
    # Native build
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
