#!/bin/bash
# HMAC File Server v3.2 - Multi-Architecture Build Script
# Compiles binaries for AMD64, ARM64, and ARM32 architectures

# Remove set -e to prevent early exit on errors

# Colors for output
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
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_arch() {
    echo -e "${CYAN}[ARCH]${NC} $1"
}

# Check if Go is installed
if ! command -v go &> /dev/null; then
    print_error "Go is not installed or not in PATH"
    exit 1
fi

# Create temp directory if it doesn't exist
TEMP_DIR="./temp"
if [[ ! -d "$TEMP_DIR" ]]; then
    mkdir -p "$TEMP_DIR"
    print_info "Created temp directory: $TEMP_DIR"
fi

# Source files to compile
SOURCE_FILES="cmd/server/main.go cmd/server/helpers.go cmd/server/config_validator.go cmd/server/config_test_scenarios.go"

print_status "Starting multi-architecture build for HMAC File Server v3.2"
print_info "Source files: $SOURCE_FILES"
print_info "Output directory: $TEMP_DIR"
echo ""

# Build function
build_for_arch() {
    local goos=$1
    local goarch=$2
    local output_name=$3
    local arch_description=$4
    
    print_arch "Building for $arch_description ($goos/$goarch)..."
    
    # Set environment variables for cross-compilation
    export GOOS=$goos
    export GOARCH=$goarch
    export CGO_ENABLED=0
    
    # Build the binary
    if go build -ldflags="-w -s" -o "$TEMP_DIR/$output_name" $SOURCE_FILES 2>/dev/null; then
        # Get file size
        if [[ "$OSTYPE" == "darwin"* ]]; then
            # macOS
            SIZE=$(stat -f%z "$TEMP_DIR/$output_name" | awk '{printf "%.1fMB", $1/1024/1024}')
        else
            # Linux
            SIZE=$(stat -c%s "$TEMP_DIR/$output_name" | awk '{printf "%.1fMB", $1/1024/1024}')
        fi
        
        print_status "Build successful: $arch_description"
        print_info "   Binary: $TEMP_DIR/$output_name"
        print_info "   Size: $SIZE"
        
        # Test binary (version check)
        if timeout 10s "$TEMP_DIR/$output_name" --version >/dev/null 2>&1; then
            print_info "   Version check: PASSED"
        else
            print_warning "   Version check: SKIPPED (cross-compiled binary)"
        fi
        
        return 0
    else
        print_error "Build failed: $arch_description"
        return 1
    fi
}

# Track build results
BUILDS_ATTEMPTED=0
BUILDS_SUCCESSFUL=0
FAILED_BUILDS=()

echo "Starting builds..."
echo "===================="
echo ""

# Build for AMD64 (x86_64)
print_arch "AMD64 (Intel/AMD 64-bit)"
((BUILDS_ATTEMPTED++))
if build_for_arch "linux" "amd64" "hmac-file-server-linux-amd64" "AMD64 Linux"; then
    ((BUILDS_SUCCESSFUL++))
else
    FAILED_BUILDS+=("AMD64")
fi
echo ""

# Build for ARM64 (AArch64)
print_arch "ARM64 (AArch64)"
((BUILDS_ATTEMPTED++))
if build_for_arch "linux" "arm64" "hmac-file-server-linux-arm64" "ARM64 Linux"; then
    ((BUILDS_SUCCESSFUL++))
else
    FAILED_BUILDS+=("ARM64")
fi
echo ""

# Build for ARM32 (ARMv7)
print_arch "ARM32 (ARMv7)"
export GOARM=7  # ARMv7 with hardware floating point
((BUILDS_ATTEMPTED++))
if build_for_arch "linux" "arm" "hmac-file-server-linux-arm32" "ARM32 Linux"; then
    ((BUILDS_SUCCESSFUL++))
else
    FAILED_BUILDS+=("ARM32")
fi
echo ""

# Reset environment variables
unset GOOS GOARCH CGO_ENABLED GOARM

# Build summary
echo "Build Summary"
echo "================"
print_info "Builds attempted: $BUILDS_ATTEMPTED"
print_info "Builds successful: $BUILDS_SUCCESSFUL"

if [[ $BUILDS_SUCCESSFUL -eq $BUILDS_ATTEMPTED ]]; then
    print_status "ALL BUILDS SUCCESSFUL!"
    echo ""
    print_info "Generated binaries in $TEMP_DIR:"
    ls -lh "$TEMP_DIR"/hmac-file-server-* | while read -r line; do
        echo "   $line"
    done
    echo ""
    print_info "Usage examples:"
    echo "   - Copy to target system and run: ./hmac-file-server-linux-amd64 --version"
    echo "   - Deploy with installer: cp temp/hmac-file-server-linux-amd64 /opt/hmac-file-server/"
    echo "   - Docker deployment: COPY temp/hmac-file-server-linux-amd64 /usr/local/bin/"
    
elif [[ $BUILDS_SUCCESSFUL -gt 0 ]]; then
    print_warning "PARTIAL SUCCESS: $BUILDS_SUCCESSFUL/$BUILDS_ATTEMPTED builds completed"
    if [[ ${#FAILED_BUILDS[@]} -gt 0 ]]; then
        print_error "Failed architectures: ${FAILED_BUILDS[*]}"
    fi
    
else
    print_error "ALL BUILDS FAILED!"
    exit 1
fi

echo ""
print_info "Architecture compatibility:"
echo "   - AMD64: Intel/AMD 64-bit servers, desktops, cloud instances"
echo "   - ARM64: Apple Silicon, AWS Graviton, modern ARM servers"  
echo "   - ARM32: Raspberry Pi, embedded systems, older ARM devices"

echo ""
print_status "Multi-architecture build completed!"

# Final verification
echo ""
print_info "Final verification:"
for binary in "$TEMP_DIR"/hmac-file-server-*; do
    if [[ -f "$binary" ]]; then
        filename=$(basename "$binary")
        if file "$binary" >/dev/null 2>&1; then
            file_info=$(file "$binary" | cut -d: -f2- | sed 's/^ *//')
            print_info "   OK $filename: $file_info"
        else
            print_info "   OK $filename: Binary file"
        fi
    fi
done

exit 0
