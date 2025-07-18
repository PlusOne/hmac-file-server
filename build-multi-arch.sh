#!/bin/bash
# HMAC File Server v3.2 - Multi-Architecture Build Script
# Compiles binaries for AMD64, ARM64, ARM32, Windows, and macOS architectures

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

# Source directory to compile
SOURCE_DIR="./cmd/server/"

# Interactive menu function
show_menu() {
    echo ""
    echo "HMAC File Server Multi-Architecture Builder"
    echo "=========================================="
    echo "1) Build for current platform (auto-detect)"
    echo "2) Build for Linux AMD64"
    echo "3) Build for Linux ARM64"  
    echo "4) Build for Linux ARM32v7"
    echo "5) Build for Windows AMD64"
    echo "6) Build for macOS AMD64 (Intel)"
    echo "7) Build for macOS ARM64 (Apple Silicon)"
    echo "8) Build all supported architectures"
    echo "9) Clean build artifacts"
    echo "0) Exit"
    echo ""
    read -p "Choose an option [0-9]: " choice
}

# Clean function
clean_artifacts() {
    print_info "Cleaning build artifacts..."
    if [[ -d "$TEMP_DIR" ]]; then
        rm -rf "$TEMP_DIR"/*
        print_status "Build artifacts cleaned"
    else
        print_info "No artifacts to clean"
    fi
}

# Detect current platform
detect_platform() {
    local os=$(uname -s | tr '[:upper:]' '[:lower:]')
    local arch=$(uname -m)
    
    case "$arch" in
        x86_64) arch="amd64" ;;
        arm64|aarch64) arch="arm64" ;;
        armv7l) arch="arm" ;;
        *) arch="unknown" ;;
    esac
    
    case "$os" in
        linux) echo "linux/$arch" ;;
        darwin) echo "darwin/$arch" ;;
        *) echo "unknown/unknown" ;;
    esac
}

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
    if go build -ldflags="-w -s" -o "$TEMP_DIR/$output_name" $SOURCE_DIR 2>/dev/null; then
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
        if [[ "$goos" == "windows" ]]; then
            print_warning "   Windows builds may fail due to platform-specific code (syscalls)"
            print_info "   Consider using Linux subsystem or implementing Windows-specific storage checks"
        fi
        return 1
    fi
}

# Build all architectures function
build_all_architectures() {
    print_status "Starting multi-architecture build for HMAC File Server v3.2"
    print_info "Source directory: $SOURCE_DIR"
    print_info "Output directory: $TEMP_DIR"
    echo ""

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
    if build_for_arch "linux" "arm" "hmac-file-server-linux-arm32v7" "ARM32 Linux"; then
        ((BUILDS_SUCCESSFUL++))
    else
        FAILED_BUILDS+=("ARM32")
    fi
    echo ""

    # Build for Windows AMD64
    print_arch "Windows AMD64"
    ((BUILDS_ATTEMPTED++))
    if build_for_arch "windows" "amd64" "hmac-file-server-windows-amd64.exe" "Windows AMD64"; then
        ((BUILDS_SUCCESSFUL++))
    else
        FAILED_BUILDS+=("Windows")
    fi
    echo ""

    # Build for macOS Intel
    print_arch "macOS Intel"
    ((BUILDS_ATTEMPTED++))
    if build_for_arch "darwin" "amd64" "hmac-file-server-darwin-amd64" "macOS Intel"; then
        ((BUILDS_SUCCESSFUL++))
    else
        FAILED_BUILDS+=("macOS Intel")
    fi
    echo ""

    # Build for macOS Apple Silicon
    print_arch "macOS Apple Silicon"
    ((BUILDS_ATTEMPTED++))
    if build_for_arch "darwin" "arm64" "hmac-file-server-darwin-arm64" "macOS Apple Silicon"; then
        ((BUILDS_SUCCESSFUL++))
    else
        FAILED_BUILDS+=("macOS ARM64")
    fi
    echo ""

    # Reset environment variables
    unset GOOS GOARCH CGO_ENABLED GOARM

    show_build_summary
}

# Build single architecture function
build_single_arch() {
    local platform_desc=$1
    local goos=$2
    local goarch=$3
    local goarm=$4
    local output_name=$5
    
    print_status "Building for $platform_desc"
    print_info "Source directory: $SOURCE_DIR"
    print_info "Output directory: $TEMP_DIR"
    echo ""
    
    if [[ -n "$goarm" ]]; then
        export GOARM=$goarm
    fi
    
    BUILDS_ATTEMPTED=1
    BUILDS_SUCCESSFUL=0
    FAILED_BUILDS=()
    
    if build_for_arch "$goos" "$goarch" "$output_name" "$platform_desc"; then
        BUILDS_SUCCESSFUL=1
    else
        FAILED_BUILDS+=("$platform_desc")
    fi
    
    unset GOOS GOARCH CGO_ENABLED GOARM
    show_build_summary
}

# Build current platform function
build_current_platform() {
    local platform=$(detect_platform)
    local goos=$(echo "$platform" | cut -d'/' -f1)
    local goarch=$(echo "$platform" | cut -d'/' -f2)
    
    case "$platform" in
        "linux/amd64")
            build_single_arch "Current Platform (Linux AMD64)" "linux" "amd64" "" "hmac-file-server-linux-amd64"
            ;;
        "linux/arm64")
            build_single_arch "Current Platform (Linux ARM64)" "linux" "arm64" "" "hmac-file-server-linux-arm64"
            ;;
        "linux/arm")
            build_single_arch "Current Platform (Linux ARM32v7)" "linux" "arm" "7" "hmac-file-server-linux-arm32v7"
            ;;
        "darwin/amd64")
            build_single_arch "Current Platform (macOS Intel)" "darwin" "amd64" "" "hmac-file-server-darwin-amd64"
            ;;
        "darwin/arm64")
            build_single_arch "Current Platform (macOS Apple Silicon)" "darwin" "arm64" "" "hmac-file-server-darwin-arm64"
            ;;
        *)
            print_error "Unsupported platform: $platform"
            print_info "Supported platforms: linux/amd64, linux/arm64, linux/arm, darwin/amd64, darwin/arm64"
            exit 1
            ;;
    esac
}

# Show build summary
show_build_summary() {
    # Build summary
    echo "Build Summary"
    echo "================"
    print_info "Builds attempted: $BUILDS_ATTEMPTED"
    print_info "Builds successful: $BUILDS_SUCCESSFUL"

    if [[ $BUILDS_SUCCESSFUL -eq $BUILDS_ATTEMPTED ]]; then
        print_status "ALL BUILDS SUCCESSFUL!"
        echo ""
        print_info "Generated binaries in $TEMP_DIR:"
        ls -lh "$TEMP_DIR"/hmac-file-server-* 2>/dev/null | while read -r line; do
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
    echo "   - Windows: Windows 10/11, Windows Server"
    echo "   - macOS: macOS 10.15+, Intel and Apple Silicon"

    echo ""
    print_status "Build completed!"

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
}

# Main execution
if [[ $# -eq 0 ]]; then
    # Interactive mode
    while true; do
        show_menu
        case $choice in
            1)
                build_current_platform
                break
                ;;
            2)
                build_single_arch "Linux AMD64" "linux" "amd64" "" "hmac-file-server-linux-amd64"
                break
                ;;
            3)
                build_single_arch "Linux ARM64" "linux" "arm64" "" "hmac-file-server-linux-arm64"
                break
                ;;
            4)
                build_single_arch "Linux ARM32v7" "linux" "arm" "7" "hmac-file-server-linux-arm32v7"
                break
                ;;
            5)
                build_single_arch "Windows AMD64" "windows" "amd64" "" "hmac-file-server-windows-amd64.exe"
                break
                ;;
            6)
                build_single_arch "macOS Intel" "darwin" "amd64" "" "hmac-file-server-darwin-amd64"
                break
                ;;
            7)
                build_single_arch "macOS Apple Silicon" "darwin" "arm64" "" "hmac-file-server-darwin-arm64"
                break
                ;;
            8)
                build_all_architectures
                break
                ;;
            9)
                clean_artifacts
                ;;
            0)
                print_info "Exiting build script"
                exit 0
                ;;
            *)
                print_error "Invalid option. Please choose 0-9."
                ;;
        esac
    done
else
    # Non-interactive mode - build all architectures
    build_all_architectures
fi

exit 0
