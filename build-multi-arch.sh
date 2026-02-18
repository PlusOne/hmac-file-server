#!/bin/bash
# HMAC File Server 3.4.0 "Cascade" - Multi-Architecture Builder
# Builds binaries for multiple architectures and platforms

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# Configuration
VERSION="3.4.0"
PROJECT_NAME="hmac-file-server"
BUILD_DIR="builds"
SOURCE_FILES="./cmd/server/"

# Supported architectures
declare -A PLATFORMS=(
    ["linux/amd64"]="Linux AMD64 (Intel/AMD 64-bit)"
    ["linux/arm64"]="Linux ARM64 (Apple Silicon, Raspberry Pi 4+)"
    ["linux/arm"]="Linux ARM32v7 (Raspberry Pi 3+)"
    ["linux/386"]="Linux 386 (32-bit Intel)"
    ["darwin/amd64"]="macOS Intel"
    ["darwin/arm64"]="macOS Apple Silicon"
    ["windows/amd64"]="Windows 64-bit"
    ["windows/386"]="Windows 32-bit"
    ["freebsd/amd64"]="FreeBSD AMD64"
    ["openbsd/amd64"]="OpenBSD AMD64"
)

# Functions
print_header() {
    echo -e "${BLUE}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║${NC} ${CYAN}HMAC File Server 3.4.0 'Cascade' Multi-Arch Builder${NC} ${BLUE}║${NC}"
    echo -e "${BLUE}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
}

print_info() {
    echo -e "${GREEN}✓${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

print_status() {
    echo -e "${PURPLE}▶${NC} $1"
}

build_binary() {
    local platform=$1
    local description=$2
    local goos=$(echo $platform | cut -d'/' -f1)
    local goarch=$(echo $platform | cut -d'/' -f2)
    
    local output_name="${PROJECT_NAME}-${goos}-${goarch}"
    if [ "$goos" = "windows" ]; then
        output_name="${output_name}.exe"
    fi
    
    local output_path="${BUILD_DIR}/${output_name}"
    
    print_status "Building for ${description} (${platform})..."
    
    # Set build environment
    export GOOS=$goos
    export GOARCH=$goarch
    export CGO_ENABLED=0
    
    # Build with optimizations
    if go build -ldflags="-w -s -X main.version=${VERSION}" -o "$output_path" $SOURCE_FILES; then
        # Get file size
        local size
        if command -v stat >/dev/null 2>&1; then
            if [[ "$OSTYPE" == "darwin"* ]]; then
                size=$(stat -f%z "$output_path" 2>/dev/null | awk '{printf "%.1fMB", $1/1024/1024}')
            else
                size=$(stat -c%s "$output_path" 2>/dev/null | awk '{printf "%.1fMB", $1/1024/1024}')
            fi
        else
            size="Unknown"
        fi
        
        print_info "   ✓ Built ${output_name} (${size})"
        return 0
    else
        print_error "   ✗ Failed to build ${output_name}"
        return 1
    fi
}

show_menu() {
    echo -e "${YELLOW}Select build targets:${NC}"
    echo ""
    echo "1) All supported platforms (recommended)"
    echo "2) Linux only (AMD64, ARM64, ARM32v7)"
    echo "3) Cross-platform (Linux, macOS, Windows)"
    echo "4) Custom selection"
    echo "5) Quick build (Linux AMD64 only)"
    echo ""
    echo "0) Exit"
    echo ""
}

build_all() {
    print_status "Building for all supported platforms..."
    local success=0
    local total=0
    
    for platform in "${!PLATFORMS[@]}"; do
        total=$((total + 1))
        if build_binary "$platform" "${PLATFORMS[$platform]}"; then
            success=$((success + 1))
        fi
    done
    
    echo ""
    print_info "Build summary: $success/$total platforms successful"
}

build_linux_only() {
    print_status "Building for Linux platforms..."
    local platforms=("linux/amd64" "linux/arm64" "linux/arm")
    local success=0
    
    for platform in "${platforms[@]}"; do
        if build_binary "$platform" "${PLATFORMS[$platform]}"; then
            success=$((success + 1))
        fi
    done
    
    echo ""
    print_info "Linux build summary: $success/${#platforms[@]} platforms successful"
}

build_cross_platform() {
    print_status "Building for cross-platform deployment..."
    local platforms=("linux/amd64" "darwin/amd64" "darwin/arm64" "windows/amd64")
    local success=0
    
    for platform in "${platforms[@]}"; do
        if build_binary "$platform" "${PLATFORMS[$platform]}"; then
            success=$((success + 1))
        fi
    done
    
    echo ""
    print_info "Cross-platform build summary: $success/${#platforms[@]} platforms successful"
}

build_quick() {
    print_status "Quick build for Linux AMD64..."
    build_binary "linux/amd64" "${PLATFORMS["linux/amd64"]}"
}

build_custom() {
    echo ""
    echo -e "${YELLOW}Available platforms:${NC}"
    local i=1
    local platform_array=()
    
    for platform in "${!PLATFORMS[@]}"; do
        echo "$i) $platform - ${PLATFORMS[$platform]}"
        platform_array+=("$platform")
        i=$((i + 1))
    done
    
    echo ""
    echo -n "Enter platform numbers (space-separated): "
    read -r selections
    
    local success=0
    local total=0
    
    for selection in $selections; do
        if [[ "$selection" =~ ^[0-9]+$ ]] && [ "$selection" -ge 1 ] && [ "$selection" -le "${#platform_array[@]}" ]; then
            local platform="${platform_array[$((selection - 1))]}"
            total=$((total + 1))
            if build_binary "$platform" "${PLATFORMS[$platform]}"; then
                success=$((success + 1))
            fi
        else
            print_warning "Invalid selection: $selection"
        fi
    done
    
    echo ""
    print_info "Custom build summary: $success/$total platforms successful"
}

show_results() {
    echo ""
    echo -e "${CYAN}Build Results:${NC}"
    echo "============="
    
    if [ -d "$BUILD_DIR" ] && [ "$(ls -A $BUILD_DIR 2>/dev/null)" ]; then
        ls -lh "$BUILD_DIR"/ | tail -n +2 | while read -r line; do
            echo "  $line"
        done
        
        echo ""
        print_info "Binaries available in: $BUILD_DIR/"
        echo ""
        echo -e "${YELLOW}Usage examples:${NC}"
        echo "  ./builds/hmac-file-server-linux-amd64 -config config.toml"
        echo "  ./builds/hmac-file-server-linux-arm64 -genconfig"
        echo "  ./builds/hmac-file-server-darwin-amd64 -version"
    else
        print_warning "No binaries were built"
    fi
}

cleanup_builds() {
    if [ -d "$BUILD_DIR" ]; then
        print_status "Cleaning previous builds..."
        rm -rf "$BUILD_DIR"
        print_info "Previous builds cleaned"
    fi
}

# Main execution
main() {
    print_header
    
    # Check if Go is installed
    if ! command -v go >/dev/null 2>&1; then
        print_error "Go is not installed or not in PATH"
        exit 1
    fi
    
    print_info "Go version: $(go version)"
    print_info "Building HMAC File Server ${VERSION}"
    echo ""
    
    # Create build directory
    mkdir -p "$BUILD_DIR"
    
    # Check if source files exist
    if [ ! -d "$SOURCE_FILES" ]; then
        print_error "Source files not found at: $SOURCE_FILES"
        exit 1
    fi
    
    while true; do
        show_menu
        echo -n "Choose an option [1-5, 0 to exit]: "
        read -r choice
        
        case $choice in
            1)
                cleanup_builds
                mkdir -p "$BUILD_DIR"
                build_all
                show_results
                break
                ;;
            2)
                cleanup_builds
                mkdir -p "$BUILD_DIR"
                build_linux_only
                show_results
                break
                ;;
            3)
                cleanup_builds
                mkdir -p "$BUILD_DIR"
                build_cross_platform
                show_results
                break
                ;;
            4)
                cleanup_builds
                mkdir -p "$BUILD_DIR"
                build_custom
                show_results
                break
                ;;
            5)
                cleanup_builds
                mkdir -p "$BUILD_DIR"
                build_quick
                show_results
                break
                ;;
            0)
                print_info "Goodbye!"
                exit 0
                ;;
            *)
                print_error "Invalid option. Please try again."
                echo ""
                ;;
        esac
    done
    
    # Reset environment
    unset GOOS GOARCH CGO_ENABLED
}

# Run if executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
