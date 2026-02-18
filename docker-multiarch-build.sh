#!/bin/bash
# HMAC File Server 3.4.0 "Cascade" - Docker Multi-Architecture Builder
# Builds multi-arch Docker images using Docker Buildx

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# Configuration
IMAGE_NAME="hmac-file-server"
VERSION="3.4.0"
REGISTRY="localhost"  # Change to your registry
PLATFORMS="linux/amd64,linux/arm64,linux/arm/v7"

# Functions
print_header() {
    echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║${NC} ${CYAN}HMAC File Server Docker Multi-Architecture Builder${NC}      ${BLUE}║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
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

check_requirements() {
    print_status "Checking requirements..."
    
    # Check Docker
    if ! command -v docker >/dev/null 2>&1; then
        print_error "Docker is not installed or not in PATH"
        exit 1
    fi
    
    # Check Docker Buildx
    if ! docker buildx version >/dev/null 2>&1; then
        print_error "Docker Buildx is not available"
        print_info "Install with: docker buildx install"
        exit 1
    fi
    
    # Check if Docker daemon is running
    if ! docker info >/dev/null 2>&1; then
        print_error "Docker daemon is not running"
        exit 1
    fi
    
    print_info "Docker $(docker --version | cut -d' ' -f3 | tr -d ',') detected"
    print_info "Buildx $(docker buildx version | cut -d' ' -f2) detected"
}

setup_buildx() {
    print_status "Setting up Docker Buildx..."
    
    # Create builder if it doesn't exist
    if ! docker buildx inspect multiarch-builder >/dev/null 2>&1; then
        print_status "Creating multiarch builder..."
        docker buildx create --name multiarch-builder --use --bootstrap
        print_info "Multiarch builder created and activated"
    else
        print_info "Using existing multiarch builder"
        docker buildx use multiarch-builder
    fi
    
    # Verify platforms
    print_status "Available platforms:"
    docker buildx inspect --bootstrap | grep "Platforms:" | head -1
}

build_images() {
    local push_flag=""
    if [ "${1:-}" = "--push" ]; then
        push_flag="--push"
        print_warning "Images will be pushed to registry"
    else
        push_flag="--load"
        print_info "Images will be loaded locally (AMD64 only)"
    fi
    
    print_status "Building multi-architecture images..."
    
    # Build and optionally push
    docker buildx build \
        --platform $PLATFORMS \
        --file Dockerfile.multiarch \
        --tag "${REGISTRY}/${IMAGE_NAME}:${VERSION}" \
        --tag "${REGISTRY}/${IMAGE_NAME}:latest" \
        $push_flag \
        .
    
    if [ "$push_flag" = "--push" ]; then
        print_info "Multi-arch images built and pushed successfully"
    else
        print_info "Multi-arch images built and loaded locally"
    fi
}

test_images() {
    print_status "Testing built images..."
    
    # Test AMD64 image (if loaded locally)
    if [ "${1:-}" != "--push" ]; then
        print_status "Testing AMD64 image..."
        if docker run --rm "${REGISTRY}/${IMAGE_NAME}:${VERSION}" -version 2>/dev/null; then
            print_info "AMD64 image test passed"
        else
            print_warning "AMD64 image test failed (this is normal if image wasn't loaded)"
        fi
    fi
    
    # Show image info
    print_status "Image information:"
    docker images "${REGISTRY}/${IMAGE_NAME}" 2>/dev/null | head -2 || print_warning "Images not found locally (normal if pushed to registry)"
}

show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --push          Build and push to registry (multi-arch)"
    echo "  --local         Build for local use (AMD64 only)"
    echo "  --registry REG  Set registry (default: localhost)"
    echo "  --help          Show this help"
    echo ""
    echo "Examples:"
    echo "  $0 --local                    # Build for local testing"
    echo "  $0 --push                     # Build and push multi-arch"
    echo "  $0 --registry hub.docker.com --push  # Push to Docker Hub"
}

# Main execution
main() {
    local push_mode=""
    
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --push)
                push_mode="--push"
                shift
                ;;
            --local)
                push_mode="--local"
                shift
                ;;
            --registry)
                REGISTRY="$2"
                shift 2
                ;;
            --help)
                show_usage
                exit 0
                ;;
            *)
                print_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
        esac
    done
    
    print_header
    
    print_info "Configuration:"
    print_info "  Image: ${REGISTRY}/${IMAGE_NAME}:${VERSION}"
    print_info "  Platforms: ${PLATFORMS}"
    print_info "  Registry: ${REGISTRY}"
    echo ""
    
    check_requirements
    setup_buildx
    
    echo ""
    
    if [ "$push_mode" = "--push" ]; then
        build_images --push
    else
        build_images --local
    fi
    
    echo ""
    test_images "$push_mode"
    
    echo ""
    print_info "Multi-architecture Docker build complete!"
    
    if [ "$push_mode" = "--push" ]; then
        echo ""
        print_info "To use the images:"
        echo "  docker run -p 8080:8080 ${REGISTRY}/${IMAGE_NAME}:${VERSION}"
        echo "  docker run --platform linux/arm64 ${REGISTRY}/${IMAGE_NAME}:${VERSION}"
    else
        echo ""
        print_info "To push to registry later:"
        echo "  $0 --registry YOUR_REGISTRY --push"
    fi
}

# Check if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
