#!/bin/bash
# deploy-podman-simple.sh - Simplified Podman deployment for testing
# This is a root-compatible version for testing purposes

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Configuration
APP_NAME="hmac-file-server"
IMAGE_NAME="localhost/hmac-file-server:latest"
CONTAINER_NAME="hmac-file-server-test"
CONFIG_DIR="/opt/podman/hmac-file-server/config"
DATA_DIR="/opt/podman/hmac-file-server/data"

# Create directories
create_directories() {
    log_info "Creating Podman directories..."
    mkdir -p "$CONFIG_DIR"
    mkdir -p "$DATA_DIR"/{uploads,duplicates,temp,logs}
    
    # Create basic configuration if it doesn't exist
    if [ ! -f "$CONFIG_DIR/config.toml" ]; then
        log_info "Creating Podman configuration..."
        cat > "$CONFIG_DIR/config.toml" << 'EOF'
[server]
listen_address = "8888"
storage_path = "/data/uploads"
max_upload_size = "10GB"

[security]
secret = "CHANGE-THIS-SECRET-KEY-MINIMUM-32-CHARACTERS"

[uploads]
allowedextensions = [".txt", ".pdf", ".jpg", ".jpeg", ".png", ".gif", ".zip", ".tar", ".gz"]
maxfilesize = "100MB"
chunkeduploadsenabled = true
networkevents = true

[network_resilience]
enabled = true
quality_monitoring = true
upload_resilience = true

[logging]
level = "INFO"
file = "/logs/hmac-file-server.log"
EOF
        log_success "Configuration created"
    fi
}

# Build image
build_image() {
    log_info "Building Podman image..."
    if podman build -t "$IMAGE_NAME" -f ./Dockerfile.podman ../../.. >/dev/null 2>&1; then
        log_success "Image built successfully"
    else
        log_error "Failed to build image"
        return 1
    fi
}

# Run container
run_container() {
    log_info "Running Podman container..."
    
    # Stop existing container if running
    if podman ps -q --filter name="$CONTAINER_NAME" | grep -q .; then
        log_info "Stopping existing container..."
        podman stop "$CONTAINER_NAME" >/dev/null 2>&1 || true
    fi
    
    # Remove existing container
    if podman ps -aq --filter name="$CONTAINER_NAME" | grep -q .; then
        log_info "Removing existing container..."
        podman rm "$CONTAINER_NAME" >/dev/null 2>&1 || true
    fi
    
    # Run new container
    podman run -d \
        --name "$CONTAINER_NAME" \
        --restart unless-stopped \
        -p 8888:8888 \
        -v "$CONFIG_DIR:/app/config:Z" \
        -v "$DATA_DIR:/data:Z" \
        "$IMAGE_NAME" \
        -config /app/config/config.toml || {
        log_error "Failed to run container"
        return 1
    }
    
    log_success "Container started successfully"
}

# Main execution
main() {
    log_info "Starting simplified Podman deployment..."
    
    if [ "$EUID" -eq 0 ]; then
        log_warning "Running as root - using rootful Podman"
    fi
    
    create_directories
    build_image
    run_container
    
    log_success "Podman deployment completed!"
    log_info "Container status:"
    podman ps --filter name="$CONTAINER_NAME"
}

# Handle arguments
case "${1:-}" in
    "test")
        # Test mode - just validate setup
        create_directories
        if podman images | grep -q hmac-file-server; then
            log_success "Podman test validation passed"
        else
            log_warning "Podman image not found"
        fi
        ;;
    *)
        main
        ;;
esac
