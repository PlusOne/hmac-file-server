#!/bin/bash
# deploy-podman.sh - Production Podman deployment script for HMAC File Server 3.2
# Usage: ./deploy-podman.sh [start|stop|restart|status|logs|config]

set -euo pipefail

# Color codes for pretty output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
info() { echo -e "${BLUE}[INFO]${NC} $1"; }
success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Configuration variables
readonly APP_NAME='hmac-file-server'
readonly POD_NAME='xmpp-pod'
readonly CTR_NAME="${POD_NAME}-${APP_NAME}"
readonly CTR_IMAGE='localhost/hmac-file-server:latest'
readonly RESTART_POLICY='unless-stopped'
readonly CTR_UID='1011'
readonly APP_DATA="${APP_DATA:-/opt/podman/hmac-file-server}"
readonly LISTEN_PORT="${LISTEN_PORT:-8888}"
readonly METRICS_PORT="${METRICS_PORT:-9090}"
readonly CONFIG_FILE="${APP_DATA}/config/config.toml"

# Check if running as root (not recommended for Podman)
check_user() {
    if [[ $EUID -eq 0 ]]; then
        warning "Running as root. Consider using Podman rootless for better security."
        read -p "Continue anyway? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi
}

# Create application directories
setup_directories() {
    info "Setting up application directories..."
    
    mkdir -p "${APP_DATA}"/{config,data,deduplication,logs}
    
    # Set proper ownership
    if command -v podman >/dev/null 2>&1; then
        podman unshare chown -R "${CTR_UID}:${CTR_UID}" "${APP_DATA}"
    else
        error "Podman not found. Please install Podman first."
        exit 1
    fi
    
    success "Directories created at ${APP_DATA}"
}

# Generate configuration file
generate_config() {
    if [[ -f "${CONFIG_FILE}" ]]; then
        warning "Configuration file already exists at ${CONFIG_FILE}"
        read -p "Overwrite? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            return 0
        fi
    fi
    
    info "Generating configuration file..."
    
    # Generate random secrets
    local hmac_secret=$(openssl rand -base64 32 2>/dev/null || head -c 32 /dev/urandom | base64)
    local jwt_secret=$(openssl rand -base64 32 2>/dev/null || head -c 32 /dev/urandom | base64)
    
    cat > "${CONFIG_FILE}" << EOF
# HMAC File Server 3.2 - Podman Production Configuration
# Generated on $(date)

[server]
listen_address = "${LISTEN_PORT}"
storage_path = "/data"
metrics_enabled = true
metrics_port = "${METRICS_PORT}"
max_upload_size = "10GB"
max_header_bytes = 1048576
cleanup_interval = "24h"
max_file_age = "720h"
enable_dynamic_workers = true
worker_scale_up_thresh = 40
worker_scale_down_thresh = 10
deduplication_enabled = true
min_free_bytes = "1GB"
file_naming = "original"

[uploads]
# XMPP-compatible file extensions for maximum client support
allowed_extensions = [".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp", ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg", ".doc", ".docx"]
chunked_uploads_enabled = true
chunk_size = "32MB"
resumable_uploads_enabled = true
max_resumable_age = "48h"
sessiontimeout = "60m"
maxretries = 3

# Upload resilience settings
session_persistence = true
session_recovery_timeout = "300s"
client_reconnect_window = "120s"
upload_slot_ttl = "3600s"
retry_failed_uploads = true
max_upload_retries = 3

[downloads]
resumable_downloads_enabled = true
chunked_downloads_enabled = true
chunk_size = "32MB"
# Same extensions as uploads for consistency
allowed_extensions = [".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp", ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg", ".doc", ".docx"]

[security]
secret = "${hmac_secret}"
enablejwt = true
jwtsecret = "${jwt_secret}"
jwtalgorithm = "HS256"
jwtexpiration = "24h"

[logging]
level = "info"
file = "/logs/hmac-file-server.log"
max_size = 100
max_backups = 7
max_age = 30
compress = true

[deduplication]
enabled = true
directory = "/deduplication"

[workers]
numworkers = 4
uploadqueuesize = 100

[timeouts]
readtimeout = "3600s"
writetimeout = "3600s"
idletimeout = "3600s"
shutdown = "30s"
EOF
    
    success "Configuration generated at ${CONFIG_FILE}"
    warning "Secrets have been auto-generated. Keep this file secure!"
}

# Build container image
build_image() {
    info "Checking if image ${CTR_IMAGE} exists..."
    
    if podman image exists "${CTR_IMAGE}"; then
        warning "Image ${CTR_IMAGE} already exists"
        read -p "Rebuild? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            return 0
        fi
    fi
    
    info "Building container image ${CTR_IMAGE}..."
    
    # Find the Dockerfile
    local dockerfile_path
    if [[ -f "dockerenv/podman/Dockerfile.podman" ]]; then
        dockerfile_path="dockerenv/podman/Dockerfile.podman"
    elif [[ -f "Dockerfile.podman" ]]; then
        dockerfile_path="Dockerfile.podman"
    else
        error "Dockerfile.podman not found. Please run from project root or ensure file exists."
        exit 1
    fi
    
    podman build --no-cache -t "${CTR_IMAGE}" -f "${dockerfile_path}" .
    
    success "Image ${CTR_IMAGE} built successfully"
}

# Create pod for networking
create_pod() {
    info "Creating pod ${POD_NAME}..."
    
    # Remove existing pod if it exists
    if podman pod exists "${POD_NAME}"; then
        warning "Pod ${POD_NAME} already exists, removing..."
        podman pod stop "${POD_NAME}" 2>/dev/null || true
        podman pod rm "${POD_NAME}" 2>/dev/null || true
    fi
    
    podman pod create --name "${POD_NAME}" \
        --publish "${LISTEN_PORT}:8888" \
        --publish "${METRICS_PORT}:9090"
    
    success "Pod ${POD_NAME} created"
}

# Start the container
start_container() {
    info "Starting HMAC File Server container..."
    
    # Stop and remove existing container
    podman container stop "${CTR_NAME}" 2>/dev/null || true
    podman container rm "${CTR_NAME}" 2>/dev/null || true
    
    # Run container with security-hardened settings
    podman run -d \
        --pod="${POD_NAME}" \
        --restart="${RESTART_POLICY}" \
        --name "${CTR_NAME}" \
        --user "${CTR_UID}:${CTR_UID}" \
        --cap-drop=ALL \
        --security-opt no-new-privileges \
        --read-only \
        --tmpfs /tmp:rw,noexec,nosuid,size=100m \
        -v "${CONFIG_FILE}:/app/config.toml:ro,Z" \
        -v "${APP_DATA}/data:/data:rw,Z" \
        -v "${APP_DATA}/deduplication:/deduplication:rw,Z" \
        -v "${APP_DATA}/logs:/logs:rw,Z" \
        --health-cmd="curl -f http://localhost:8888/health || exit 1" \
        --health-interval=30s \
        --health-timeout=10s \
        --health-retries=3 \
        --health-start-period=40s \
        "${CTR_IMAGE}" -config /app/config.toml
    
    success "Container ${CTR_NAME} started successfully!"
}

# Stop the container
stop_container() {
    info "Stopping HMAC File Server..."
    
    podman container stop "${CTR_NAME}" 2>/dev/null || true
    podman container rm "${CTR_NAME}" 2>/dev/null || true
    podman pod stop "${POD_NAME}" 2>/dev/null || true
    podman pod rm "${POD_NAME}" 2>/dev/null || true
    
    success "HMAC File Server stopped"
}

# Show status
show_status() {
    echo
    info "=== HMAC File Server Status ==="
    
    if podman pod exists "${POD_NAME}"; then
        echo "Pod Status:"
        podman pod ps --filter "name=${POD_NAME}"
        echo
    fi
    
    if podman container exists "${CTR_NAME}"; then
        echo "Container Status:"
        podman ps --filter "name=${CTR_NAME}"
        echo
        
        echo "Health Status:"
        podman healthcheck run "${CTR_NAME}" 2>/dev/null && echo "✅ Healthy" || echo "❌ Unhealthy"
        echo
    else
        warning "Container ${CTR_NAME} not found"
    fi
    
    echo "Service URLs:"
    echo "  🌐 File Server: http://localhost:${LISTEN_PORT}"
    echo "  📊 Metrics: http://localhost:${METRICS_PORT}/metrics"
    echo "  🔍 Health Check: http://localhost:${LISTEN_PORT}/health"
    echo
}

# Show logs
show_logs() {
    if podman container exists "${CTR_NAME}"; then
        info "Showing logs for ${CTR_NAME} (Ctrl+C to exit)..."
        podman logs -f "${CTR_NAME}"
    else
        error "Container ${CTR_NAME} not found"
        exit 1
    fi
}

# Full deployment
deploy() {
    info "Starting full HMAC File Server deployment..."
    
    check_user
    setup_directories
    generate_config
    build_image
    create_pod
    start_container
    
    sleep 5  # Wait for container to start
    show_status
    
    success "🎉 HMAC File Server deployed successfully!"
    echo
    info "Next steps:"
    echo "1. Test the service: curl -f http://localhost:${LISTEN_PORT}/health"
    echo "2. View logs: ./deploy-podman.sh logs"
    echo "3. Check status: ./deploy-podman.sh status"
    echo "4. Edit config: ${CONFIG_FILE}"
    echo
}

# Main command dispatcher
case "${1:-deploy}" in
    start|deploy)
        deploy
        ;;
    stop)
        stop_container
        ;;
    restart)
        stop_container
        sleep 2
        create_pod
        start_container
        show_status
        ;;
    status)
        show_status
        ;;
    logs)
        show_logs
        ;;
    config)
        info "Configuration file location: ${CONFIG_FILE}"
        if [[ -f "${CONFIG_FILE}" ]]; then
            echo "Current configuration:"
            cat "${CONFIG_FILE}"
        else
            warning "Configuration file not found. Run './deploy-podman.sh' to generate it."
        fi
        ;;
    build)
        build_image
        ;;
    pod)
        create_pod
        ;;
    clean)
        warning "This will remove all containers, pods, and the image. Data will be preserved."
        read -p "Continue? (y/N): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            stop_container
            podman image rm "${CTR_IMAGE}" 2>/dev/null || true
            success "Cleanup completed"
        fi
        ;;
    help|--help|-h)
        echo "HMAC File Server Podman Deployment Script"
        echo
        echo "Usage: $0 [COMMAND]"
        echo
        echo "Commands:"
        echo "  deploy    Full deployment (default)"
        echo "  start     Start services"
        echo "  stop      Stop all services"
        echo "  restart   Restart services"
        echo "  status    Show service status"
        echo "  logs      Show container logs"
        echo "  config    Show configuration"
        echo "  build     Build container image only"
        echo "  pod       Create pod only"
        echo "  clean     Remove containers and image"
        echo "  help      Show this help"
        echo
        echo "Environment Variables:"
        echo "  APP_DATA      Data directory (default: /opt/podman/hmac-file-server)"
        echo "  LISTEN_PORT   Server port (default: 8888)"
        echo "  METRICS_PORT  Metrics port (default: 9090)"
        echo
        ;;
    *)
        error "Unknown command: $1"
        echo "Run '$0 help' for usage information"
        exit 1
        ;;
esac
