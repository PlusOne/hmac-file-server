# HMAC File Server 3.2 - Tremora del Terra

[![Version](https://img.shields.io/badge/version-3.2.1-blue.svg)](https://github.com/PlusOne/hmac-file-server)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
[![Go Version](https://img.shields.io/badge/go-1.21+-00ADD8.svg)](https://golang.org/)
[![Architecture](https://img.shields.io/badge/arch-AMD64%20%7C%20ARM64%20%7C%20ARM32v7-brightgreen.svg)](https://github.com/PlusOne/hmac-file-server)

A high-performance, secure file server implementing XEP-0363 (HTTP File Upload) with HMAC authentication, deduplication, and multi-architecture support.

---

## What's New in 3.2 "Tremora del Terra"

### Configuration Revolution
- **93% Config Reduction**: From 112-line complex configs to 8-line minimal configs
- **Smart Defaults**: Production-ready settings built into the application
- **Auto-Generation**: `--genconfig` creates minimal configs instantly
- **Zero Breaking Changes**: Existing configs continue working

### Enhanced Performance
- **Fixed Deduplication**: Existing files return success instead of "file not found"
- **Queue Optimization**: Doubled capacity (50 to 100), faster scaling (80% to 40% threshold)
- **Extended Timeouts**: 4800s defaults for large file reliability
- **Session Persistence**: 60-minute timeouts for enterprise transfers

### Multi-Architecture Support
- **Cross-Platform**: AMD64, ARM64, ARM32v7 with native performance
- **Interactive Builder**: Easy architecture targeting with menu system
- **Production Ready**: All platforms enterprise-grade

### Container Support
- **Docker & Podman**: Full support for both container engines
- **Enterprise Ready**: Podman deployment tested and verified ✅
- **Security Hardened**: Rootless, daemonless operation with SELinux integration
- **XMPP Optimized**: Pod networking for multi-service deployments

---

## Quick Start

### Option 1: Minimal Configuration (Recommended)
```bash
# Download HMAC File Server 3.2
wget https://github.com/PlusOne/hmac-file-server/releases/download/v3.2/hmac-file-server-linux-amd64
chmod +x hmac-file-server-linux-amd64

# Generate minimal config
./hmac-file-server-linux-amd64 -genconfig > config.toml

# Edit 3 essential settings:
# - listen_address = "8080"
# - storage_path = "/your/storage/path"
# - secret = "your-secure-secret"

# Start server
./hmac-file-server-linux-amd64 -config config.toml
```

### Option 2: Zero-Config Startup
```bash
# Auto-creates minimal config
./hmac-file-server-linux-amd64
# Follow prompts to customize settings
```

---

## Universal Installation Manager

HMAC File Server 3.2 includes a comprehensive installation framework that supports all deployment methods:

### 🚀 **Automated Installation (All Methods)**
```bash
# Interactive menu - choose your deployment method
./install-manager.sh

# Direct method selection
./install-manager.sh systemd    # Native SystemD installation
./install-manager.sh docker     # Docker deployment  
./install-manager.sh podman     # Podman deployment
./install-manager.sh debian     # Debian package

# Test all installation methods
./install-manager.sh --test
```

### ✅ **Supported Deployment Methods**
- **✅ SystemD**: Native installation with service integration
- **✅ Docker**: Full containerized deployment with compose files  
- **✅ Podman**: Rootless container deployment (tested & verified)
- **✅ Debian**: Package-based installation with dependency management
- **✅ Multi-Architecture**: AMD64, ARM64, ARM32v7 support for all methods

### 🧪 **Comprehensive Testing Suite**
```bash
# Run all functionality tests
./test

# Quick validation test
./quick-test

# Test specific components
./test setup    # Setup test files only
./test clean    # Clean up test files
```

### 🐳 **Enhanced Container Build Script**
```bash
# Universal container builder - auto-detects Docker & Podman
./builddocker.sh

# Use specific container engine
./builddocker.sh docker          # Force Docker usage
./builddocker.sh podman          # Force Podman usage

# Build only (no services start)
./builddocker.sh docker --build-only
./builddocker.sh podman --build-only

# Show usage help
./builddocker.sh --help
```

**Features:**
- ✅ **Auto-Detection**: Automatically finds available container engines (Docker/Podman)
- ✅ **Engine Selection**: Interactive menu for multiple engines or force specific engine
- ✅ **Compose Support**: Uses appropriate compose files (docker-compose.yml / podman-compose.yml)
- ✅ **Podman Optimized**: SELinux labels, rootless support, security optimizations
- ✅ **Build & Deploy**: Combined build and optional service startup in one command

**Test Coverage:**
- ✅ HMAC Authentication & File Upload Validation
- ✅ XMPP Integration (MP4 uploads for Conversations/Gajim)
- ✅ Network Resilience & Mobile Switching Features  
- ✅ Large File Support & Extension Validation
- ✅ Security Testing (Invalid HMAC rejection)

---

## Table of Contents

- [Release Information](#release-information)
- [Configuration Generation](#configuration-generation)
- [Configuration Documentation](#configuration-documentation)
- [Build Options](#build-options)
- [Docker Compose Examples](#docker-compose-examples)
- [Podman Deployment](#podman-deployment) ⭐ **NEW: Tested & Verified**
- [Nginx Reverse Proxy](#nginx-reverse-proxy)
- [Apache2 Reverse Proxy](#apache2-reverse-proxy)
- [Prosody XMPP Integration](#prosody-xmpp-integration)
- [Ejabberd XMPP Integration](#ejabberd-xmpp-integration)
- [XEP-0363 Implementation](#xep-0363-implementation)
- [API Versions (V1, V2, V3)](#api-versions)

---

## Release Information

### HMAC File Server 3.2.1 - Tremora del Terra

**Release Date**: July 20, 2025  
**Codename**: Tremora del Terra (powerful, balanced, and ready to shake the ground)

#### Key Improvements
- **Configuration Simplification**: 93% reduction in required configuration
- **Enhanced Deduplication**: Fixed "file not found" errors for existing files
- **Performance Optimization**: Doubled queue capacity, optimized worker scaling
- **Multi-Architecture Support**: Native builds for AMD64, ARM64, ARM32v7
- **Developer Experience**: Minimal config-first approach with comprehensive defaults

#### Critical Fixes (3.2.1)
- **🔧 XMPP Integration**: Fixed MP4 upload failures for Conversations/Gajim clients
- **🔧 Configuration Loading**: Resolved TOML key mismatch causing extension validation errors
- **🔧 Network Resilience**: Restored seamless WLAN ↔ IPv6 5G mobile switching
- **🔧 Testing Framework**: Comprehensive test suite with 100% pass rate validation

#### Migration Notes
- **Backward Compatible**: All existing 3.1.x configs work unchanged
- **Performance Boost**: Automatic optimizations with existing configurations
- **Optional Migration**: Users can optionally migrate to simplified 8-line configs

#### System Requirements
- **Memory**: 512MB minimum, 2GB+ recommended for large files
- **Storage**: 100MB application + user data storage
- **Network**: Standard TCP/IP connectivity
- **OS**: Linux (primary), Windows/macOS (experimental)

---

## Mobile Network Resilience

HMAC File Server 3.2 introduces enhanced network resilience specifically designed for mobile devices and network switching scenarios.

### 📱 **Mobile Network Switching Support**

#### **Scenario 1: WLAN ↔ IPv6 5G Switching**
Perfect for mobile devices that switch between WiFi and cellular networks:

```toml
[server]
networkevents = true                        # REQUIRED: Enable network monitoring

[network_resilience] 
fast_detection = true                       # 1-second detection vs 5-second default
quality_monitoring = true                   # Monitor connection quality
predictive_switching = true                 # Switch before network fails
mobile_optimizations = true                # Cellular-friendly settings

[uploads]
session_recovery_timeout = "600s"           # 10-minute recovery window for IP changes
client_reconnect_window = "300s"            # 5-minute reconnection window
max_resumable_age = "72h"                   # Extended session retention
max_upload_retries = 8                     # More retries for cellular

[timeouts]
readtimeout = "600s"                        # Extended for cellular latency
writetimeout = "600s"                       # Handle cellular upload delays
idletimeout = "1200s"                       # 20-minute tolerance
```

#### **Scenario 2: Dual-Connected Devices (Wired + WiFi)**
For devices with multiple network interfaces:

```toml
[network_resilience]
fast_detection = true                       # Quick interface change detection
quality_monitoring = true                   # Monitor both connections
predictive_switching = true                 # Use best available interface

# System automatically selects best interface based on:
# - RTT (latency)
# - Packet loss percentage  
# - Connection stability
# - Interface priority (ethernet > wifi > cellular)
```

### **Benefits for Mobile Scenarios**

| Feature | Standard Detection | Enhanced Mobile Detection |
|---------|-------------------|---------------------------|
| **Detection Speed** | 5 seconds | **1 second** |
| **Network Quality** | Interface status only | **RTT + packet loss monitoring** |
| **Switching Logic** | Reactive (after failure) | **Proactive (before failure)** |
| **Mobile Tolerance** | Fixed thresholds | **Cellular-optimized thresholds** |
| **Session Recovery** | 2-minute window | **10-minute window** |
| **Upload Resumption** | Basic retry | **Smart retry with backoff** |

### **Configuration Examples**

**Ultra-Fast Mobile Detection**:
```toml
[network_resilience]
detection_interval = "500ms"               # Sub-second detection
quality_check_interval = "2s"             # Frequent quality checks
mobile_optimizations = true               # Lenient cellular thresholds
```

**Conservative Stable Network**:
```toml
[network_resilience]
detection_interval = "10s"                # Slower detection
quality_monitoring = false                # Disable quality checks
predictive_switching = false              # React only to hard failures
```

---

## Configuration Generation

### Generate Minimal Configuration
```bash
# Create minimal 8-line config (recommended for most users)
./hmac-file-server -genconfig > config.toml
```

**Output Example:**
```toml
# HMAC File Server - Minimal Configuration
[server]
listen_address = "8080"
storage_path = "./uploads"

[security]
secret = "your-very-secret-hmac-key"

[logging]
level = "info"
file = "/var/log/hmac-file-server.log"
```

### Generate Advanced Configuration
```bash
# Create comprehensive config template with all options
./hmac-file-server -genconfig-advanced > config-advanced.toml
```

### Write Configuration to File
```bash
# Write minimal config to specific file
./hmac-file-server -genconfig-path /etc/hmac-file-server/config.toml

# Write advanced config to specific file
./hmac-file-server -genconfig-advanced -genconfig-path /etc/hmac-file-server/config-full.toml
```

### Configuration Validation
```bash
# Validate configuration without starting server
./hmac-file-server -validate-config -config config.toml

# Quiet validation (errors only)
./hmac-file-server -validate-quiet -config config.toml

# Verbose validation with system checks
./hmac-file-server -validate-verbose -config config.toml

# Security-focused validation
./hmac-file-server -check-security -config config.toml

# Performance-focused validation
./hmac-file-server -check-performance -config config.toml
```

### ⚠️ Configuration Troubleshooting

**Common Issue**: Service fails with `storage path is required` or `permission denied`

```bash
# ❌ WRONG - Field names without underscores
[server]
storagepath = "/opt/hmac-file-server/data/uploads"
listenport = "8080"

# ✅ CORRECT - Use underscores in TOML field names  
[server]
storage_path = "/opt/hmac-file-server/data/uploads"
listen_address = "8080"
```

**Quick Fix Commands:**
```bash
# Test configuration
./hmac-file-server --validate-config

# Auto-fix common field names (creates backup)
./fix-config.sh config.toml

# Manual fix for common field names
sed -i 's/storagepath/storage_path/g' config.toml
sed -i 's/listenport/listen_address/g' config.toml

# Check permissions
ls -la $(dirname "$(grep storage_path config.toml | cut -d'"' -f2)")
```

---

## Configuration Documentation

### Complete config.toml Line-by-Line Documentation

```toml
[server]
# Network binding configuration
listen_address = "8080"                    # Port to listen on (string: "8080", "0.0.0.0:8080")
bind_ip = ""                               # IP to bind to (empty = all interfaces)

# Storage and file handling
storage_path = "./uploads"                 # Directory for uploaded files (use absolute paths in production)
max_upload_size = "10GB"                   # Maximum file size (supports B, KB, MB, GB, TB)
max_header_bytes = 1048576                 # HTTP header size limit (1MB default)
file_naming = "original"                   # File naming: "original" or "HMAC"
min_free_bytes = "1GB"                     # Minimum free space required

# Server behavior
cleanup_interval = "24h"                   # How often to run cleanup (Go duration format)
max_file_age = "720h"                      # Auto-delete files older than this (30 days)
force_protocol = ""                        # Force protocol: "", "http", "https"

# Performance and scaling
enable_dynamic_workers = true              # Enable automatic worker scaling
worker_scale_up_thresh = 40                # Queue % to scale up workers (40% default)
worker_scale_down_thresh = 10              # Queue % to scale down workers (10% default)

# Metrics and monitoring
metrics_enabled = true                     # Enable Prometheus metrics
metrics_path = "/metrics"                  # Metrics endpoint path
metrics_port = "9090"                      # Metrics server port

# System integration
pid_file = "/var/run/hmac-file-server.pid" # Process ID file location
unix_socket = false                        # Use Unix socket instead of TCP

# Caching and performance
pre_cache = true                           # Enable file pre-caching
pre_cache_workers = 4                      # Number of pre-cache workers
pre_cache_interval = "1h"                  # Pre-cache refresh interval

# File type handling
global_extensions = [".txt", ".pdf"]       # Global allowed extensions (overrides upload/download)
deduplication_enabled = true               # Enable file deduplication

# Network resilience (cluster-aware settings)
graceful_shutdown_timeout = "300s"         # Time for graceful shutdown
connection_drain_timeout = "120s"          # Time to drain connections
max_idle_conns_per_host = 5                # Max idle connections per client
idle_conn_timeout = "90s"                  # Idle connection timeout
disable_keep_alives = false                # Disable HTTP keep-alives
client_timeout = "300s"                    # Client request timeout
restart_grace_period = "60s"               # Grace period after restart

# Enhanced Network Resilience (v3.2+)
[network_resilience]
fast_detection = true                       # Enable 1-second network change detection (vs 5-second default)
quality_monitoring = true                   # Monitor RTT and packet loss per interface
predictive_switching = true                 # Switch proactively before network failure
mobile_optimizations = true                # Use mobile-friendly thresholds for cellular networks
detection_interval = "1s"                  # Network change detection interval
quality_check_interval = "5s"              # Connection quality monitoring interval
max_detection_interval = "10s"             # Maximum detection interval during stable periods

[uploads]
# File upload configuration
allowed_extensions = [".zip", ".rar", ".jpg", ".jpeg", ".png", ".gif", ".webp", ".pdf", ".txt", ".mp4", ".mov", ".ogg", ".mp3", ".doc", ".docx"]      # Permitted upload file extensions (XMPP-compatible)
chunked_uploads_enabled = true             # Enable chunked/resumable uploads
chunk_size = "10MB"                        # Upload chunk size
resumable_uploads_enabled = true           # Enable upload resumption
max_resumable_age = "48h"                  # How long to keep resumable uploads
sessiontimeout = "60m"                     # Upload session timeout
maxretries = 3                             # Maximum upload retry attempts

# Upload resilience
session_persistence = true                 # Persist sessions across restarts
session_recovery_timeout = "300s"          # Session recovery timeout
client_reconnect_window = "120s"           # Client reconnection window
upload_slot_ttl = "3600s"                  # Upload slot validity time
retry_failed_uploads = true                # Auto-retry failed uploads
max_upload_retries = 3                     # Maximum retry attempts

[downloads]
# File download configuration
allowed_extensions = [".txt", ".pdf", ".jpg", ".jpeg", ".png", ".gif", ".webp", ".mp4", ".mov", ".ogg", ".mp3", ".doc", ".docx"]      # Permitted download file extensions (XMPP-compatible)
chunked_downloads_enabled = true           # Enable chunked downloads
chunk_size = "10MB"                        # Download chunk size
resumable_downloads_enabled = true         # Enable download resumption

[security]
# Authentication and security
secret = "your-very-secret-hmac-key"       # HMAC secret key (REQUIRED - change in production!)
enablejwt = false                          # Enable JWT authentication
jwtsecret = "your-256-bit-secret"          # JWT signing secret
jwtalgorithm = "HS256"                     # JWT algorithm
jwtexpiration = "24h"                      # JWT token expiration

[logging]
# Logging configuration
level = "info"                             # Log level: debug, info, warn, error
file = "/var/log/hmac-file-server.log"     # Log file path
max_size = 100                             # Max log file size (MB)
max_backups = 7                            # Number of log files to keep
max_age = 30                               # Days to keep log files
compress = true                            # Compress rotated logs

[deduplication]
# File deduplication settings
enabled = true                             # Enable deduplication
directory = "./dedup_store"                # Deduplication storage directory
maxsize = "1GB"                           # Maximum deduplication storage

[iso]
# ISO file handling
enabled = false                            # Enable ISO functionality
size = "1GB"                              # ISO size limit
mountpoint = "/mnt/iso"                    # ISO mount point
charset = "utf-8"                         # ISO character set
containerfile = "/mnt/iso/container.iso"   # ISO container file path

[timeouts]
# Network timeout configuration
readtimeout = "4800s"                      # HTTP read timeout (80 minutes)
writetimeout = "4800s"                     # HTTP write timeout (80 minutes)
idletimeout = "4800s"                      # HTTP idle timeout (80 minutes)
shutdown = "30s"                           # Server shutdown timeout

[versioning]
# File versioning
enableversioning = false                   # Enable file versioning
backend = "simple"                         # Versioning backend
maxversions = 1                           # Maximum versions to keep

[clamav]
# Antivirus scanning with ClamAV
clamavenabled = false                      # Enable ClamAV scanning
clamavsocket = "/var/run/clamav/clamd.ctl" # ClamAV socket path
numscanworkers = 2                         # Number of scan workers
scanfileextensions = [".exe", ".zip"]      # File types to scan
maxscansize = "200MB"                      # Maximum file size to scan

[redis]
# Redis integration
redisenabled = false                       # Enable Redis
redisdbindex = 0                          # Redis database index
redisaddr = "localhost:6379"              # Redis server address
redispassword = ""                        # Redis password
redishealthcheckinterval = "120s"         # Redis health check interval

[workers]
# Worker pool configuration
numworkers = 4                            # Number of worker threads
uploadqueuesize = 100                     # Upload queue size (doubled in 3.2)

[build]
# Build information
version = "3.2"                          # Application version
```

---

## Build Options

### Interactive Build Script
```bash
# Use interactive build menu
./build-multi-arch.sh
```

**Menu Options:**
```
HMAC File Server Multi-Architecture Builder
==========================================
1) Build for current platform (auto-detect)
2) Build for Linux AMD64
3) Build for Linux ARM64  
4) Build for Linux ARM32v7
5) Build for Windows AMD64
6) Build for macOS AMD64 (Intel)
7) Build for macOS ARM64 (Apple Silicon)
8) Build all supported architectures
9) Clean build artifacts
0) Exit
```

### Manual Go Build Commands
```bash
# Current platform
go build -o hmac-file-server ./cmd/server/

# Linux AMD64
GOOS=linux GOARCH=amd64 go build -o hmac-file-server-linux-amd64 ./cmd/server/

# Linux ARM64
GOOS=linux GOARCH=arm64 go build -o hmac-file-server-linux-arm64 ./cmd/server/

# Linux ARM32v7
GOOS=linux GOARCH=arm GOARM=7 go build -o hmac-file-server-linux-arm32v7 ./cmd/server/

# Windows AMD64
GOOS=windows GOARCH=amd64 go build -o hmac-file-server-windows-amd64.exe ./cmd/server/

# macOS Intel
GOOS=darwin GOARCH=amd64 go build -o hmac-file-server-darwin-amd64 ./cmd/server/

# macOS Apple Silicon
GOOS=darwin GOARCH=arm64 go build -o hmac-file-server-darwin-arm64 ./cmd/server/
```

### Build with Optimizations
```bash
# Production build with optimizations
go build -ldflags="-w -s" -o hmac-file-server ./cmd/server/

# Build with version info
VERSION=$(git describe --tags --always)
go build -ldflags="-X main.version=${VERSION} -w -s" -o hmac-file-server ./cmd/server/

# Static binary (for containers)
CGO_ENABLED=0 GOOS=linux go build -a -ldflags="-w -s" -o hmac-file-server ./cmd/server/
```

### Docker Build
```bash
# Build Docker image
docker build -t hmac-file-server:3.2 .

# Multi-platform Docker build
docker buildx build --platform linux/amd64,linux/arm64,linux/arm/v7 -t hmac-file-server:3.2 .
```

---

## Docker Compose Examples

### Basic Docker Compose
```yaml
# docker-compose.yml
version: '3.8'

services:
  hmac-file-server:
    image: hmac-file-server:3.2
    container_name: hmac-file-server
    restart: unless-stopped
    ports:
      - "8080:8080"
      - "9090:9090"  # Metrics port
    volumes:
      - ./data/uploads:/app/uploads
      - ./data/logs:/app/logs
      - ./data/dedup:/app/dedup_store
      - ./config.toml:/app/config.toml:ro
    environment:
      - HMAC_CONFIG_PATH=/app/config.toml
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s
```

### Production Docker Compose with Redis and Monitoring
```yaml
# docker-compose.prod.yml
version: '3.8'

services:
  hmac-file-server:
    image: hmac-file-server:3.2
    container_name: hmac-file-server
    restart: unless-stopped
    depends_on:
      - redis
      - prometheus
    ports:
      - "8080:8080"
    volumes:
      - uploads:/app/uploads
      - dedup:/app/dedup_store
      - logs:/app/logs
      - ./config/config.toml:/app/config.toml:ro
    environment:
      - HMAC_CONFIG_PATH=/app/config.toml
    networks:
      - hmac-network
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 30s
      timeout: 10s
      retries: 3

  redis:
    image: redis:7-alpine
    container_name: hmac-redis
    restart: unless-stopped
    volumes:
      - redis_data:/data
    networks:
      - hmac-network
    command: redis-server --appendonly yes

  prometheus:
    image: prom/prometheus:latest
    container_name: hmac-prometheus
    restart: unless-stopped
    ports:
      - "9090:9090"
    volumes:
      - ./config/prometheus.yml:/etc/prometheus/prometheus.yml:ro
      - prometheus_data:/prometheus
    networks:
      - hmac-network
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'

  grafana:
    image: grafana/grafana:latest
    container_name: hmac-grafana
    restart: unless-stopped
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin123
    volumes:
      - grafana_data:/var/lib/grafana
      - ./config/grafana-datasources.yml:/etc/grafana/provisioning/datasources/datasources.yml:ro
    networks:
      - hmac-network

  nginx:
    image: nginx:alpine
    container_name: hmac-nginx
    restart: unless-stopped
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./config/nginx.conf:/etc/nginx/nginx.conf:ro
      - ./config/ssl:/etc/nginx/ssl:ro
    depends_on:
      - hmac-file-server
    networks:
      - hmac-network

volumes:
  uploads:
  dedup:
  logs:
  redis_data:
  prometheus_data:
  grafana_data:

networks:
  hmac-network:
    driver: bridge
```

### Docker Compose with ClamAV
```yaml
# docker-compose.clamav.yml
version: '3.8'

services:
  hmac-file-server:
    image: hmac-file-server:3.2
    container_name: hmac-file-server
    restart: unless-stopped
    depends_on:
      - clamav
    ports:
      - "8080:8080"
    volumes:
      - ./data/uploads:/app/uploads
      - ./data/logs:/app/logs
      - ./config.toml:/app/config.toml:ro
      - clamav_socket:/var/run/clamav
    networks:
      - hmac-network

  clamav:
    image: clamav/clamav:latest
    container_name: hmac-clamav
    restart: unless-stopped
    volumes:
      - clamav_db:/var/lib/clamav
      - clamav_socket:/var/run/clamav
    networks:
      - hmac-network
    environment:
      - CLAMAV_NO_FRESHCLAMD=false
      - CLAMAV_NO_CLAMD=false

volumes:
  clamav_db:
  clamav_socket:

networks:
  hmac-network:
    driver: bridge
```

---

## Podman Deployment

### Podman Build and Run (Alternative to Docker)

Podman is a daemonless container engine that's often preferred in enterprise environments for its security and rootless capabilities.

#### Build Container Image with Podman
```bash
# Clone repository
git clone https://github.com/PlusOne/hmac-file-server.git
cd hmac-file-server

# Build image with Podman
podman build --no-cache -t localhost/hmac-file-server:latest -f Dockerfile.podman .
```

#### Optimized Podman Dockerfile
```dockerfile
# Dockerfile.podman - Optimized for Podman deployment
FROM docker.io/golang:1.24-alpine AS builder

WORKDIR /build

# Install build dependencies
RUN apk add --no-cache git ca-certificates tzdata

# Clone and build HMAC File Server
RUN git clone https://github.com/PlusOne/hmac-file-server.git .
RUN go mod download
RUN CGO_ENABLED=0 go build -ldflags "-s -w" -o hmac-file-server ./cmd/server/

# Production stage - Alpine for better compatibility
FROM alpine:latest

# Install runtime dependencies
RUN apk add --no-cache ca-certificates tzdata curl shadow && \
    adduser -D -s /bin/sh -u 1011 appuser

# Create application directories
RUN mkdir -p /app /data /deduplication /iso /logs /tmp && \
    chown -R appuser:appuser /app /data /deduplication /iso /logs /tmp

# Copy binary and set permissions
COPY --from=builder /build/hmac-file-server /app/hmac-file-server
RUN chmod +x /app/hmac-file-server

# Switch to non-root user
USER appuser
WORKDIR /app

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
  CMD curl -f http://localhost:8888/health || exit 1

# Expose port
EXPOSE 8888

# Start server
ENTRYPOINT ["/app/hmac-file-server"]
```

#### Production Podman Configuration Script
```bash
#!/bin/bash
# deploy-podman.sh - Production Podman deployment script

# Configuration variables
app_name='hmac-file-server'
pod_name='xmpp-pod'
ctr_name="${pod_name}-${app_name}"
ctr_image='localhost/hmac-file-server:latest'
restart_policy='unless-stopped'
ctr_uid='1011'
app_data="/opt/podman/hmac-file-server"
listen_port='8888'
metrics_port='9090'

# Create application directories
sudo mkdir -p ${app_data}/{config,data,deduplication,logs}
sudo chown -R ${ctr_uid}:${ctr_uid} ${app_data}

# Set up proper permissions for Podman rootless
podman unshare chown -R ${ctr_uid}:${ctr_uid} ${app_data}

# Create pod (similar to docker-compose network)
podman pod create --name "${pod_name}" \
    --publish ${listen_port}:8888 \
    --publish ${metrics_port}:9090

# Generate configuration if it doesn't exist
if [ ! -f "${app_data}/config/config.toml" ]; then
    echo "Generating configuration..."
    cat > ${app_data}/config/config.toml << 'EOF'
# HMAC File Server - Podman Production Configuration
[server]
listen_address = "8888"
storage_path = "/data"
metrics_enabled = true
metrics_port = "9090"
max_upload_size = "10GB"
enable_dynamic_workers = true
worker_scale_up_thresh = 40
worker_scale_down_thresh = 10

[uploads]
allowed_extensions = [".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp", ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"]
chunked_uploads_enabled = true
chunk_size = "32MB"
resumable_uploads_enabled = true
max_resumable_age = "48h"

[downloads]
resumable_downloads_enabled = true
chunked_downloads_enabled = true
chunk_size = "32MB"
allowed_extensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"]

[security]
secret = "CHANGE-THIS-PRODUCTION-SECRET"
enablejwt = true
jwtsecret = "CHANGE-THIS-JWT-SECRET"
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
EOF
    
    echo "⚠️  IMPORTANT: Edit ${app_data}/config/config.toml and change the secrets!"
fi

# Build image if it doesn't exist
if ! podman image exists ${ctr_image}; then
    echo "Building container image..."
    podman build --no-cache -t ${ctr_image} -f Dockerfile.podman .
fi

# Stop and remove existing container
podman container stop ${ctr_name} 2>/dev/null || true
podman container rm ${ctr_name} 2>/dev/null || true

# Run container with security-hardened settings
echo "Starting HMAC File Server container..."
podman run -d \
    --pod="${pod_name}" \
    --restart="${restart_policy}" \
    --name "${ctr_name}" \
    --user ${ctr_uid}:${ctr_uid} \
    --cap-drop=ALL \
    --security-opt no-new-privileges \
    --read-only \
    --tmpfs /tmp:rw,noexec,nosuid,size=100m \
    -v ${app_data}/config/config.toml:/app/config.toml:ro,Z \
    -v ${app_data}/data:/data:rw,Z \
    -v ${app_data}/deduplication:/deduplication:rw,Z \
    -v ${app_data}/logs:/logs:rw,Z \
    --health-cmd="curl -f http://localhost:8888/health || exit 1" \
    --health-interval=30s \
    --health-timeout=10s \
    --health-retries=3 \
    --health-start-period=40s \
    "${ctr_image}" -config /app/config.toml

echo "✅ HMAC File Server deployed successfully!"
echo "🌐 Server available at: http://localhost:${listen_port}"
echo "📊 Metrics available at: http://localhost:${metrics_port}/metrics"
echo "📋 Container status: podman ps"
echo "📝 View logs: podman logs ${ctr_name}"
echo "🔍 Health check: curl -f http://localhost:${listen_port}/health"
```

#### Podman Systemd Service (Rootless)
```ini
# ~/.config/systemd/user/hmac-file-server.service
[Unit]
Description=HMAC File Server (Podman)
Documentation=https://github.com/PlusOne/hmac-file-server
Wants=network-online.target
After=network-online.target
RequiresMountsFor=%t/containers

[Service]
Environment=PODMAN_SYSTEMD_UNIT=%n
Restart=on-failure
TimeoutStopSec=70
ExecStart=/usr/bin/podman run \
    --cidfile=%t/%n.ctr-id \
    --cgroups=no-conmon \
    --rm \
    --sdnotify=conmon \
    --replace \
    --name hmac-file-server \
    --user 1011:1011 \
    --cap-drop=ALL \
    --security-opt no-new-privileges \
    --read-only \
    --tmpfs /tmp:rw,noexec,nosuid,size=100m \
    --publish 8888:8888 \
    --publish 9090:9090 \
    -v /opt/podman/hmac-file-server/config/config.toml:/app/config.toml:ro,Z \
    -v /opt/podman/hmac-file-server/data:/data:rw,Z \
    -v /opt/podman/hmac-file-server/deduplication:/deduplication:rw,Z \
    -v /opt/podman/hmac-file-server/logs:/logs:rw,Z \
    localhost/hmac-file-server:latest -config /app/config.toml

ExecStop=/usr/bin/podman stop --ignore --cidfile=%t/%n.ctr-id
ExecStopPost=/usr/bin/podman rm -f --ignore --cidfile=%t/%n.ctr-id
Type=notify
NotifyAccess=all

[Install]
WantedBy=default.target
```

#### Podman Commands Reference
```bash
# Build image
podman build -t localhost/hmac-file-server:latest -f Dockerfile.podman .

# Run with basic settings
podman run -d --name hmac-file-server \
    -p 8888:8888 \
    -v ./config.toml:/app/config.toml:ro \
    -v ./data:/data:rw \
    localhost/hmac-file-server:latest -config /app/config.toml

# Create and manage pods
podman pod create --name xmpp-services -p 8888:8888 -p 9090:9090
podman run -d --pod xmpp-services --name hmac-file-server localhost/hmac-file-server:latest

# View logs and status
podman logs hmac-file-server
podman ps -a
podman pod ps

# Health check
podman healthcheck run hmac-file-server

# Stop and cleanup
podman stop hmac-file-server
podman rm hmac-file-server
podman pod stop xmpp-services
podman pod rm xmpp-services

# Rootless systemd management
systemctl --user daemon-reload
systemctl --user enable hmac-file-server.service
systemctl --user start hmac-file-server.service
systemctl --user status hmac-file-server.service
```

#### Podman with XMPP Integration
```bash
# Complete XMPP + HMAC File Server pod setup
podman pod create --name xmpp-pod \
    --publish 5222:5222 \
    --publish 5269:5269 \
    --publish 5443:5443 \
    --publish 8888:8888

# Run Prosody XMPP server
podman run -d --pod xmpp-pod --name prosody \
    -v ./prosody/config:/etc/prosody:ro \
    -v ./prosody/data:/var/lib/prosody:rw \
    docker.io/prosody/prosody:latest

# Run HMAC File Server
podman run -d --pod xmpp-pod --name hmac-file-server \
    -v ./hmac/config.toml:/app/config.toml:ro \
    -v ./hmac/data:/data:rw \
    localhost/hmac-file-server:latest -config /app/config.toml

# Check pod status
podman pod ps
podman ps --pod
```

### Podman vs Docker Comparison

| Feature | Docker | Podman |
|---------|--------|--------|
| **Daemon** | Requires Docker daemon | Daemonless architecture |
| **Root Access** | Requires root for Docker daemon | Can run completely rootless |
| **Security** | Good, but daemon runs as root | Enhanced security, no privileged daemon |
| **Systemd Integration** | Via Docker service | Native systemd integration |
| **Pod Support** | Requires docker-compose or swarm | Native Kubernetes-style pods |
| **Image Compatibility** | Docker images | Compatible with Docker images |
| **Enterprise Use** | Popular in startups/mid-size | Preferred in enterprise environments |
| **SELinux** | Basic support | Excellent SELinux integration |

### Podman Benefits for HMAC File Server

1. **Enhanced Security**: No privileged daemon, better isolation
2. **Rootless Operation**: Can run without root privileges
3. **SELinux Integration**: Better compliance in enterprise environments
4. **Systemd Native**: Better integration with system services
5. **Pod Support**: Natural clustering with XMPP servers
6. **Resource Efficiency**: Lower overhead without daemon

### Testing Results & Verification

The Podman deployment has been fully tested and verified:

#### ✅ **Installation Success**
- **Docker Removal**: Complete removal of Docker packages and dependencies
- **Podman Installation**: Podman 4.3.1 installed with all dependencies (`fuse-overlayfs`, `slirp4netns`, `uidmap`)
- **Image Build**: Successfully built `localhost/hmac-file-server:latest` with security optimizations

#### ✅ **Container Deployment Success**
- **Security Hardened**: Running as non-root user (UID 1011) with `--cap-drop=ALL`, `--read-only`, `--no-new-privileges`
- **Health Checks**: Built-in health monitoring and status reporting
- **Volume Mounting**: Proper SELinux labeling with `:Z` flags

#### ✅ **Functional Verification**
```bash
# Health endpoint test
curl -f http://localhost:8888/health
# Response: OK ✅

# Metrics endpoint test  
curl -s http://localhost:9090/metrics | head -5
# Response: Prometheus metrics ✅

# Container status
podman ps
# Status: Up and running ✅

# Configuration validation
podman logs hmac-file-server
# Result: All settings validated ✅
```

#### ✅ **Production Ready Features**
- **XMPP Integration**: Pod networking for multi-service XMPP deployments
- **Configuration Management**: Auto-generated secure configs with random secrets
- **Service Management**: Native systemd integration for both rootless and system-wide deployment
- **Enterprise Security**: Enhanced security features preferred in enterprise environments

### Quick Start Commands

```bash
# Complete deployment in one command
cd hmac-file-server/dockerenv/podman && ./deploy-podman.sh

# Manual deployment
podman build -t localhost/hmac-file-server:latest -f dockerenv/podman/Dockerfile.podman .
podman run -d --name hmac-file-server \
  -p 8888:8888 -p 9090:9090 \
  -v ./config.toml:/app/config.toml:ro \
  -v ./data:/data:rw \
  localhost/hmac-file-server:latest -config /app/config.toml

# Health verification
curl -f http://localhost:8888/health && echo " - Server is healthy! ✅"
```

### Manual Setup: Paths, Ownership & Permissions

When setting up Podman or Docker manually, proper path ownership is crucial:

#### **Container User Configuration**
- **Container User**: `appuser` (UID: 1011, GID: 1011)
- **Security**: Non-root user for enhanced security
- **Compatibility**: Works with both rootless and rootful containers

#### **Required Directory Structure**
```bash
# Create base directory structure
mkdir -p /opt/podman/hmac-file-server/{config,data,deduplication,logs}

# Set proper ownership (CRITICAL for container access)
# For Podman rootless:
podman unshare chown -R 1011:1011 /opt/podman/hmac-file-server

# For Docker or Podman rootful:
chown -R 1011:1011 /opt/podman/hmac-file-server

# Set proper permissions
chmod 755 /opt/podman/hmac-file-server/{config,data,deduplication,logs}
chmod 644 /opt/podman/hmac-file-server/config/config.toml  # Read-only config
```

#### **Path Mapping Reference**

| Host Path | Container Path | Purpose | Required Permissions |
|-----------|----------------|---------|---------------------|
| `/opt/podman/hmac-file-server/config/config.toml` | `/app/config.toml` | Configuration file | `644` (read-only) |
| `/opt/podman/hmac-file-server/data/` | `/data/` | File uploads/storage | `755` (read-write) |
| `/opt/podman/hmac-file-server/deduplication/` | `/deduplication/` | Deduplication cache | `755` (read-write) |
| `/opt/podman/hmac-file-server/logs/` | `/logs/` | Application logs | `755` (read-write) |

#### **SELinux Labels (Important for RHEL/CentOS/Fedora)**
```bash
# Add SELinux labels for Podman volume mounts
podman run -d --name hmac-file-server \
  -v /opt/podman/hmac-file-server/config/config.toml:/app/config.toml:ro,Z \
  -v /opt/podman/hmac-file-server/data:/data:rw,Z \
  -v /opt/podman/hmac-file-server/deduplication:/deduplication:rw,Z \
  -v /opt/podman/hmac-file-server/logs:/logs:rw,Z \
  localhost/hmac-file-server:latest

# Note: The `:Z` flag relabels content and should be used for private volumes
# Use `:z` for shared volumes between multiple containers
```

#### **Common Ownership Issues & Solutions**

**❌ Problem**: Container fails with permission errors
```bash
# Logs show: "permission denied: open /data/.write_test"
```

**✅ Solution**: Fix ownership and verify
```bash
# Fix ownership
chown -R 1011:1011 /opt/podman/hmac-file-server

# Verify ownership
ls -la /opt/podman/hmac-file-server/
# Should show: drwxr-xr-x 2 1011 1011

# Test write permissions
sudo -u "#1011" touch /opt/podman/hmac-file-server/data/test.txt
# Should succeed without errors
```

**❌ Problem**: SELinux blocking container access  
```bash
# Logs show: "SELinux is preventing access"
```

**✅ Solution**: Correct SELinux labeling
```bash
# Option 1: Use :Z labels in volume mounts (recommended)
-v /path/to/data:/data:rw,Z

# Option 2: Set SELinux context manually
sudo setsebool -P container_manage_cgroup on
sudo restorecon -R /opt/podman/hmac-file-server
```

#### **Docker vs Podman Ownership Differences**

| Scenario | Docker | Podman Rootless | Podman Rootful |
|----------|--------|-----------------|----------------|
| **Host UID** | 1011:1011 | 1011:1011 | 1011:1011 |
| **Container UID** | 1011:1011 | 1011:1011 | 1011:1011 |
| **Volume Ownership** | `chown 1011:1011` | `podman unshare chown 1011:1011` | `chown 1011:1011` |
| **SELinux Labels** | `:Z` or `:z` | `:Z` or `:z` | `:Z` or `:z` |

#### **Verification Commands**
```bash
# Check container user
podman exec hmac-file-server id
# Expected: uid=1011(appuser) gid=1011(appuser)

# Check file permissions in container
podman exec hmac-file-server ls -la /data /deduplication /logs
# Expected: drwxr-xr-x appuser appuser

# Verify write access
podman exec hmac-file-server touch /data/permission-test
# Should succeed without errors

# Check configuration access
podman exec hmac-file-server cat /app/config.toml | head -5
# Should display config content
```

---

## Nginx Reverse Proxy

### Basic Nginx Configuration
```nginx
# /etc/nginx/sites-available/hmac-file-server
server {
    listen 80;
    server_name files.example.com;
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name files.example.com;
    
    # SSL Configuration
    ssl_certificate /etc/ssl/certs/files.example.com.crt;
    ssl_certificate_key /etc/ssl/private/files.example.com.key;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers ECDHE-RSA-AES256-GCM-SHA512:DHE-RSA-AES256-GCM-SHA512:ECDHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES256-GCM-SHA384;
    ssl_prefer_server_ciphers off;
    ssl_session_cache shared:SSL:10m;
    
    # File upload limits
    client_max_body_size 10G;
    client_body_timeout 300s;
    client_header_timeout 300s;
    
    # Proxy settings for large files
    proxy_connect_timeout 300s;
    proxy_send_timeout 300s;
    proxy_read_timeout 300s;
    proxy_request_buffering off;
    proxy_buffering off;
    
    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # WebSocket support (if needed)
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
    }
    
    # Metrics endpoint (optional - restrict access)
    location /metrics {
        proxy_pass http://127.0.0.1:9090;
        allow 10.0.0.0/8;
        allow 172.16.0.0/12;
        allow 192.168.0.0/16;
        deny all;
    }
    
    # Health check
    location /health {
        proxy_pass http://127.0.0.1:8080;
        access_log off;
    }
}
```

### Advanced Nginx Configuration with Load Balancing
```nginx
# /etc/nginx/nginx.conf
upstream hmac_backend {
    least_conn;
    server 127.0.0.1:8080 max_fails=3 fail_timeout=30s;
    server 127.0.0.1:8081 max_fails=3 fail_timeout=30s backup;
    keepalive 32;
}

server {
    listen 443 ssl http2;
    server_name files.example.com;
    
    # Security headers
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
    add_header X-Frame-Options DENY always;
    add_header X-Content-Type-Options nosniff always;
    add_header X-XSS-Protection "1; mode=block" always;
    add_header Referrer-Policy "strict-origin-when-cross-origin" always;
    
    # Rate limiting
    limit_req_zone $binary_remote_addr zone=upload:10m rate=10r/m;
    limit_req_zone $binary_remote_addr zone=download:10m rate=100r/m;
    
    location /upload {
        limit_req zone=upload burst=5 nodelay;
        proxy_pass http://hmac_backend;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
    
    location /download {
        limit_req zone=download burst=20 nodelay;
        proxy_pass http://hmac_backend;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
    
    location / {
        proxy_pass http://hmac_backend;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

---

## Apache2 Reverse Proxy

### Basic Apache2 Configuration
```apache
# /etc/apache2/sites-available/hmac-file-server.conf
<VirtualHost *:80>
    ServerName files.example.com
    Redirect permanent / https://files.example.com/
</VirtualHost>

<VirtualHost *:443>
    ServerName files.example.com
    
    # SSL Configuration
    SSLEngine on
    SSLCertificateFile /etc/ssl/certs/files.example.com.crt
    SSLCertificateKeyFile /etc/ssl/private/files.example.com.key
    SSLProtocol all -SSLv3 -TLSv1 -TLSv1.1
    SSLCipherSuite ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305
    SSLHonorCipherOrder off
    SSLSessionTickets off
    
    # Security Headers
    Header always set Strict-Transport-Security "max-age=31536000; includeSubDomains"
    Header always set X-Frame-Options DENY
    Header always set X-Content-Type-Options nosniff
    Header always set X-XSS-Protection "1; mode=block"
    Header always set Referrer-Policy "strict-origin-when-cross-origin"
    
    # File upload limits
    LimitRequestBody 10737418240  # 10GB
    
    # Proxy configuration
    ProxyPreserveHost On
    ProxyRequests Off
    
    # Main proxy
    ProxyPass / http://127.0.0.1:8080/
    ProxyPassReverse / http://127.0.0.1:8080/
    
    # Set headers for the backend
    ProxyPassReverse / http://127.0.0.1:8080/
    ProxySetEnv proxy-initial-not-pooled 1
    
    # Timeout settings
    ProxyTimeout 300
    
    # Metrics endpoint (restricted)
    <Location "/metrics">
        ProxyPass http://127.0.0.1:9090/
        ProxyPassReverse http://127.0.0.1:9090/
        Require ip 10.0.0.0/8
        Require ip 172.16.0.0/12
        Require ip 192.168.0.0/16
    </Location>
    
    # Logging
    ErrorLog ${APACHE_LOG_DIR}/hmac-file-server_error.log
    CustomLog ${APACHE_LOG_DIR}/hmac-file-server_access.log combined
</VirtualHost>
```

### Advanced Apache2 Configuration with Load Balancing
```apache
# Enable required modules
# a2enmod proxy proxy_http proxy_balancer lbmethod_byrequests ssl headers

# /etc/apache2/sites-available/hmac-file-server-lb.conf
<Proxy "balancer://hmac-cluster">
    BalancerMember http://127.0.0.1:8080 status=+H
    BalancerMember http://127.0.0.1:8081 status=+H
    ProxySet lbmethod=byrequests
</Proxy>

<VirtualHost *:443>
    ServerName files.example.com
    
    # SSL Configuration (same as basic)
    SSLEngine on
    SSLCertificateFile /etc/ssl/certs/files.example.com.crt
    SSLCertificateKeyFile /etc/ssl/private/files.example.com.key
    
    # Rate limiting (requires mod_evasive)
    DOSHashTableSize    8192
    DOSPageCount        3
    DOSPageInterval     1
    DOSEmailNotify      admin@example.com
    
    # Proxy to load balancer
    ProxyPreserveHost On
    ProxyPass / balancer://hmac-cluster/
    ProxyPassReverse / balancer://hmac-cluster/
    
    # Balancer manager (restrict access)
    <Location "/balancer-manager">
        SetHandler balancer-manager
        Require ip 127.0.0.1
        Require ip 10.0.0.0/8
    </Location>
    ProxyPass /balancer-manager !
    
    # Health check
    <Location "/health">
        ProxyPass http://127.0.0.1:8080/health
        ProxyPassReverse http://127.0.0.1:8080/health
    </Location>
</VirtualHost>
```

---

## Prosody XMPP Integration

### Prosody Configuration
```lua
-- /etc/prosody/prosody.cfg.lua
-- HMAC File Server integration for XEP-0363

-- Enable HTTP file upload module
modules_enabled = {
    -- Core modules
    "roster";
    "saslauth";
    "tls";
    "dialback";
    "disco";
    "carbons";
    "pep";
    "private";
    "blocklist";
    "vcard4";
    "vcard_legacy";
    "version";
    "uptime";
    "time";
    "ping";
    "admin_adhoc";
    
    -- HTTP file upload
    "http_upload_external";
}

-- VirtualHost configuration
VirtualHost "example.com"
    enabled = true
    
    -- SSL configuration
    ssl = {
        key = "/etc/prosody/certs/example.com.key";
        certificate = "/etc/prosody/certs/example.com.crt";
    }
    
    -- HTTP file upload configuration
    http_upload_external_base_url = "https://files.example.com"
    http_upload_external_secret = "your-very-secret-hmac-key"
    http_upload_external_file_size_limit = 10737418240  -- 10GB
    http_upload_external_quota = 1073741824000          -- 1TB per user
    
    -- Custom upload URL patterns (for HMAC File Server)
    http_upload_external_put_url = "https://files.example.com/upload/{filename}"
    http_upload_external_get_url = "https://files.example.com/download/{filename}"

-- Component for file upload service
Component "upload.example.com" "http_upload_external"
    http_upload_external_base_url = "https://files.example.com"
    http_upload_external_secret = "your-very-secret-hmac-key"
    http_upload_external_file_size_limit = 10737418240
    
-- Logging
log = {
    info = "/var/log/prosody/prosody.log";
    error = "/var/log/prosody/prosody.err";
    "*syslog";
}
```

### Prosody Module Configuration
```lua
-- /usr/lib/prosody/modules/mod_http_upload_external.lua
-- Custom module for HMAC File Server integration

local hmac_sha256 = require "util.hashes".hmac_sha256;
local base64 = require "util.encodings".base64;
local uuid = require "util.uuid".generate;
local http = require "net.http";

module:depends("disco");

local external_base_url = module:get_option_string("http_upload_external_base_url");
local external_secret = module:get_option_string("http_upload_external_secret");
local file_size_limit = module:get_option_number("http_upload_external_file_size_limit", 100*1024*1024);
local quota = module:get_option_number("http_upload_external_quota", 1024*1024*1024);

-- XEP-0363 disco feature
module:add_feature("urn:xmpp:http:upload:0");

-- Handle upload requests
function handle_upload_request(event)
    local stanza = event.stanza;
    local filename = stanza:get_child_text("filename", "urn:xmpp:http:upload:0");
    local filesize = tonumber(stanza:get_child_text("size", "urn:xmpp:http:upload:0"));
    local content_type = stanza:get_child_text("content-type", "urn:xmpp:http:upload:0") or "application/octet-stream";
    
    if not filename or not filesize then
        return st.error_reply(stanza, "modify", "bad-request", "Missing filename or size");
    end
    
    if filesize > file_size_limit then
        return st.error_reply(stanza, "modify", "not-acceptable", "File too large");
    end
    
    -- Generate HMAC authentication
    local timestamp = os.time();
    local upload_id = uuid();
    local message = filename .. filesize .. timestamp .. upload_id;
    local signature = base64.encode(hmac_sha256(external_secret, message));
    
    -- Construct URLs
    local put_url = string.format("%s/upload?filename=%s&timestamp=%d&uploadid=%s&signature=%s",
        external_base_url, 
        filename,
        timestamp,
        upload_id,
        signature
    );
    
    local get_url = string.format("%s/download/%s", external_base_url, filename);
    
    -- Return slot
    local reply = st.reply(stanza)
        :tag("slot", {xmlns="urn:xmpp:http:upload:0"})
            :tag("put", {url=put_url}):up()
            :tag("get", {url=get_url}):up()
        :up();
    
    return reply;
end

module:hook("iq-get/host/urn:xmpp:http:upload:0:request", handle_upload_request);
```

---

## Ejabberd XMPP Integration

### Ejabberd Configuration
```yaml
# /etc/ejabberd/ejabberd.yml
# HMAC File Server integration

hosts:
  - "example.com"

listen:
  -
    port: 5222
    ip: "::"
    module: ejabberd_c2s
    starttls: true
    certfile: "/etc/ejabberd/certs/example.com.pem"
    
  -
    port: 5269
    ip: "::"
    module: ejabberd_s2s_in
    
  -
    port: 5443
    ip: "::"
    module: ejabberd_http
    tls: true
    certfile: "/etc/ejabberd/certs/example.com.pem"
    request_handlers:
      "/upload": mod_http_upload
      "/admin": ejabberd_web_admin
      "/api": mod_http_api

modules:
  mod_adhoc: {}
  mod_admin_extra: {}
  mod_announce: {}
  mod_avatar: {}
  mod_blocking: {}
  mod_bosh: {}
  mod_caps: {}
  mod_carboncopy: {}
  mod_client_state: {}
  mod_configure: {}
  mod_disco: {}
  mod_fail2ban: {}
  mod_http_api: {}
  mod_http_upload:
    put_url: "https://files.example.com/upload"
    get_url: "https://files.example.com/download"
    external_secret: "your-very-secret-hmac-key"
    max_size: 10737418240  # 10GB
    thumbnail: false
    custom_headers:
      "Access-Control-Allow-Origin": "*"
      "Access-Control-Allow-Methods": "GET,HEAD,PUT,OPTIONS"
      "Access-Control-Allow-Headers": "Content-Type"
  mod_last: {}
  mod_mam: {}
  mod_mqtt: {}
  mod_muc: {}
  mod_muc_admin: {}
  mod_offline: {}
  mod_ping: {}
  mod_privacy: {}
  mod_private: {}
  mod_proxy65: {}
  mod_pubsub: {}
  mod_push: {}
  mod_register: {}
  mod_roster: {}
  mod_shared_roster: {}
  mod_stats: {}
  mod_time: {}
  mod_vcard: {}
  mod_version: {}

# Authentication
auth_method: internal

# Database
default_db: mnesia

# Access rules
access_rules:
  local:
    - allow: local
  c2s:
    - deny: blocked
    - allow
  announce:
    - allow: admin
  configure:
    - allow: admin
  muc_create:
    - allow: local
  pubsub_createnode:
    - allow: local
  register:
    - allow
  trusted_network:
    - allow: loopback

# ACL
acl:
  local:
    user_regexp: ""
  loopback:
    ip:
      - "127.0.0.0/8"
      - "::1/128"
      - "::FFFF:127.0.0.1/128"
  admin:
    user:
      - "admin@example.com"

# Logging
loglevel: 4
log_rotate_size: 10485760
log_rotate_count: 5
```

### Custom Ejabberd HTTP Upload Module
```erlang
% /opt/ejabberd/lib/ejabberd-23.01/ebin/mod_http_upload_external.erl
% Custom module for HMAC File Server integration

-module(mod_http_upload_external).
-author('admin@example.com').

-behaviour(gen_mod).

-export([start/2, stop/1, process_iq/1, mod_opt_type/1, mod_options/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

start(Host, Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                                  ?NS_HTTP_UPLOAD_0, ?MODULE,
                                  process_iq).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_HTTP_UPLOAD_0).

process_iq(#iq{type = get, sub_els = [#upload_request{filename = Filename,
                                                     size = Size,
                                                     'content-type' = ContentType}]} = IQ) ->
    Host = ejabberd_config:get_myname(),
    
    % Get configuration
    PutURL = gen_mod:get_module_opt(Host, ?MODULE, put_url),
    GetURL = gen_mod:get_module_opt(Host, ?MODULE, get_url),
    Secret = gen_mod:get_module_opt(Host, ?MODULE, external_secret),
    MaxSize = gen_mod:get_module_opt(Host, ?MODULE, max_size),
    
    % Validate file size
    case Size =< MaxSize of
        true ->
            % Generate HMAC signature
            Timestamp = erlang:system_time(second),
            UploadId = uuid:uuid_to_string(uuid:get_v4()),
            Message = <<Filename/binary, (integer_to_binary(Size))/binary,
                       (integer_to_binary(Timestamp))/binary, UploadId/binary>>,
            Signature = base64:encode(crypto:mac(hmac, sha256, Secret, Message)),
            
            % Construct URLs
            PutURLFinal = <<PutURL/binary, "?filename=", Filename/binary,
                           "&timestamp=", (integer_to_binary(Timestamp))/binary,
                           "&uploadid=", UploadId/binary,
                           "&signature=", Signature/binary>>,
            GetURLFinal = <<GetURL/binary, "/", Filename/binary>>,
            
            % Return slot
            Slot = #upload_slot{get = GetURLFinal, put = PutURLFinal},
            xmpp:make_iq_result(IQ, Slot);
        false ->
            xmpp:make_error(IQ, xmpp:err_not_acceptable(<<"File too large">>, ?MYLANG))
    end;

process_iq(IQ) ->
    xmpp:make_error(IQ, xmpp:err_bad_request()).

mod_opt_type(put_url) -> fun iolist_to_binary/1;
mod_opt_type(get_url) -> fun iolist_to_binary/1;
mod_opt_type(external_secret) -> fun iolist_to_binary/1;
mod_opt_type(max_size) -> fun(I) when is_integer(I), I > 0 -> I end.

mod_options(_Host) ->
    [{put_url, <<"">>},
     {get_url, <<"">>},
     {external_secret, <<"">>},
     {max_size, 104857600}].
```

---

## XEP-0363 Implementation

### XEP-0363: HTTP File Upload

HMAC File Server implements [XEP-0363: HTTP File Upload](https://xmpp.org/extensions/xep-0363.html) with HMAC authentication for secure file sharing in XMPP environments.

#### Protocol Flow

1. **Discovery**: Client discovers upload service
2. **Request Slot**: Client requests upload/download URLs
3. **Upload**: Client uploads file to provided PUT URL
4. **Share**: Client shares GET URL with contacts
5. **Download**: Recipients download using GET URL

#### Implementation Details

```xml
<!-- 1. Service Discovery -->
<iq type='get' to='upload.example.com' id='discover'>
  <query xmlns='http://jabber.org/protocol/disco#info'/>
</iq>

<!-- Response -->
<iq type='result' from='upload.example.com' id='discover'>
  <query xmlns='http://jabber.org/protocol/disco#info'>
    <feature var='urn:xmpp:http:upload:0'/>
    <x xmlns='jabber:x:data' type='result'>
      <field var='max-file-size'>
        <value>10737418240</value>  <!-- 10GB -->
      </field>
    </x>
  </query>
</iq>

<!-- 2. Request Upload Slot -->
<iq type='get' to='upload.example.com' id='upload1'>
  <request xmlns='urn:xmpp:http:upload:0' filename='document.pdf' size='1048576' content-type='application/pdf'/>
</iq>

<!-- Response with URLs -->
<iq type='result' from='upload.example.com' id='upload1'>
  <slot xmlns='urn:xmpp:http:upload:0'>
    <put url='https://files.example.com/upload?filename=document.pdf&amp;timestamp=1642678800&amp;uploadid=550e8400-e29b-41d4-a716-446655440000&amp;signature=YWJjZGVmZ2hpams='/>
    <get url='https://files.example.com/download/document.pdf'/>
  </slot>
</iq>

<!-- 3. HTTP Upload -->
PUT /upload?filename=document.pdf&timestamp=1642678800&uploadid=550e8400-e29b-41d4-a716-446655440000&signature=YWJjZGVmZ2hpams= HTTP/1.1
Host: files.example.com
Content-Type: application/pdf
Content-Length: 1048576

[Binary file data]

<!-- 4. Share Download URL -->
<message to='friend@example.com' type='chat'>
  <body>Check out this document:</body>
  <x xmlns='jabber:x:oob'>
    <url>https://files.example.com/download/document.pdf</url>
  </x>
</message>
```

#### HMAC Authentication

HMAC File Server uses HMAC-SHA256 for authentication:

```
message = filename + filesize + timestamp + upload_id
signature = base64(hmac_sha256(secret, message))
```

#### Configuration for XEP-0363

```toml
[server]
listen_address = "8080"
storage_path = "/var/lib/hmac-file-server/uploads"

[security]
secret = "your-xmpp-upload-secret"

[uploads]
allowed_extensions = [".jpg", ".png", ".gif", ".pdf", ".doc", ".zip"]
max_upload_size = "10GB"
chunked_uploads_enabled = true
resumable_uploads_enabled = true

[timeouts]
readtimeout = "300s"
writetimeout = "300s"
idletimeout = "300s"
```

---

## API Versions

### Overview

HMAC File Server supports multiple API versions to ensure compatibility with different XMPP clients and custom integrations.

### V1 API (Legacy Support)

**Endpoint**: `/api/v1/upload`  
**Authentication**: Basic HMAC  
**Usage**: Legacy XMPP clients, basic integrations

```bash
# V1 Upload
curl -X POST "https://files.example.com/api/v1/upload" \
  -H "Authorization: HMAC-SHA256 signature" \
  -H "Content-Type: multipart/form-data" \
  -F "file=@document.pdf"

# Response
{
  "success": true,
  "url": "https://files.example.com/download/document.pdf",
  "filename": "document.pdf",
  "size": 1048576
}
```

### V2 API (Enhanced)

**Endpoint**: `/api/v2/upload`  
**Authentication**: Enhanced HMAC with timestamps  
**Usage**: Modern XMPP clients, advanced features

```bash
# V2 Upload with metadata
curl -X POST "https://files.example.com/api/v2/upload" \
  -H "Authorization: HMAC-SHA256-V2 signature" \
  -H "X-Upload-Timestamp: 1642678800" \
  -H "X-Upload-ID: 550e8400-e29b-41d4-a716-446655440000" \
  -H "Content-Type: multipart/form-data" \
  -F "file=@document.pdf" \
  -F "metadata={\"title\":\"My Document\",\"tags\":[\"important\"]}"

# Response
{
  "success": true,
  "version": "v2",
  "upload_id": "550e8400-e29b-41d4-a716-446655440000",
  "url": "https://files.example.com/download/document.pdf",
  "thumbnail_url": "https://files.example.com/thumbnail/document.pdf",
  "filename": "document.pdf",
  "size": 1048576,
  "content_type": "application/pdf",
  "checksum": "sha256:d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2",
  "expires_at": "2024-01-21T12:00:00Z",
  "metadata": {
    "title": "My Document",
    "tags": ["important"]
  }
}
```

### V3 API (Custom/Experimental)

**Endpoint**: `/api/v3/upload`  
**Authentication**: JWT or Enhanced HMAC  
**Usage**: Custom clients, experimental features  
**Note**: V3 is not a defined standard - custom implementation

```bash
# V3 Upload with JWT
curl -X POST "https://files.example.com/api/v3/upload" \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  -H "Content-Type: application/json" \
  -d '{
    "filename": "document.pdf",
    "content_type": "application/pdf",
    "size": 1048576,
    "checksum": "sha256:d2d2d2d2...",
    "options": {
      "chunked": true,
      "resumable": true,
      "encryption": "aes256",
      "compression": "gzip"
    }
  }'

# Response
{
  "success": true,
  "version": "v3",
  "session_id": "sess_550e8400-e29b-41d4-a716-446655440000",
  "upload_urls": {
    "put": "https://files.example.com/api/v3/upload/sess_550e8400-e29b-41d4-a716-446655440000",
    "chunks": [
      "https://files.example.com/api/v3/chunk/1",
      "https://files.example.com/api/v3/chunk/2"
    ]
  },
  "download_url": "https://files.example.com/api/v3/download/document.pdf",
  "websocket_url": "wss://files.example.com/api/v3/progress/sess_550e8400-e29b-41d4-a716-446655440000",
  "capabilities": [
    "chunked_upload",
    "resumable_upload", 
    "deduplication",
    "encryption",
    "compression",
    "real_time_progress"
  ],
  "expires_at": "2024-01-21T12:00:00Z"
}
```

### Token Formats

#### V1 Token (Basic HMAC)
```
Authorization: HMAC-SHA256 <base64(hmac_sha256(secret, message))>
message = filename + content_type + timestamp
```

#### V2 Token (Enhanced HMAC)
```
Authorization: HMAC-SHA256-V2 <base64(hmac_sha256(secret, message))>
message = method + uri + timestamp + upload_id + content_hash
```

#### V3 Token (JWT)
```javascript
// JWT Header
{
  "alg": "HS256",
  "typ": "JWT"
}

// JWT Payload
{
  "iss": "hmac-file-server",
  "sub": "user@example.com",
  "aud": "files.example.com",
  "exp": 1642682400,
  "iat": 1642678800,
  "jti": "550e8400-e29b-41d4-a716-446655440000",
  "scope": ["upload", "download", "delete"],
  "upload_quota": 10737418240,
  "allowed_types": ["image/*", "application/pdf"]
}
```

### Client Implementation Examples

#### JavaScript (V2 API)
```javascript
class HMACFileClient {
  constructor(baseUrl, secret) {
    this.baseUrl = baseUrl;
    this.secret = secret;
  }
  
  async upload(file, metadata = {}) {
    const timestamp = Math.floor(Date.now() / 1000);
    const uploadId = this.generateUUID();
    const message = `POST/api/v2/upload${timestamp}${uploadId}${file.name}${file.size}`;
    const signature = await this.hmacSHA256(this.secret, message);
    
    const formData = new FormData();
    formData.append('file', file);
    formData.append('metadata', JSON.stringify(metadata));
    
    const response = await fetch(`${this.baseUrl}/api/v2/upload`, {
      method: 'POST',
      headers: {
        'Authorization': `HMAC-SHA256-V2 ${signature}`,
        'X-Upload-Timestamp': timestamp.toString(),
        'X-Upload-ID': uploadId
      },
      body: formData
    });
    
    return response.json();
  }
  
  async hmacSHA256(key, message) {
    const encoder = new TextEncoder();
    const keyData = encoder.encode(key);
    const messageData = encoder.encode(message);
    
    const cryptoKey = await crypto.subtle.importKey(
      'raw', keyData, { name: 'HMAC', hash: 'SHA-256' }, false, ['sign']
    );
    
    const signature = await crypto.subtle.sign('HMAC', cryptoKey, messageData);
    return btoa(String.fromCharCode(...new Uint8Array(signature)));
  }
  
  generateUUID() {
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
      const r = Math.random() * 16 | 0;
      const v = c === 'x' ? r : (r & 0x3 | 0x8);
      return v.toString(16);
    });
  }
}
```

#### Python (V3 API with JWT)
```python
import jwt
import requests
import json
from datetime import datetime, timedelta

class HMACFileClientV3:
    def __init__(self, base_url, jwt_secret):
        self.base_url = base_url
        self.jwt_secret = jwt_secret
    
    def generate_token(self, user_id, scopes=['upload', 'download']):
        payload = {
            'iss': 'hmac-file-server',
            'sub': user_id,
            'aud': self.base_url,
            'exp': datetime.utcnow() + timedelta(hours=1),
            'iat': datetime.utcnow(),
            'scope': scopes
        }
        return jwt.encode(payload, self.jwt_secret, algorithm='HS256')
    
    def upload(self, filename, content_type, size, user_id='anonymous'):
        token = self.generate_token(user_id)
        
        headers = {
            'Authorization': f'Bearer {token}',
            'Content-Type': 'application/json'
        }
        
        data = {
            'filename': filename,
            'content_type': content_type,
            'size': size,
            'options': {
                'chunked': True,
                'resumable': True
            }
        }
        
        response = requests.post(
            f'{self.base_url}/api/v3/upload',
            headers=headers,
            json=data
        )
        
        return response.json()
```

---

## Quick Reference

### Essential Commands
```bash
# Generate minimal config
./hmac-file-server -genconfig > config.toml

# Start with config validation
./hmac-file-server -validate-config -config config.toml

# Start server
./hmac-file-server -config config.toml

# Build for multiple architectures
./build-multi-arch.sh

# Podman deployment (tested and verified ✅)
cd dockerenv/podman && ./deploy-podman.sh
```

### Minimal Production Config
```toml
[server]
listen_address = "8080"
storage_path = "/opt/hmac-file-server/uploads"

[security]
secret = "your-production-secret-here"

[logging]
level = "info"
file = "/var/log/hmac-file-server.log"
```

### Health Check
```bash
curl -f http://localhost:8080/health
```

### Metrics
```bash
curl http://localhost:9090/metrics
```

---

## Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Commit changes: `git commit -m 'Add amazing feature'`
4. Push to branch: `git push origin feature/amazing-feature`
5. Open a Pull Request

---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## Links

- **GitHub**: https://github.com/PlusOne/hmac-file-server
- **Documentation**: https://hmac-file-server.readthedocs.io
- **Issues**: https://github.com/PlusOne/hmac-file-server/issues
- **Releases**: https://github.com/PlusOne/hmac-file-server/releases

---

*HMAC File Server 3.2 "Tremora del Terra" - Where enterprise power meets user simplicity*
