# HMAC File Server 3.3.0 - Nexus Infinitum

[![Version](https://img.shields.io/badge/version-3.3.0-blue.svg)](https://git.uuxo.net/uuxo/hmac-file-server/)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Go Version](https://img.shields.io/badge/go-1.21+-00ADD8.svg)](https://golang.org/)
[![Architecture](https://img.shields.io/badge/arch-AMD64%20%7C%20ARM64%20%7C%20ARM32v7-brightgreen.svg)](https://git.uuxo.net/uuxo/hmac-file-server/)

A high-performance, secure file server implementing XEP-0363 (HTTP File Upload) with HMAC authentication, deduplication, and multi-architecture support.

---

## What's New in 3.3.0 "Nexus Infinitum"

### XMPP Client Compatibility
- Full CORS support resolves intermittent upload errors
- Tested with Dino, Gajim, Conversations, Monal
- CORS headers for Converse.js and browser-based clients
- W3C CORS + XEP-0363 HTTP File Upload standards compliant

### Configuration Revolution
- 93% reduction in required configuration (from 112-line to 8-line configs)
- Production-ready settings built into the application
- Automatic generation of minimal configs with `--genconfig`
- Backward compatibility with existing configurations

### Enhanced Performance
- Fixed deduplication returning success instead of "file not found" for existing files
- Doubled queue capacity (50 to 100) with faster scaling (80% to 40% threshold)
- Extended timeouts of 4800s for large file reliability
- 60-minute session persistence for enterprise transfers

### Multi-Architecture Support
- Native performance across AMD64, ARM64, ARM32v7 platforms
- Interactive build script with architecture selection menu
- Enterprise-grade support for all platforms

### Container Support
- Full support for both Docker and Podman container engines
- Podman deployment tested and verified
- Security hardened with rootless operation and SELinux integration
- Optimized pod networking for multi-service XMPP deployments

---

## Installation

### Download Binary

**From Gitea (Primary):**
```bash
wget https://git.uuxo.net/UUXO/hmac-file-server/releases/download/v3.3.0/hmac-file-server-linux-amd64
chmod +x hmac-file-server-linux-amd64
sudo mv hmac-file-server-linux-amd64 /usr/local/bin/hmac-file-server
```

**From GitHub (Mirror):**
```bash
wget https://github.com/PlusOne/hmac-file-server/releases/download/v3.3.0/hmac-file-server-linux-amd64
chmod +x hmac-file-server-linux-amd64
sudo mv hmac-file-server-linux-amd64 /usr/local/bin/hmac-file-server
```

### Build from Source

**Clone from Gitea:**
```bash
git clone https://git.uuxo.net/UUXO/hmac-file-server.git
cd hmac-file-server
go build -o hmac-file-server ./cmd/server/
```

**Clone from GitHub:**
```bash
git clone https://github.com/PlusOne/hmac-file-server.git
cd hmac-file-server
go build -o hmac-file-server ./cmd/server/
```

---

## Quick Start

### Option 1: Minimal Configuration (Recommended)
```bash
# Generate minimal config
./hmac-file-server -genconfig > config.toml

# Edit 3 essential settings:
# - listen_address = "8080"
# - storage_path = "/your/storage/path"
# - secret = "your-secure-secret"

# Start server
./hmac-file-server -config config.toml
```

### Option 2: Zero-Config Startup
```bash
# Auto-creates minimal config
./hmac-file-server
# Follow prompts to customize settings
```

---

## Universal Installation Manager

HMAC File Server 3.3.0 includes a comprehensive installation framework that supports all deployment methods:

### Automated Installation (All Methods)
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

### Supported Deployment Methods
- SystemD: Native installation with service integration
- Docker: Full containerized deployment with compose files
- Podman: Rootless container deployment (tested and verified)
- Debian: Package-based installation with dependency management
- Multi-Architecture: AMD64, ARM64, ARM32v7 support for all methods

### Comprehensive Testing Suite
```bash
# Run all functionality tests
./test

# Quick validation test
./quick-test

# Test specific components
./test setup    # Setup test files only
./test clean    # Clean up test files
```

### Enhanced Container Build Script
```bash
# Universal container builder - auto-detects Docker and Podman
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

Features:
- Auto-Detection: Automatically finds available container engines (Docker/Podman)
- Engine Selection: Interactive menu for multiple engines or force specific engine
- Compose Support: Uses appropriate compose files (docker-compose.yml / podman-compose.yml)
- Podman Optimized: SELinux labels, rootless support, security optimizations
- Build and Deploy: Combined build and optional service startup in one command

Test Coverage:
- HMAC Authentication and File Upload Validation
- XMPP Integration (MP4 uploads for Conversations/Gajim)
- Network Resilience and Mobile Switching Features
- Large File Support and Extension Validation
- Security Testing (Invalid HMAC rejection)

---

## Table of Contents

- [Release Information](#release-information)
- [Configuration Generation](#configuration-generation)
- [Configuration Documentation](#configuration-documentation)
- [Build Options](#build-options)
- [Docker Compose Examples](#docker-compose-examples)
- [Podman Deployment](#podman-deployment)
- [Nginx Reverse Proxy](#nginx-reverse-proxy)
- [Apache2 Reverse Proxy](#apache2-reverse-proxy)
- [XMPP Server Integration](#xmpp-server-integration)
- [XEP-0363 Implementation](#xep-0363-implementation)
- [API Versions (V1, V2, V3)](#api-versions)

---

## Release Information

### HMAC File Server 3.3.0 - Nexus Infinitum

Release Date: August 26, 2025
Codename: Nexus Infinitum (infinite connectivity and boundless network reach)

#### Latest Updates (3.3.0)
- Enhanced MIME Types: Added 80+ additional file format support
- XMPP Client Ecosystem: Comprehensive compatibility analysis
- Network Resilience: Advanced mobile switching optimizations
- Documentation: Complete client compatibility matrix
- Security: HMAC core functions remain untouched and secure

#### Key Improvements
- Configuration Simplification: 93% reduction in required configuration
- Enhanced Deduplication: Fixed "file not found" errors for existing files
- Performance Optimization: Doubled queue capacity, optimized worker scaling
- Multi-Architecture Support: Native builds for AMD64, ARM64, ARM32v7
- Developer Experience: Minimal config-first approach with comprehensive defaults

#### Network Switching Enhancements (3.3.0)
- Session Persistence: Advanced session-based authentication for 5G/WiFi switching
- XEP-0363 Enhancement: Bearer token refresh mechanism with up to 10 refreshes
- Network Change Detection: Real-time network transition logging and handling
- Upload Resumption: Interrupted transfer recovery across network changes

#### Migration Notes
- Backward Compatible: All existing 3.2.x configs work unchanged
- Performance Boost: Automatic optimizations with existing configurations
- Optional Migration: Users can optionally migrate to simplified 8-line configs

#### System Requirements
- Memory: 512MB minimum, 2GB+ recommended for large files
- Storage: 100MB application + user data storage
- Network: Standard TCP/IP connectivity
- OS: Linux (primary), Windows/macOS (experimental)

---

## Mobile Network Resilience

HMAC File Server 3.3.0 includes comprehensive network resilience features designed to handle mobile network switching, device standby scenarios, and IP address changes during file uploads.

### Key Features

**Problem Solved:** WiFi ↔ LTE switching with 404 errors, device standby authentication loss, and IP address changes during uploads.

**Built-in Solutions (Works Automatically):**
- **Ultra-Flexible Grace Periods:** 8h base, 12h mobile, 72h ultra-maximum for device standby
- **Mobile Client Detection:** Automatic recognition of Conversations, Dino, Gajim, and Android XMPP clients
- **IP Change Handling:** Seamless transition via X-Forwarded-For and X-Real-IP headers
- **Session Persistence:** Upload continuation across network changes and IP switches
- **Bearer Token Validation:** 5 different HMAC payload formats for maximum compatibility

### Real-World Scenarios Handled

1. **WiFi → LTE Switch:** Authentication persists, uploads continue seamlessly
2. **Device Standby:** 72-hour grace period keeps authentication valid after sleep
3. **Carrier IP Change:** Proxy header detection handles mobile carrier IP reassignment
4. **Network Interruption:** Upload resumption and session recovery across network changes

### Configuration

Network resilience features are enabled by default with sensible settings. For advanced tuning, see configuration examples in `examples/config-mobile-resilient.toml` and `examples/config-network-switching.toml`.

---

## Configuration Generation

### Generate Minimal Configuration
```bash
# Create minimal 8-line config (recommended for most users)
./hmac-file-server -genconfig > config.toml
```

Output Example:
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

### Configuration Troubleshooting

Common Issue: Service fails with `storage path is required` or `permission denied`

```bash
# WRONG - Field names without underscores
[server]
storagepath = "/opt/hmac-file-server/data/uploads"
listenport = "8080"

# CORRECT - Use underscores in TOML field names
[server]
storage_path = "/opt/hmac-file-server/data/uploads"
listen_address = "8080"
```

### XMPP Client Issues

Gajim "Bad Gateway" Error: Fixed in 3.3.0 with full CORS support
```bash
# Verify CORS functionality
curl -X OPTIONS http://your-server:8080/ -v
# Should return HTTP 200 with Access-Control headers
```

See [GAJIM_BAD_GATEWAY_FIX.md](GAJIM_BAD_GATEWAY_FIX.md) for complete details

Quick Fix Commands:
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

# Enhanced Network Resilience (v3.3.0+)
[network_resilience]
enabled = true                              # Enable network resilience system
fast_detection = true                       # Enable 1-second network change detection (vs 5-second default)
quality_monitoring = true                   # Monitor RTT and packet loss per interface
predictive_switching = true                 # Switch proactively before network failure
mobile_optimizations = true                # Use mobile-friendly thresholds for cellular networks
upload_resilience = true                    # Resume uploads across network changes
detection_interval = "1s"                  # Network change detection interval
quality_check_interval = "5s"              # Connection quality monitoring interval
max_detection_interval = "10s"             # Maximum detection interval during stable periods
network_change_threshold = 3               # Switches required to trigger network change
interface_stability_time = "30s"           # Time to wait before marking interface stable
upload_pause_timeout = "5m"               # Maximum time to pause uploads during network changes
upload_retry_timeout = "10m"              # Maximum time to retry uploads after network changes
rtt_warning_threshold = "200ms"            # RTT threshold for warning
rtt_critical_threshold = "1000ms"          # RTT threshold for critical
packet_loss_warning_threshold = 2.0        # Packet loss % for warning
packet_loss_critical_threshold = 10.0      # Packet loss % for critical

# Multi-Interface Support (v3.3.0+)
multi_interface_enabled = false            # Enable multi-interface management
interface_priority = ["eth0", "wlan0", "wwan0", "ppp0"]  # Interface priority order
auto_switch_enabled = true                 # Enable automatic interface switching
switch_threshold_latency = "500ms"         # Latency threshold for switching
switch_threshold_packet_loss = 5.0         # Packet loss threshold for switching
quality_degradation_threshold = 0.5        # Quality degradation threshold
max_switch_attempts = 3                    # Maximum switch attempts per detection
switch_detection_interval = "10s"          # Switch detection interval

# Client Network Support (v3.3.0+)
[client_network_support]
session_based_tracking = false             # Track sessions by ID instead of IP
allow_ip_changes = true                    # Allow session continuation from different IPs
session_migration_timeout = "5m"           # Time to wait for client reconnection
max_ip_changes_per_session = 10           # Maximum IP changes per session
client_connection_detection = false        # Detect client network type
adapt_to_client_network = false           # Optimize parameters based on client connection

[uploads]
# File upload configuration
allowed_extensions = [".zip", ".rar", ".jpg", ".jpeg", ".png", ".gif", ".webp", ".pdf", ".txt", ".mp4", ".mov", ".ogg", ".mp3", ".doc", ".docx"]      # Permitted upload file extensions (XMPP-compatible)
chunked_uploads_enabled = true             # Enable chunked/resumable uploads
chunk_size = "10MB"                        # Upload chunk size
resumable_uploads_enabled = true           # Enable upload resumption
max_resumable_age = "48h"                  # How long to keep resumable uploads
sessiontimeout = "60m"                     # Upload session timeout
maxretries = 3                             # Maximum upload retry attempts
networkevents = false                      # Enable network event monitoring for uploads

# Upload resilience and session management
session_persistence = true                 # Persist sessions across restarts
session_recovery_timeout = "300s"          # Session recovery timeout after network changes
client_reconnect_window = "120s"           # Time window for client reconnection
upload_slot_ttl = "3600s"                  # Upload slot validity time
retry_failed_uploads = true                # Auto-retry failed uploads
max_upload_retries = 3                     # Maximum retry attempts
allow_session_resume = true                # Allow resume from different IPs
session_persistence_duration = "24h"       # How long to keep session data
detect_duplicate_uploads = true            # Detect same upload from different IPs
merge_duplicate_sessions = true            # Merge sessions from same client

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
uploadqueuesize = 100                     # Upload queue size (doubled in 3.3.0)

[build]
# Build information
version = "3.3.0"                          # Application version
```

---

## Build Options

### Interactive Build Script
```bash
# Use interactive build menu
./build-multi-arch.sh
```

Menu Options:
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
docker build -t hmac-file-server:3.3.0 .

# Multi-platform Docker build
docker buildx build --platform linux/amd64,linux/arm64,linux/arm/v7 -t hmac-file-server:3.3.0 .
```

---

## Docker Compose Examples

### Basic Docker Compose
```yaml
# docker-compose.yml
version: '3.8'

services:
  hmac-file-server:
    image: hmac-file-server:3.3.0
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
    image: hmac-file-server:3.3.0
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
    image: hmac-file-server:3.3.0
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
git clone https://git.uuxo.net/uuxo/hmac-file-server.git
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
RUN git clone https://git.uuxo.net/uuxo/hmac-file-server.git .
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

    echo "IMPORTANT: Edit ${app_data}/config/config.toml and change the secrets!"
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

echo "HMAC File Server deployed successfully!"
echo "Server available at: http://localhost:${listen_port}"
echo "Metrics available at: http://localhost:${metrics_port}/metrics"
echo "Container status: podman ps"
echo "View logs: podman logs ${ctr_name}"
echo "Health check: curl -f http://localhost:${listen_port}/health"
```

#### Podman Systemd Service (Rootless)
```ini
# ~/.config/systemd/user/hmac-file-server.service
[Unit]
Description=HMAC File Server (Podman)
Documentation=https://git.uuxo.net/uuxo/hmac-file-server/
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
| Daemon | Requires Docker daemon | Daemonless architecture |
| Root Access | Requires root for Docker daemon | Can run completely rootless |
| Security | Good, but daemon runs as root | Enhanced security, no privileged daemon |
| Systemd Integration | Via Docker service | Native systemd integration |
| Pod Support | Requires docker-compose or swarm | Native Kubernetes-style pods |
| Image Compatibility | Docker images | Compatible with Docker images |
| Enterprise Use | Popular in startups/mid-size | Preferred in enterprise environments |
| SELinux | Basic support | Excellent SELinux integration |

### Podman Benefits for HMAC File Server

1. Enhanced Security: No privileged daemon, better isolation
2. Rootless Operation: Can run without root privileges
3. SELinux Integration: Better compliance in enterprise environments
4. Systemd Native: Better integration with system services
5. Pod Support: Natural clustering with XMPP servers
6. Resource Efficiency: Lower overhead without daemon

### Testing Results and Verification

The Podman deployment has been fully tested and verified:

#### Installation Success
- Docker Removal: Complete removal of Docker packages and dependencies
- Podman Installation: Podman 4.3.1 installed with all dependencies (fuse-overlayfs, slirp4netns, uidmap)
- Image Build: Successfully built localhost/hmac-file-server:latest with security optimizations

#### Container Deployment Success
- Security Hardened: Running as non-root user (UID 1011) with --cap-drop=ALL, --read-only, --no-new-privileges
- Health Checks: Built-in health monitoring and status reporting
- Volume Mounting: Proper SELinux labeling with :Z flags

#### Functional Verification
```bash
# Health endpoint test
curl -f http://localhost:8888/health
# Response: OK

# Metrics endpoint test
curl -s http://localhost:9090/metrics | head -5
# Response: Prometheus metrics

# Container status
podman ps
# Status: Up and running

# Configuration validation
podman logs hmac-file-server
# Result: All settings validated
```

#### Production Ready Features
- XMPP Integration: Pod networking for multi-service XMPP deployments
- Configuration Management: Auto-generated secure configs with random secrets
- Service Management: Native systemd integration for both rootless and system-wide deployment
- Enterprise Security: Enhanced security features preferred in enterprise environments

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
curl -f http://localhost:8888/health && echo " - Server is healthy!"
```

### Manual Setup: Paths, Ownership and Permissions

When setting up Podman or Docker manually, proper path ownership is crucial:

#### Container User Configuration
- Container User: appuser (UID: 1011, GID: 1011)
- Security: Non-root user for enhanced security
- Compatibility: Works with both rootless and rootful containers

#### Required Directory Structure
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

#### Path Mapping Reference

| Host Path | Container Path | Purpose | Required Permissions |
|-----------|----------------|---------|---------------------|
| /opt/podman/hmac-file-server/config/config.toml | /app/config.toml | Configuration file | 644 (read-only) |
| /opt/podman/hmac-file-server/data/ | /data/ | File uploads/storage | 755 (read-write) |
| /opt/podman/hmac-file-server/deduplication/ | /deduplication/ | Deduplication cache | 755 (read-write) |
| /opt/podman/hmac-file-server/logs/ | /logs/ | Application logs | 755 (read-write) |

#### SELinux Labels (Important for RHEL/CentOS/Fedora)
```bash
# Add SELinux labels for Podman volume mounts
podman run -d --name hmac-file-server \
  -v /opt/podman/hmac-file-server/config/config.toml:/app/config.toml:ro,Z \
  -v /opt/podman/hmac-file-server/data:/data:rw,Z \
  -v /opt/podman/hmac-file-server/deduplication:/deduplication:rw,Z \
  -v /opt/podman/hmac-file-server/logs:/logs:rw,Z \
  localhost/hmac-file-server:latest

# Note: The :Z flag relabels content and should be used for private volumes
# Use :z for shared volumes between multiple containers
```

#### Common Ownership Issues and Solutions

Problem: Container fails with permission errors
```bash
# Logs show: "permission denied: open /data/.write_test"
```

Solution: Fix ownership and verify
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

Problem: SELinux blocking container access
```bash
# Logs show: "SELinux is preventing access"
```

Solution: Correct SELinux labeling
```bash
# Option 1: Use :Z labels in volume mounts (recommended)
-v /path/to/data:/data:rw,Z

# Option 2: Set SELinux context manually
sudo setsebool -P container_manage_cgroup on
sudo restorecon -R /opt/podman/hmac-file-server
```

#### Docker vs Podman Ownership Differences

| Scenario | Docker | Podman Rootless | Podman Rootful |
|----------|--------|-----------------|----------------|
| Host UID | 1011:1011 | 1011:1011 | 1011:1011 |
| Container UID | 1011:1011 | 1011:1011 | 1011:1011 |
| Volume Ownership | chown 1011:1011 | podman unshare chown 1011:1011 | chown 1011:1011 |
| SELinux Labels | :Z or :z | :Z or :z | :Z or :z |

#### Verification Commands
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

## XMPP Server Integration

HMAC File Server implements the standard **XEP-0363: HTTP File Upload** protocol and works out-of-the-box with all XMPP servers including Prosody, Ejabberd, Openfire, and others.

### Quick Setup

Configure your XMPP server's `http_upload_external` module to point to your HMAC File Server:

```yaml
# Standard XEP-0363 configuration (works with any XMPP server)
http_upload_external:
  base_url: "https://files.example.com"
  secret: "your-shared-hmac-secret"
  max_file_size: 10737418240  # 10GB
```

The server handles all the HMAC authentication and file management automatically. No custom modules required.

### Experimental Custom Modules

Advanced users can explore experimental Prosody and Ejabberd modules in the `ejabberd-module/` directory. These are **development stage** and require additional testing. The standard XEP-0363 implementation is recommended for production use.

---

## XEP-0363 Implementation

### XEP-0363: HTTP File Upload

HMAC File Server implements [XEP-0363: HTTP File Upload](https://xmpp.org/extensions/xep-0363.html) with HMAC authentication for secure file sharing in XMPP environments.

#### Protocol Flow

1. Discovery: Client discovers upload service
2. Request Slot: Client requests upload/download URLs
3. Upload: Client uploads file to provided PUT URL
4. Share: Client shares GET URL with contacts
5. Download: Recipients download using GET URL

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

Endpoint: `/api/v1/upload`
Authentication: Basic HMAC
Usage: Legacy XMPP clients, basic integrations

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

Endpoint: `/api/v2/upload`
Authentication: Enhanced HMAC with timestamps
Usage: Modern XMPP clients, advanced features

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

Endpoint: `/api/v3/upload`
Authentication: JWT or Enhanced HMAC
Usage: Custom clients, experimental features
Note: V3 is not a defined standard - custom implementation

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

# Podman deployment (tested and verified)
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

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.

---

## Repository

Primary Repository (Gitea): [git.uuxo.net/UUXO/hmac-file-server](https://git.uuxo.net/UUXO/hmac-file-server)
GitHub Mirror: [github.com/PlusOne/hmac-file-server](https://github.com/PlusOne/hmac-file-server)

Issues and contributions welcome on both platforms.

---

## Links

- Primary Repository (Gitea): https://git.uuxo.net/UUXO/hmac-file-server
- GitHub Mirror: https://github.com/PlusOne/hmac-file-server
- Documentation: https://git.uuxo.net/UUXO/hmac-file-server/wiki
- Issues (Gitea): https://git.uuxo.net/UUXO/hmac-file-server/issues
- Issues (GitHub): https://github.com/PlusOne/hmac-file-server/issues
- Releases (Gitea): https://git.uuxo.net/UUXO/hmac-file-server/releases
- Releases (GitHub): https://github.com/PlusOne/hmac-file-server/releases
- Website: https://uuxo.net

HMAC File Server 3.3 "Nexus Infinitum" - Where enterprise power meets user simplicity

---
