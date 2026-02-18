This documentation provides detailed information on configuring, setting up, and maintaining the HMAC File Server 3.4.0 "Cascade". Whether you're a developer, system administrator, or an enthusiast, this guide will help you navigate through the server's features and configurations effectively.

---

## Table of Contents

1. [Introduction](#introduction)
2. [3.3.0 "Nexus Infinitum" & 3.4.0 "Cascade" Features](#330-nexus-infinitum--340-cascade-features)
3. [Configuration](#configuration)
    - [Server Configuration](#server-configuration)
    - [Deduplication Settings](#deduplication-settings)
    - [ISO Settings](#iso-settings)
    - [Timeout Settings](#timeout-settings)
    - [Security Settings](#security-settings)
    - [Versioning Settings](#versioning-settings)
    - [Uploads Settings](#uploads-settings)
    - [Downloads Settings](#downloads-settings)
    - [ClamAV Settings](#clamav-settings)
    - [Redis Settings](#redis-settings)
    - [Worker Settings](#worker-settings)
    - [Network Resilience Settings](#network-resilience-settings)
    - [Client Network Support Settings](#client-network-support-settings)
    - [Rate Limiting Configuration](#rate-limiting-configuration)
    - [HMAC Key Rotation Configuration](#hmac-key-rotation-configuration)
    - [Metadata Store Configuration](#metadata-store-configuration)
    - [Admin Dashboard](#admin-dashboard)
    - [SIMS Thumbnails (XEP-0385)](#sims-thumbnails-xep-0385)
4. [Example Configuration](#example-configuration)
5. [Setup Instructions](#setup-instructions)
    - [1. HMAC File Server Installation](#1-hmac-file-server-installation)
    - [2. Reverse Proxy Configuration](#2-reverse-proxy-configuration)
        - [Apache2 Reverse Proxy](#apache2-reverse-proxy)
        - [Nginx Reverse Proxy](#nginx-reverse-proxy)
    - [3. ejabberd Configuration](#3-ejabberd-configuration)
    - [4. Systemd Service Setup](#4-systemd-service-setup)
6. [Running with Docker & Docker Compose](#running-with-docker--docker-compose)
7. [Running with Podman](#running-with-podman)
8. [Multi-Architecture Build System](#multi-architecture-build-system)
9. [Network Resilience & Queue Optimization](#network-resilience--queue-optimization)
10. [Multi-Architecture Deployment](#multi-architecture-deployment)
11. [Command-Line Tools & Utilities](#command-line-tools--utilities)
12. [Development & Build Tools](#development--build-tools)
13. [Additional Recommendations](#additional-recommendations)
14. [XMPP Client Large File Upload (Gajim 1GB+ Multi-Upload Fix)](#xmpp-client-large-file-upload-gajim-1gb-multi-upload-fix)
15. [Notes](#notes)
16. [Using HMAC File Server for CI/CD Build Artifacts](#using-hmac-file-server-for-ci-cd-build-artifacts)
17. [Monitoring](#monitoring)

---

## Introduction

The **HMAC File Server 3.4.0 "Cascade"** is a secure and high-performance file management solution designed for XMPP ecosystems with enterprise-grade operational features. Building on the "Nexus Infinitum" foundation, this release adds **operational hardening**, **automated security**, and a **web-based admin dashboard**.

**Version 3.4.0 "Cascade" New Features:**
- **Rate Limiting**: Token bucket per JID/IP with configurable burst, whitelists, and 429 responses
- **HMAC Key Rotation**: Automatic secret rotation with grace period and JSON key persistence
- **Admin Dashboard**: HTMX + Tailwind CSS dark web UI with live stats, file browser, and user quotas
- **SQLite Metadata Store**: Persistent tracking of uploads/downloads with content types and download counts
- **SIMS Thumbnails (XEP-0385)**: Automatic 320×240 JPEG thumbnail generation for image uploads
- **Automatic File Cleanup**: TTL-based file expiry with empty directory pruning

**Continuing from v3.3.0 "Nexus Infinitum":**
- **Network Resilience**: WiFi ↔ LTE switching with zero interruption  
- **Mobile Client Optimization**: 72-hour ultra-grace periods for critical scenarios
- **Multi-Architecture Support**: Native AMD64, ARM64, ARM32v7 builds
- **XEP-0363 XMPP Integration**: Full XMPP file sharing protocol support
- **Prometheus Monitoring**: Enterprise-grade metrics and observability

Built with a focus on security, scalability, and performance, it integrates seamlessly with various tools and services to provide a comprehensive file handling experience optimized for modern cloud environments.

---

## 3.3.0 "Nexus Infinitum" & 3.4.0 "Cascade" Features

HMAC File Server 3.4.0 "Cascade" builds on the "Nexus Infinitum" foundation, adding operational hardening and admin tooling. Below are features across both releases:

### 🚀 **93% Configuration Reduction**
- **Simplified Setup**: Reduced configuration complexity by 93% through intelligent defaults
- **Minimal Config Required**: Essential settings only - server runs with just a few lines
- **Smart Defaults**: Automatically optimized settings for most use cases
- **Zero-Touch Deployment**: Ready for production with minimal configuration

### 🌐 **Network Resilience System**
- **Connection Recovery**: Automatic reconnection and retry mechanisms
- **Timeout Optimization**: Extended 4800s timeouts for seamless large file transfers
- **Network Switching**: Handles network changes gracefully without service interruption
- **Connection Pooling**: Intelligent connection management for high-load scenarios

### ⚡ **Queue Optimization Engine**
- **Dynamic Worker Scaling**: Optimized 40%/10% thresholds for perfect load balancing
- **Queue Intelligence**: Smart queue management preventing bottlenecks
- **Load Prediction**: Proactive scaling based on traffic patterns
- **Memory Optimization**: Reduced memory footprint while handling larger queues

### 🏗️ **Multi-Architecture Excellence**
- **Native AMD64**: Optimized performance for Intel/AMD processors
- **ARM64 Support**: Full native support for Apple Silicon and ARM servers
- **ARM32v7 Compatibility**: Raspberry Pi and IoT device support
- **Cross-Platform**: Consistent behavior across all architectures

### 📊 **Enterprise Monitoring**
- **Prometheus Integration**: Comprehensive metrics collection
- **Real-time Dashboards**: Advanced monitoring capabilities
- **Performance Analytics**: Detailed insights into server operations
- **Alert Systems**: Proactive issue detection and notification

### 🔗 **XEP-0363 XMPP Integration**
- **Full Protocol Support**: Complete XMPP file sharing implementation
- **ejabberd Integration**: Seamless integration with XMPP servers
- **Secure File Sharing**: HMAC-authenticated file sharing through XMPP
- **Standard Compliance**: Full XEP-0363 protocol compliance

---

## Configuration

The HMAC File Server is configured using a `config.toml` file. Below are the detailed explanations of each configuration section and their respective options.

### Server Configuration

```toml
# Server configuration
listen_address = ":8080"  # Listen address and port for incoming requests
storage_path = "/srv/hmac-file-server/uploads"  # Directory to store uploaded files
metrics_enabled = true   # Enable Prometheus metrics
metrics_path = "/metrics"    # Path for Prometheus metrics endpoint
pid_file = "/var/run/hmac-file-server.pid" # Path to PID file
max_upload_size = "10GB" # Maximum file upload size
max_header_bytes = 1048576 # Maximum header size (1MB)
cleanup_interval = "24h" # Interval for cleanup operations
max_file_age = "720h"    # Maximum age for files (30 days)
pre_cache = true         # Pre-cache file structures on startup
pre_cache_workers = 4    # Number of workers for pre-caching
pre_cache_interval = "1h" # Interval for pre-cache operations
global_extensions = [".txt", ".dat", ".iso", ".mp4", ".mkv", ".avi", ".mov"] # Global allowed extensions
deduplication_enabled = true # Enable file deduplication
min_free_bytes = "1GB"   # Minimum free disk space required
file_naming = "original" # File naming strategy: "original", "HMAC"
force_protocol = ""      # Force protocol: "http", "https" or empty for auto
enable_dynamic_workers = true # Enable dynamic worker scaling
worker_scale_up_thresh = 40   # Queue length % to scale up workers (40% optimized threshold)
worker_scale_down_thresh = 10 # Queue length % to scale down workers (10% stability threshold)
```

#### Configuration Options

- **listen_address**:  
  - *Type*: `String`  
  - *Description*: Specifies the address and port on which the server listens for incoming requests.  
  - *Default*: `":8080"`
  
- **storage_path**:  
  - *Type*: `String`  
  - *Description*: Defines the directory path where uploaded files are stored. Ensure this path exists and has appropriate permissions.  
  - *Default*: `"/srv/hmac-file-server/uploads"`
  
- **metrics_enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables Prometheus metrics collection.  
  - *Default*: `true`
  
- **max_upload_size**:  
  - *Type*: `String`  
  - *Description*: Maximum allowed file upload size. Supports B, KB, MB, GB, TB units.  
  - *Default*: `"10GB"`
  
- **deduplication_enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables file deduplication to save storage space by using hard links for identical files.  
  - *Default*: `true`
  
- **enable_dynamic_workers**:  
  - *Type*: `Boolean`  
  - *Description*: Enables automatic scaling of worker threads based on upload queue length.  
  - *Default*: `true`
  
- **file_naming**:  
  - *Type*: `String`  
  - *Description*: Strategy for naming uploaded files.  
  - *Options*: `"original"` (preserve original names), `"HMAC"` (use HMAC-based names)  
  - *Default*: `"original"`  
  - *Description*: Enables or disables the Prometheus metrics endpoint.  
  - *Default*: `true`
  
- **metricsport**:  
  - *Type*: `String`  
  - *Description*: Defines the port on which Prometheus metrics are exposed.  
  - *Default*: `"9090"`
  
- **deduplicationenabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables file deduplication to optimize storage usage.  
  - *Default*: `true`

---

### Deduplication Settings

```toml
# Deduplication settings
[deduplication]
enabled = true
directory = "/opt/hmac-file-server/data/dedup"  # Path to deduplication storage
maxsize = "1GB"  # Maximum file size for deduplication
```

#### Configuration Options

- **enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables the deduplication feature, which uses hard links to eliminate duplicate files and save storage space.  
  - *Default*: `true`
  
- **directory**:  
  - *Type*: `String`  
  - *Description*: Specifies the directory path where deduplicated files are stored. Files are organized by their SHA256 hash.  
  - *Default*: `"/opt/hmac-file-server/data/dedup"`
  
- **maxsize**:  
  - *Type*: `String`  
  - *Description*: Maximum file size eligible for deduplication. Larger files are not deduplicated to avoid performance impact.  
  - *Default*: `"1GB"`

---

### ISO Settings

```toml
# ISO settings
[iso]
enabled = false
size = "1TB"  # Maximum ISO size
mountpoint = "/path/to/hmac-file-server/iso/"  # ISO mount point
charset = "utf-8"  # Filesystem character set encoding
```

#### Configuration Options

- **enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables the mounting of an ISO-based filesystem for specialized storage needs.  
  - *Default*: `false`
  
- **size**:  
  - *Type*: `String`  
  - *Description*: Defines the maximum allowed size for the ISO container.  
  - *Default*: `"1TB"`
  
- **mountpoint**:  
  - *Type*: `String`  
  - *Description*: Specifies the directory path where the ISO is mounted. Ensure this path exists and has appropriate permissions.  
  - *Default*: `"/path/to/hmac-file-server/iso/"`
  
- **charset**:  
  - *Type*: `String`  
  - *Description*: Sets the filesystem character set encoding for the ISO.  
  - *Default*: `"utf-8"`

> **Note**: Ensure only one `[iso]` block is active in your `config.toml` to avoid configuration conflicts.

---

### Timeout Settings

```toml
# Timeout settings
[timeouts]
readtimeout = "4800s"    # Maximum time to read a request (80 minutes for large files)
writetimeout = "4800s"   # Maximum time to write a response (80 minutes for large files)
idletimeout = "4800s"    # Maximum keep-alive time for idle connections
```

#### Configuration Options

- **readtimeout**:  
  - *Type*: `String`  
  - *Description*: Sets the maximum duration for reading the entire request, including the body. Extended timeout for large file uploads.  
  - *Format*: Duration (e.g., `"4800s"` for 80 minutes)  
  - *Default*: `"4800s"`
  
- **writetimeout**:  
  - *Type*: `String`  
  - *Description*: Defines the maximum duration before timing out writes of the response. Extended timeout for large file downloads.  
  - *Format*: Duration (e.g., `"4800s"` for 80 minutes)  
  - *Default*: `"4800s"`
  
- **idletimeout**:  
  - *Type*: `String`  
  - *Description*: Specifies the maximum amount of time to wait for the next request when keep-alives are enabled.  
  - *Format*: Duration (e.g., `"4800s"` for 80 minutes)  
  - *Default*: `"4800s"`

**Note**: These extended timeout values are specifically configured for handling large file uploads and downloads (GB-sized files). Ensure your reverse proxy configuration has matching timeout values.

---

### Security Configuration

```toml
# Security settings
[security]
secret = "your-secure-secret-key"  # HMAC shared secret key (change to a secure value)
enablejwt = false                  # Enable JWT authentication
jwtsecret = "your-jwt-secret"      # JWT signing secret
jwtalgorithm = "HS256"             # JWT algorithm
jwtexpiration = "24h"              # JWT token expiration
```

#### Configuration Options

- **secret**:  
  - *Type*: `String`  
  - *Description*: The HMAC shared secret key used for signing requests and operations.  
  - *Default*: `"your-secure-secret-key"`  
  - *Warning*: **Change this immediately** to a unique, strong string in production environments to ensure the security of HMAC operations.

- **enablejwt**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables JWT token authentication. When enabled, the server will accept JWT tokens for authentication.  
  - *Default*: `false`

- **jwtsecret**:  
  - *Type*: `String`  
  - *Description*: The secret key used for signing and validating JWT tokens. Must be strong and secure.  
  - *Default*: `"your-jwt-secret"`

- **jwtalgorithm**:  
  - *Type*: `String`  
  - *Description*: The algorithm used for JWT token signing.  
  - *Options*: `"HS256"`, `"HS384"`, `"HS512"`  
  - *Default*: `"HS256"`

- **jwtexpiration**:  
  - *Type*: `String`  
  - *Description*: The expiration time for JWT tokens.  
  - *Format*: Duration (e.g., `"24h"` for 24 hours, `"30m"` for 30 minutes)  
  - *Default*: `"24h"`

---

### Versioning Settings

```toml
# Versioning settings
[versioning]
enableversioning = false
maxversions = 1  # Number of file versions to retain
```

#### Configuration Options

- **enableversioning**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables the versioning feature, which maintains multiple versions of the same file.  
  - *Default*: `false`
  
- **maxversions**:  
  - *Type*: `Integer`  
  - *Description*: Specifies the maximum number of versions to retain for each file.  
  - *Default*: `1`

---

### Logging Configuration

```toml
# Logging settings
[logging]
level = "info"
file = "/var/log/hmac-file-server.log"
max_size = 100    # Maximum log file size in MB
max_backups = 7   # Number of backup log files to keep
max_age = 30      # Maximum age of log files in days
compress = true   # Compress old log files
```

#### Configuration Options

- **level**:  
  - *Type*: `String`  
  - *Description*: Sets the verbosity level of logs.  
  - *Options*: `"debug"`, `"info"`, `"warn"`, `"error"`  
  - *Default*: `"info"`

- **file**:  
  - *Type*: `String`  
  - *Description*: Specifies the file path for logging. If left empty, logs are output to `stdout`.  
  - *Default*: `"/var/log/hmac-file-server.log"`

- **max_size**:  
  - *Type*: `Integer`  
  - *Description*: Maximum size of log files before rotation (in MB).  
  - *Default*: `100`

- **max_backups**:  
  - *Type*: `Integer`  
  - *Description*: Number of backup log files to retain after rotation.  
  - *Default*: `7`

- **max_age**:  
  - *Type*: `Integer`  
  - *Description*: Maximum age of log files in days before deletion.  
  - *Default*: `30`

- **compress**:  
  - *Type*: `Boolean`  
  - *Description*: Whether to compress old log files with gzip.  
  - *Default*: `true`

---

### Uploads Configuration

```toml
# Upload settings
[uploads]
allowed_extensions = [".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp", ".txt", ".pdf", ".jpg", ".jpeg", ".png", ".gif", ".webp", ".mp4", ".mov", ".ogg", ".mp3", ".doc", ".docx"]
chunked_uploads_enabled = true
chunk_size = "10MB"                        # Chunk size for uploads
resumable_uploads_enabled = true
max_resumable_age = "48h"                  # Maximum age for resumable uploads
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
```

#### Configuration Options

- **allowed_extensions**:  
  - *Type*: `Array of Strings`  
  - *Description*: Lists the file extensions permitted for upload. Includes XMPP-compatible formats.  
  - *Default*: `[".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp", ".txt", ".pdf", ".jpg", ".jpeg", ".png", ".gif", ".webp", ".mp4", ".mov", ".ogg", ".mp3", ".doc", ".docx"]`

- **chunked_uploads_enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables chunked file uploads for better performance with large files.  
  - *Default*: `true`
  
- **chunk_size**:  
  - *Type*: `String`  
  - *Description*: Defines the size of each chunk in chunked uploads.  
  - *Format*: Size (e.g., `"10MB"`)  
  - *Default*: `"10MB"`
  
- **resumable_uploads_enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables support for resumable uploads that can be continued after interruption.  
  - *Default*: `true`
  
- **max_resumable_age**:  
  - *Type*: `String`  
  - *Description*: Maximum time a resumable upload session remains valid.  
  - *Format*: Duration (e.g., `"48h"`)  
  - *Default*: `"48h"`

- **networkevents**:  
  - *Type*: `Boolean`  
  - *Description*: Enables network event monitoring for uploads. Required for network resilience features.  
  - *Default*: `false`

- **session_persistence**:  
  - *Type*: `Boolean`  
  - *Description*: Persists upload sessions across server restarts and network changes.  
  - *Default*: `true`

- **session_recovery_timeout**:  
  - *Type*: `String`  
  - *Description*: Maximum time to wait for session recovery after network changes.  
  - *Format*: Duration (e.g., `"300s"`)  
  - *Default*: `"300s"`

- **allow_session_resume**:  
  - *Type*: `Boolean`  
  - *Description*: Allows upload sessions to resume from different IP addresses (useful for mobile clients).  
  - *Default*: `true`

---

### Downloads Configuration

```toml
# Downloads settings
[downloads]
resumable_downloads_enabled = true
chunked_downloads_enabled = true
chunk_size = "8192"  # Chunk size for downloads
allowed_extensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]
```

#### Configuration Options

- **resumable_downloads_enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables support for resumable downloads.  
  - *Default*: `true`
  
- **chunked_downloads_enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables chunked downloads for better performance.  
  - *Default*: `true`
  
- **chunk_size**:  
  - *Type*: `String`  
  - *Description*: Defines the size of each chunk in chunked downloads.  
  - *Format*: Size (e.g., `"8192"` bytes)  
  - *Default*: `"8192"`

- **allowed_extensions**:  
  - *Type*: `Array of Strings`  
  - *Description*: Lists the file extensions permitted for download.  
  - *Default*: `[".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]`

---

### ClamAV Settings

```toml
# ClamAV settings
[clamav]
clamavenabled = true
clamavsocket = "/var/run/clamav/clamd.ctl"  # Path to ClamAV socket
numscanworkers = 2  # Number of concurrent scan workers
# Only scan potentially dangerous file types, skip large media files
scanfileextensions = [".txt", ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".exe", ".zip", ".rar", ".7z", ".tar", ".gz"]
# Skip scanning files larger than 200MB (ClamAV limit)
maxscansize = "200MB"
```

#### Configuration Options

- **clamavenabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables ClamAV integration for virus scanning of uploaded files.  
  - *Default*: `true`
  
- **clamavsocket**:  
  - *Type*: `String`  
  - *Description*: Specifies the file path to the ClamAV socket (`.ctl` file). Ensure ClamAV is installed and the socket path is correct.  
  - *Default*: `"/var/run/clamav/clamd.ctl"`
  
- **numscanworkers**:  
  - *Type*: `Integer`  
  - *Description*: Sets the number of concurrent workers dedicated to scanning files with ClamAV.  
  - *Default*: `2`
  
- **scanfileextensions**:  
  - *Type*: `Array of Strings`  
  - *Description*: Lists the file extensions that should be scanned for viruses. Excludes large media files for performance.  
  - *Default*: `[".txt", ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".exe", ".zip", ".rar", ".7z", ".tar", ".gz"]`

- **maxscansize**:  
  - *Type*: `String`  
  - *Description*: Maximum file size for virus scanning. Files larger than this are skipped to avoid ClamAV limits.  
  - *Default*: `"200MB"`

---

### Redis Settings

```toml
# Redis settings
[redis]
redisenabled = true
redisdbindex = 0
redisaddr = "localhost:6379"  # Redis server address
redispassword = ""            # Redis password if required
redishealthcheckinterval = "120s"  # Interval for Redis health checks
```

#### Configuration Options

- **redisenabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables Redis integration for caching or session tracking.  
  - *Default*: `true`
  
- **redisaddr**:  
  - *Type*: `String`  
  - *Description*: Specifies the address of the Redis server (e.g., `"localhost:6379"`).  
  - *Default*: `"localhost:6379"`
  
- **redispassword**:  
  - *Type*: `String`  
  - *Description*: Sets the Redis authentication password, if required.  
  - *Default*: `""`
  
- **redisdbindex**:  
  - *Type*: `Integer`  
  - *Description*: Specifies the Redis database index to use.  
  - *Default*: `0`
  
- **redishealthcheckinterval**:  
  - *Type*: `String`  
  - *Description*: Defines the interval for performing health checks on the Redis connection.  
  - *Format*: Duration (e.g., `"120s"` for two minutes)  
  - *Default*: `"120s"`

---

### Workers Configuration

```toml
# Workers settings
[workers]
numworkers = 4               # Number of worker threads
uploadqueuesize = 50         # Size of upload queue
```

#### Configuration Options

- **numworkers**:  
  - *Type*: `Integer`  
  - *Description*: Specifies the base number of worker threads to handle file operations. Works with dynamic worker scaling.  
  - *Default*: `4`

- **uploadqueuesize**:  
  - *Type*: `Integer`  
  - *Description*: Sets the size of the upload queue buffer. Used with dynamic scaling thresholds.  
  - *Default*: `50`

**Note**: When `enable_dynamic_workers = true` in the server section, the worker count automatically scales between 4-8 workers based on queue length (scale up at 50, scale down at 10).

---

### Network Resilience Settings

```toml
# Network resilience configuration for mobile and multi-interface environments
[network_resilience]
enabled = true                              # Enable network resilience system
fast_detection = true                       # Enable 1-second network change detection
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

# Multi-Interface Support (v3.4.0+)
multi_interface_enabled = false            # Enable multi-interface management
interface_priority = ["eth0", "wlan0", "wwan0", "ppp0"]  # Interface priority order
auto_switch_enabled = true                 # Enable automatic interface switching
switch_threshold_latency = "500ms"         # Latency threshold for switching
switch_threshold_packet_loss = 5.0         # Packet loss threshold for switching
quality_degradation_threshold = 0.5        # Quality degradation threshold
max_switch_attempts = 3                    # Maximum switch attempts per detection
switch_detection_interval = "10s"          # Switch detection interval
```

#### Configuration Options

- **enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables the network resilience system for handling network changes and quality monitoring.  
  - *Default*: `true`

- **fast_detection**:  
  - *Type*: `Boolean`  
  - *Description*: Enables 1-second network change detection vs 5-second default for rapid network switching scenarios.  
  - *Default*: `true`

- **quality_monitoring**:  
  - *Type*: `Boolean`  
  - *Description*: Monitors RTT and packet loss per interface to determine network quality and trigger proactive switching.  
  - *Default*: `true`

- **predictive_switching**:  
  - *Type*: `Boolean`  
  - *Description*: Switches networks proactively before complete failure based on quality degradation patterns.  
  - *Default*: `true`

- **mobile_optimizations**:  
  - *Type*: `Boolean`  
  - *Description*: Uses mobile-friendly thresholds for cellular networks with higher tolerance for latency and packet loss.  
  - *Default*: `true`

- **upload_resilience**:  
  - *Type*: `Boolean`  
  - *Description*: Enables upload session preservation and resumption across network changes.  
  - *Default*: `true`

- **multi_interface_enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables management of multiple network interfaces with automatic switching capabilities.  
  - *Default*: `false`

- **interface_priority**:  
  - *Type*: `Array of Strings`  
  - *Description*: Defines the preference order for network interfaces. First interface has highest priority.  
  - *Default*: `["eth0", "wlan0", "wwan0", "ppp0"]`

**Use Cases**:
- Mobile devices switching between WiFi and cellular
- Laptops with Ethernet + WiFi
- IoT devices with primary and backup connections
- Server environments with multiple network adapters

---

### Client Network Support Settings

```toml
# Client network support for handling clients with changing IPs
[client_network_support]
session_based_tracking = false             # Track sessions by ID instead of IP
allow_ip_changes = true                    # Allow session continuation from different IPs
session_migration_timeout = "5m"           # Time to wait for client reconnection
max_ip_changes_per_session = 10           # Maximum IP changes per session
client_connection_detection = false        # Detect client network type
adapt_to_client_network = false           # Optimize parameters based on client connection
```

#### Configuration Options

- **session_based_tracking**:  
  - *Type*: `Boolean`  
  - *Description*: Tracks upload sessions by session ID instead of client IP, enabling seamless operation when clients change networks.  
  - *Default*: `false`

- **allow_ip_changes**:  
  - *Type*: `Boolean`  
  - *Description*: Allows the same upload session to continue from different IP addresses.  
  - *Default*: `true`

- **adapt_to_client_network**:  
  - *Type*: `Boolean`  
  - *Description*: Automatically optimizes upload parameters (chunk size, timeouts) based on detected client connection type.  
  - *Default*: `false`

**Note**: These settings are particularly useful for mobile applications and environments where clients frequently change networks.

---

#### Configuration Options

- **maxfilesize**:  
  - *Type*: `String`  
  - *Description*: Defines the maximum allowed file size for uploads.  
  - *Format*: Size (e.g., `"10GB"`)  
  - *Default*: `"10GB"`

---

### Rate Limiting Configuration

```toml
[rate_limit]
enabled = true
requests_per_minute = 60        # Token refill rate
burst_size = 10                 # Maximum burst of requests
cleanup_interval = "5m"         # Stale bucket eviction interval
by_jid = true                   # Rate limit per XMPP JID
by_ip = true                    # Rate limit per client IP
whitelisted_ips = ["10.0.0.0/8", "192.168.0.0/16"]
whitelisted_jids = ["admin@example.com"]
```

#### Configuration Options

- **enabled**: Enable/disable rate limiting. Default: `false`
- **requests_per_minute**: Token refill rate per key (JID or IP). Default: `60`
- **burst_size**: Maximum burst of requests before throttling. Default: half of `requests_per_minute` (min 5)
- **cleanup_interval**: How often to evict stale rate limit buckets. Default: `"5m"`
- **by_jid**: Rate limit per XMPP JID (from `X-User-JID` header or query parameter). Default: `true`
- **by_ip**: Rate limit per client IP address. Default: `true`
- **whitelisted_ips**: IP addresses or CIDR ranges exempt from rate limiting. Localhost is always whitelisted.
- **whitelisted_jids**: XMPP JIDs exempt from rate limiting.

**Behavior**: When a client exceeds the rate limit, the server returns `429 Too Many Requests` with a `Retry-After` header indicating how many seconds to wait.

---

### HMAC Key Rotation Configuration

```toml
[key_rotation]
enabled = true
rotation_interval = "720h"      # Rotate key every 30 days
grace_period = "48h"             # Accept previous key for 48h after rotation
key_storage = "/etc/hmac-file-server/keys.json"
```

#### Configuration Options

- **enabled**: Enable/disable automatic key rotation. Default: `false`
- **rotation_interval**: How often to rotate the HMAC signing key. Default: `"24h"`
- **grace_period**: Duration after rotation during which the previous key is still accepted for validation. Default: `"24h"`
- **key_storage**: Path to JSON file where keys are persisted (created with `0600` permissions). Default: `"/etc/hmac-file-server/keys.json"`

**How it works**:
1. On startup, loads keys from `key_storage` (or initializes from `[security].secret` if no key file exists)
2. A background goroutine rotates the key at `rotation_interval`
3. New uploads are always signed with the **current** key
4. Download/validation tries: current key → previous key (within grace period) → static config fallback
5. Keys are persisted to disk and survive restarts

**Important**: Ensure the key storage path is writable by the server process and has restricted permissions.

---

### Metadata Store Configuration

```toml
[metadata]
enabled = true
db_path = "/var/lib/hmac-file-server/metadata/files.db"
purge_age = "90d"               # Purge soft-deleted records after 90 days
```

#### Configuration Options

- **enabled**: Enable/disable the SQLite metadata store. Default: `false`
- **db_path**: Path to the SQLite database file. Directory is created automatically. Default: `"<storage_path>/.metadata/files.db"`
- **purge_age**: Age after which soft-deleted metadata records are permanently removed. Default: none (records kept indefinitely)

**What is tracked**:
- File path, name, size, and content type
- Uploader JID and IP address
- Upload timestamp and last access time
- Download count (incremented on each GET request)
- Checksum (SHA256, optional)
- Soft-delete flag (set when file is deleted, preserving history)

**Database**: Uses `modernc.org/sqlite` (pure Go, no CGO required). WAL journal mode for concurrent reads. Single-writer with mutex for consistency.

---

### Admin Dashboard

The admin dashboard provides a web-based UI accessible at `{path_prefix}/dashboard` (default: `/admin/dashboard`). It requires the same authentication as the Admin API.

**Pages**:
- **Dashboard** (`/admin/dashboard`) — Storage stats, user count, uptime, memory/goroutines, feature status cards (Redis, rate limiter, key rotation, cleanup), and recent uploads table. Stats auto-refresh every 30 seconds via HTMX.
- **Files** (`/admin/dashboard/files`) — Full file browser with name, size, content type, modification time, and delete buttons.
- **Users** (`/admin/dashboard/users`) — Per-JID quota usage with progress bars showing usage percentage.

**Technology**: Dark-themed Tailwind CSS via CDN + HTMX for dynamic updates. No npm, no build step — everything is embedded in the Go binary.

**Configuration**: Uses the existing `[admin]` config section. The dashboard is available when `admin.enabled = true`.

```toml
[admin]
enabled = true
path_prefix = "/admin"

[admin.auth]
type = "bearer"
token = "your-admin-token"
```

---

### SIMS Thumbnails (XEP-0385)

Automatic JPEG thumbnail generation for uploaded images, compatible with the XMPP SIMS (Stateless Inline Media Sharing) specification.

**Behavior**:
- On image upload, a 320×240 JPEG thumbnail is generated asynchronously (non-blocking)
- Supported source formats: JPEG, PNG, GIF, BMP, TIFF, WebP
- Thumbnails are stored alongside the original with a `.thumb.jpg` suffix
- Serve thumbnails by appending `?thumbnail=true` or `?thumb=true` to the download URL
- If a thumbnail doesn't exist yet, it's generated on-demand on the first request
- Thumbnails are automatically cleaned up when the source file is deleted

**Example**:
```bash
# Upload an image (normal upload flow)
curl -X PUT https://upload.example.com/upload/host/v/token/photo.jpg --data-binary @photo.jpg

# Download the thumbnail
curl https://upload.example.com/upload/host/v/token/photo.jpg?thumbnail=true
```

**Configuration**: Thumbnails are always enabled when the server is running. No additional config needed.

---

## Configuration Troubleshooting

### Common Configuration Issues

#### ❌ **Field Name Errors**

**Problem**: Service fails to start with `storage path is required` or defaults to `./uploads`

```bash
# ❌ WRONG - Missing underscore
[server]
storagepath = "/opt/hmac-file-server/data/uploads"

# ✅ CORRECT - Use underscores in field names
[server]
storage_path = "/opt/hmac-file-server/data/uploads"
```

**Common Field Name Corrections:**
- `storagepath` → `storage_path`
- `listenport` → `listen_address`
- `bindip` → `bind_ip`
- `pidfilepath` → `pid_file`
- `metricsenabled` → `metrics_enabled`

#### ❌ **Path & Permission Issues**

**Problem**: `directory is not writable: permission denied`

```bash
# Check directory ownership
ls -la /opt/hmac-file-server/data/

# Fix ownership for systemd service
sudo chown -R hmac-file-server:hmac-file-server /opt/hmac-file-server/data/
sudo chmod 750 /opt/hmac-file-server/data/uploads
```

#### ❌ **Network Resilience Not Working**

**Problem**: Network events not detected, uploads don't resume after network changes

```toml
# ✅ Enable network events in uploads section (REQUIRED)
[uploads]
networkevents = true  # This enables the network monitoring system

# ✅ Add network resilience configuration
[network_resilience]
enabled = true
quality_monitoring = true
upload_resilience = true
fast_detection = true
```

**Common Issues**:
- `networkevents = false` (or missing) in uploads section
- Network resilience disabled but expecting network change detection
- Missing `upload_resilience = true` for upload session recovery

#### ❌ **Service Fails with Read-Only File System**

**Problem**: `open uploads/.write_test: read-only file system`

**Cause**: Conflicting local directories or systemd restrictions

```bash
# Remove conflicting directories
sudo rm -rf /opt/hmac-file-server/uploads

# Use absolute paths in configuration
[server]
storage_path = "/opt/hmac-file-server/data/uploads"  # Absolute path
```

### 🛠️ **Quick Diagnostic Commands**

```bash
# 1. Auto-fix common field naming issues (recommended)
./fix-config.sh config.toml

# 2. Validate configuration syntax
./hmac-file-server --validate-config

# 3. Check service logs for errors
journalctl -u hmac-file-server.service -f

# 4. Test configuration manually
sudo -u hmac-file-server ./hmac-file-server -config config.toml --validate-config

# 5. Check directory permissions
ls -la /opt/hmac-file-server/data/
stat /opt/hmac-file-server/data/uploads
```

### 📋 **Configuration Checklist**

Before starting the service, verify:

- ✅ All field names use underscores (`storage_path`, not `storagepath`)
- ✅ Absolute paths for all directories
- ✅ Correct user ownership (`hmac-file-server:hmac-file-server`)
- ✅ Proper directory permissions (750 for data directories)
- ✅ No conflicting local directories in working directory
- ✅ Network events enabled if using network resilience

---

## Configuration Validation

The HMAC File Server v3.4.0 includes a comprehensive configuration validation system with specialized command-line flags for different validation scenarios.

### Available Validation Flags

#### Core Validation Commands

**`--validate-config`**
- **Purpose**: Full comprehensive validation of all configuration sections
- **Usage**: `./hmac-file-server --validate-config`
- **Output**: Complete validation report with all errors and warnings

```bash
# Example
./hmac-file-server -config config.toml --validate-config
```

**`--test-config`**
- **Purpose**: Run predefined configuration test scenarios
- **Usage**: `./hmac-file-server --test-config`
- **Output**: Test scenario results for configuration validation

#### Specialized Validation Modes

**`--check-security`**
- **Purpose**: Security-focused validation only
- **Checks**: Secret strength, default values, JWT algorithms, network exposure, file permissions
- **Example**: `./hmac-file-server -config config.toml --check-security`

**`--check-performance`**
- **Purpose**: Performance-focused validation only  
- **Checks**: Worker optimization, memory usage, timeout balance, large file handling
- **Example**: `./hmac-file-server -config config.toml --check-performance`

**`--check-connectivity`**
- **Purpose**: Network connectivity validation only
- **Checks**: Redis connections, ClamAV sockets, address validation, DNS resolution
- **Example**: `./hmac-file-server -config config.toml --check-connectivity`

#### Output Control Flags

**`--validate-quiet`**
- **Purpose**: Minimal output, returns only exit codes
- **Usage**: Perfect for automation and scripts

**`--validate-verbose`**
- **Purpose**: Detailed output with comprehensive analysis
- **Usage**: Best for troubleshooting and development

**`--check-fixable`**
- **Purpose**: Show only issues that can be automatically fixed
- **Usage**: Helps prioritize configuration improvements

### Validation Categories

#### Security Checks (6 categories)
- Secret strength analysis
- Default value detection  
- Algorithm recommendations
- Network exposure warnings
- File permission analysis
- Debug logging security

#### Performance Checks (5 categories)
- Resource optimization
- Memory usage analysis
- Timeout balancing
- Large file preparation
- Configuration efficiency

#### Connectivity Checks (4 categories)
- Service connectivity
- Socket accessibility  
- Address validation
- DNS resolution

#### System Checks (5 categories)
- CPU availability
- Memory monitoring
- Disk space validation
- Permission testing
- Resource constraints

### Integration Examples

#### Shell Script Integration
```bash
#!/bin/bash
CONFIG_FILE="/etc/hmac-file-server/config.toml"

echo "🔍 Validating HMAC File Server configuration..."

# Run validation
if ./hmac-file-server -config "$CONFIG_FILE" --validate-config; then
    echo "✅ Configuration validation passed"
    
    # Additional specific checks
    echo "🔐 Running security audit..."
    ./hmac-file-server -config "$CONFIG_FILE" --check-security
    
    echo "⚡ Checking performance settings..."
    ./hmac-file-server -config "$CONFIG_FILE" --check-performance
else
    echo "❌ Configuration validation failed"
    echo "💡 Try: ./hmac-file-server -config $CONFIG_FILE --check-fixable"
    exit 1
fi
```

#### Docker Integration
```dockerfile
# Add validation step to Dockerfile
RUN ./hmac-file-server -config /etc/config.toml --validate-config && \
    ./hmac-file-server -config /etc/config.toml --check-security
```

#### Kubernetes Health Check
```yaml
livenessProbe:
  exec:
    command:
    - /usr/local/bin/hmac-file-server
    - -config
    - /etc/config/config.toml
    - --validate-quiet
  initialDelaySeconds: 30
  periodSeconds: 60
```

The enhanced command-line validation system provides comprehensive coverage with 50+ validation checks across all configuration areas, making HMAC File Server v3.4.0 production-ready with enterprise-grade configuration management.

---

## Command-Line Tools & Utilities

HMAC File Server 3.4.0 "Cascade" includes a comprehensive suite of command-line tools and utilities for development, debugging, and maintenance.

### Core Server Options

```bash
# Basic operations
./hmac-file-server -config config.toml                    # Start server
./hmac-file-server -genconfig                             # Generate default config
./hmac-file-server -version                               # Show version info
./hmac-file-server -help                                  # Show help

# Configuration validation
./hmac-file-server -config config.toml --validate        # Validate config
./hmac-file-server -config config.toml --validate-quiet  # Silent validation
./hmac-file-server -config config.toml --check           # Check configuration
```

### Diagnostic & Debugging Tools

```bash
# XMPP Client Troubleshooting (NEW in 3.4.0)
./fix_xmpp_clients.sh                    # Fix desktop client upload issues
./fix_xmpp_clients.sh --clear-cache     # Clear XMPP client caches
./fix_xmpp_clients.sh --dino            # Fix Dino-specific issues
./fix_xmpp_clients.sh --gajim           # Fix Gajim-specific issues

# Network Resilience Verification (NEW in 3.4.0)
./verify_network_resilience.sh          # Test network switching scenarios
./verify_network_resilience.sh --mobile # Test mobile network scenarios
./verify_network_resilience.sh --wifi   # Test WiFi scenarios
```

### Build & Development Tools

```bash
# Multi-Architecture Building (NEW in 3.4.0)
./build-multi-arch.sh                   # Interactive multiarch builder
./build-multi-arch.sh --help           # Show build options

# Docker Multi-Architecture (NEW in 3.4.0)  
./docker-multiarch-build.sh --local    # Build for local testing
./docker-multiarch-build.sh --push     # Build and push to registry
./docker-multiarch-build.sh --help     # Show Docker build options

# Debian Package Building
./builddebian.sh                       # Build .deb packages (AMD64 + ARM64)
./builddebian.sh --help               # Show packaging options

# Docker Standard Building
./builddocker.sh                      # Build standard Docker image
```

### Installation & Setup Tools

```bash
# Automated Installation
./installer.sh                        # Interactive installer
./installer.sh --help                # Show installation options

# Installation Manager (NEW in 3.4.0)
./install-manager.sh                 # Advanced installation management
./install-manager.sh --upgrade       # Upgrade existing installation
./install-manager.sh --uninstall     # Clean uninstallation
```

### Configuration Generation

```bash
# Generate configuration templates
./hmac-file-server -genconfig > config.toml              # Basic config
./hmac-file-server -genconfig-mobile > mobile.toml       # Mobile-optimized
./hmac-file-server -genconfig-enterprise > enterprise.toml # Enterprise config
./hmac-file-server -genconfig-minimal > minimal.toml     # Minimal config

# Configuration examples available:
# - config-mobile-resilient.toml        # Mobile resilience optimized
# - config-production-enhanced.toml     # Production deployment
# - config-production-validated.toml    # Validated production config
```

### Environment Variables

```bash
# Common environment variables
export HMAC_SECRET="your-secret-key"           # HMAC authentication secret
export STORAGE_PATH="/data/uploads"            # Upload storage directory
export LISTEN_PORT="8080"                      # Server listen port
export LOG_LEVEL="info"                        # Logging level
export PROMETHEUS_PORT="9090"                  # Metrics port

# Development mode
export HMAC_DEV_MODE="true"                    # Enable development features
export HMAC_DEBUG="true"                       # Enable debug logging
export HMAC_TRACE="true"                       # Enable trace logging
```

---

## Development & Build Tools

### Multi-Architecture Build System

HMAC File Server 3.4.0 features a comprehensive multi-architecture build system supporting 13+ platforms.

#### Interactive Builder

```bash
./build-multi-arch.sh
```

**Menu Options:**
1. **All supported platforms** - Complete multiarch build (Linux, macOS, Windows, FreeBSD)
2. **Linux only** - AMD64, ARM64, ARM32v7 for server deployment
3. **Cross-platform** - Linux, macOS, Windows for desktop distribution  
4. **Custom selection** - Choose specific platforms
5. **Quick build** - Linux AMD64 only for rapid development

#### Supported Platforms

| Platform | Architecture | Use Case |
|----------|-------------|----------|
| `linux/amd64` | x86-64 | Data centers, cloud instances |
| `linux/arm64` | ARM 64-bit | Apple Silicon, AWS Graviton, Pi 4+ |
| `linux/arm` | ARM 32-bit | Raspberry Pi 3, IoT devices |
| `linux/386` | x86 32-bit | Legacy systems |
| `darwin/amd64` | Intel Mac | macOS Intel development |
| `darwin/arm64` | Apple Silicon | macOS M1/M2/M3 development |
| `windows/amd64` | Windows 64-bit | Windows server deployment |
| `windows/386` | Windows 32-bit | Legacy Windows systems |
| `freebsd/amd64` | FreeBSD | BSD server deployment |
| `openbsd/amd64` | OpenBSD | Security-focused deployment |

#### Docker Multi-Architecture

```bash
# Local development
./docker-multiarch-build.sh --local

# Production deployment  
./docker-multiarch-build.sh --registry your-registry.com --push
```

**Features:**
- **Docker Buildx integration** - Native multi-platform support
- **Platform targeting** - `linux/amd64,linux/arm64,linux/arm/v7`
- **Registry push** - Automated multi-arch image distribution
- **Local testing** - Build and load for immediate testing

#### Manual Build Commands

```bash
# Linux AMD64 (Primary)
GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -ldflags="-w -s" -o builds/hmac-file-server-linux-amd64 ./cmd/server/

# Linux ARM64 (Apple Silicon, Graviton)
GOOS=linux GOARCH=arm64 CGO_ENABLED=0 go build -ldflags="-w -s" -o builds/hmac-file-server-linux-arm64 ./cmd/server/

# Linux ARM32v7 (Raspberry Pi)
GOOS=linux GOARCH=arm GOARM=7 CGO_ENABLED=0 go build -ldflags="-w -s" -o builds/hmac-file-server-linux-arm ./cmd/server/

# macOS Universal
GOOS=darwin GOARCH=amd64 CGO_ENABLED=0 go build -ldflags="-w -s" -o builds/hmac-file-server-darwin-amd64 ./cmd/server/
GOOS=darwin GOARCH=arm64 CGO_ENABLED=0 go build -ldflags="-w -s" -o builds/hmac-file-server-darwin-arm64 ./cmd/server/

# Windows
GOOS=windows GOARCH=amd64 CGO_ENABLED=0 go build -ldflags="-w -s" -o builds/hmac-file-server-windows-amd64.exe ./cmd/server/
```

### Debian Package System

```bash
./builddebian.sh
```

**Features:**
- **Multi-architecture packages** - AMD64 and ARM64 .deb files
- **Systemd integration** - Complete service configuration
- **Dependency management** - Automatic dependency resolution
- **Configuration templates** - Production-ready configs included

**Generated Packages:**
- `hmac-file-server_3.4.0_amd64.deb` - AMD64 Debian package
- `hmac-file-server_3.4.0_arm64.deb` - ARM64 Debian package

### Container Build Tools

#### Standard Docker Build
```bash
./builddocker.sh                    # Standard single-arch Docker build
```

#### Podman Support
```bash
# Clone repository
git clone https://git.uuxo.net/uuxo/hmac-file-server.git
cd hmac-file-server/dockerenv/podman

# One-command deployment
./deploy-podman.sh

# Check status
./deploy-podman.sh status
```

---

## Example Configuration

Below is an example `config.toml` file with current settings:

```toml
# Example HMAC File Server configuration

[server]
listen_address = ":8080"
storage_path = "/srv/hmac-file-server/uploads"
metrics_enabled = true
metrics_path = "/metrics"
pid_file = "/var/run/hmac-file-server.pid"
max_upload_size = "10GB"
max_header_bytes = 1048576
cleanup_interval = "24h"
max_file_age = "720h"
pre_cache = true
pre_cache_workers = 4
pre_cache_interval = "1h"
global_extensions = [".txt", ".dat", ".iso", ".mp4", ".mkv", ".avi", ".mov", ".wmv", ".flv", ".webm", ".mpeg"]
deduplication_enabled = true
min_free_bytes = "1GB"
file_naming = "original"
force_protocol = ""
enable_dynamic_workers = true
worker_scale_up_thresh = 40   # 40% optimized threshold for 3.2
worker_scale_down_thresh = 10

[uploads]
allowed_extensions = [".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp", ".txt", ".pdf", ".jpg", ".jpeg", ".png", ".gif", ".webp", ".mp4", ".mov", ".ogg", ".mp3", ".doc", ".docx"]
chunked_uploads_enabled = true
chunk_size = "10MB"
resumable_uploads_enabled = true
max_resumable_age = "48h"
sessiontimeout = "60m"
maxretries = 3
networkevents = true                        # Enable network event monitoring
session_persistence = true
session_recovery_timeout = "300s"
client_reconnect_window = "120s"
allow_session_resume = true

[downloads]
resumable_downloads_enabled = true
chunked_downloads_enabled = true
chunk_size = "8192"
allowed_extensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[security]
secret = "f6g4ldPvQM7O2UTFeBEUUj33VrXypDAcsDt0yqKrLiOr5oQW"
enablejwt = false
jwtsecret = "f6g4ldPvQM7O2UTFeBEUUj33VrXypDAcsDt0yqKrLiOr5oQW"
jwtalgorithm = "HS256"
jwtexpiration = "24h"

[logging]
level = "info"
file = "/var/log/hmac-file-server.log"
max_size = 100
max_backups = 7
max_age = 30
compress = true

[deduplication]
enabled = true
directory = "/opt/hmac-file-server/data/dedup"
maxsize = "1GB"

[iso]
enabled = true
size = "1GB"
mountpoint = "/mnt/iso"
charset = "utf-8"
containerfile = "/mnt/iso/container.iso"

[timeouts]
readtimeout = "4800s"
writetimeout = "4800s"
idletimeout = "4800s"

[versioning]
enableversioning = false
maxversions = 1

[clamav]
clamavenabled = true
clamavsocket = "/var/run/clamav/clamd.ctl"
numscanworkers = 2
scanfileextensions = [".txt", ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".exe", ".zip", ".rar", ".7z", ".tar", ".gz"]
maxscansize = "200MB"

[redis]
redisenabled = true
redisdbindex = 0
redisaddr = "localhost:6379"
redispassword = ""
redishealthcheckinterval = "120s"

[workers]
numworkers = 4
uploadqueuesize = 50

# Network Resilience (v3.4.0+)
[network_resilience]
enabled = true
fast_detection = true
quality_monitoring = true
predictive_switching = true
mobile_optimizations = false               # Use strict thresholds for server environment
upload_resilience = true
detection_interval = "5s"                  # Standard detection for servers
quality_check_interval = "10s"
network_change_threshold = 3
interface_stability_time = "30s"
upload_pause_timeout = "5m"
upload_retry_timeout = "10m"
rtt_warning_threshold = "200ms"
rtt_critical_threshold = "1000ms"
packet_loss_warning_threshold = 2.0
packet_loss_critical_threshold = 10.0

# Multi-interface support (optional)
multi_interface_enabled = false            # Enable for multi-interface setups
interface_priority = ["eth0", "wlan0", "wwan0", "ppp0"]
auto_switch_enabled = true
switch_threshold_latency = "500ms"
switch_threshold_packet_loss = 5.0

# Client Network Support (v3.4.0+)
[client_network_support]
session_based_tracking = false             # Standard IP-based tracking for servers
allow_ip_changes = true                    # Allow for client network changes
session_migration_timeout = "5m"
max_ip_changes_per_session = 10
client_connection_detection = false
adapt_to_client_network = false

[file]
# Add file-specific configurations here

[build]
version = "3.4.0"
```

---

## Configuration Best Practices

### CPU Instruction Set Extensions (ISA) & Hardware Acceleration

Starting with version 3.4+, the HMAC File Server **automatically detects CPU instruction set extensions** at startup and uses them to optimize compression algorithm selection, cryptographic operations, and I/O throughput.

#### Detected ISA Extensions

| Extension | Intel Name | AMD Name | Performance Impact |
|-----------|-----------|----------|-------------------|
| **AES-NI** | Intel AES-NI | AMD AES Instructions | HMAC-SHA256 & TLS: **10× faster** encrypt/decrypt + side-channel attack protection |
| **SSE4.2** | Intel SSE4.2 | AMD SSE4.2 | CRC32c checksums, string comparisons — used by **XXHash** in zstd |
| **AVX2** | Intel AVX2 | AMD AVX2 | 256-bit SIMD: parallel data scanning, checksum computation |
| **AVX-512** | Intel AVX-512 | AMD AVX-512 (Zen 4 / Ryzen 7000+) | 512-bit SIMD: maximum throughput for checksum and pattern matching |
| **BMI2** | Intel BMI2 | AMD BMI2 (full-speed Zen 3+) | **PEXT/PDEP**: critical for zstd entropy coding (HUF/FSE) — **2-4× speedup** |
| **BMI1** | Intel BMI1 | AMD BMI1 | Bit manipulation instructions (ANDN, BEXTR, BLSI, BLSMSK, BLSR, TZCNT) |
| **SHA-NI** | Intel SHA Extensions | AMD SHA (Zen+) | Hardware SHA-256: accelerates HMAC signature verification |
| **CLMUL** | Intel PCLMULQDQ | AMD CLMUL | Carry-less multiply: GCM-mode AES, CRC computation |
| **POPCNT** | Intel POPCNT | AMD POPCNT | Population count — used in hash tables and bitmap operations |
| **RDRAND** | Intel RDRAND | AMD RDRAND | Hardware random number generator for secure token creation |

#### CPU Minimum Requirements by Feature

| Feature | Minimum Intel | Minimum AMD | Why |
|---------|--------------|-------------|-----|
| AES-NI (hardware crypto) | Westmere (2010) | Bulldozer (2011) | Without it, HMAC verification uses 10× slower software crypto |
| SSE4.2 (checksums) | Nehalem (2008) | Bulldozer (2011) | XXHash and CRC32 acceleration |
| AVX2 (SIMD vectors) | Haswell (2013) | Excavator (2015) | Parallel data scanning in transfer operations |
| BMI2 (entropy coding) | Haswell (2013) | Zen 3 (2020)¹ | zstd HUF/FSE decompression at full speed |
| AVX-512 (wide SIMD) | Skylake-X (2017) | Zen 4 (2022) | Maximum checksum throughput |
| SHA-NI | Ice Lake (2019) | Zen (2017) | Hardware SHA-256 for signatures |

> ¹ **AMD BMI2 Note**: AMD CPUs before Zen 3 (Ryzen 5000) implement PEXT/PDEP in microcode, making them **~10× slower** than Intel's single-cycle implementation. For optimal zstd performance, AMD Zen 3+ is required.

#### How ISA Extensions Affect Compression Selection

The server's **CPU Auto-Optimization** automatically selects the best compression algorithm based on detected hardware:

| Compression Tier | Required ISA | Algorithm | Level | Reason |
|-----------------|-------------|-----------|-------|--------|
| **optimal** | BMI2 + AVX2 | zstd | 3 | Full-speed entropy coding via PEXT + SIMD checksums via AVX2 |
| **good** | BMI2 + SSE4.2 | zstd | 1 | Entropy coding accelerated, narrower checksum parallelism |
| **baseline** | SSE2 only | gzip | 6 | BMI2 missing → zstd entropy coding falls back to slow shifts |
| **minimal** | none | gzip | 4 | No SIMD → pure software, lower level for acceptable speed |

**Why this matters for zstd specifically:**
- **BMI2 (PEXT)**: zstd's Huffman and FSE entropy decoders extract variable-length bit fields from packed streams. With PEXT, this is a single CPU instruction. Without it, zstd emulates PEXT with 5-8 shift/mask operations per field extraction.
- **SSE4.2 / AVX2**: zstd uses XXHash64 for block checksums. SSE4.2 provides CRC32c and string scanning acceleration. AVX2 enables 32-byte parallel processing of hash computation.
- **Without these extensions**, zstd in software is often **slower than gzip** for equivalent ratios, which is why the auto-optimizer falls back to gzip on older hardware.

#### Startup Log Output

When the server starts, it logs detected ISA extensions:

```
INFO CPU: Intel(R) Core(TM) i7-12700K, Cores=20
INFO CPU ISA Extensions: AES-NI SSE4.2 AVX AVX2 BMI1 BMI2 POPCNT CLMUL SHA-NI (GenuineIntel)
INFO Compression Tier: optimal (recommended algorithm: zstd)
INFO Hardware Crypto: AES-NI available — HMAC/TLS operations hardware-accelerated
INFO Compression Hint: BMI2+AVX2 detected — zstd entropy coding (HUF/FSE) hardware-accelerated
INFO Compression Auto-Selection: algorithm=zstd level=3 tier=optimal
```

On an older or minimal CPU:

```
WARN Hardware Crypto: AES-NI NOT detected — cryptographic operations use software fallback
WARN Compression Tier: baseline (recommended algorithm: gzip)
INFO Compression Auto-Selection: algorithm=gzip level=6 tier=baseline
```

#### Configuration

Add to your `config.toml`:

```toml
[compression]
# Enable transfer compression (default: false)
enabled = false

# Algorithm: "auto" (CPU-based selection), "zstd", "gzip", "none"
algorithm = "auto"

# Compression level: 0 = auto-select based on CPU tier
# gzip: 1-9, zstd: 1-22
level = 0

# Minimum file size for compression (smaller files skip compression)
threshold = "1KB"

# Adjust compression level dynamically based on CPU load
adaptive_compression = true

# Honor client Accept-Encoding headers
compression_negotiation = true
```

#### Config Validation

The `--check-performance` validator now reports ISA extension warnings:

```bash
./hmac-file-server -config config.toml --check-performance
```

Example output:
```
⚠ system.cpu.bmi2: BMI2 not detected — zstd entropy coding (HUF/FSE via PEXT/PDEP)
  cannot use hardware acceleration. Intel: requires Haswell (2013+),
  AMD: requires Zen 3 (2020+) for full-speed PEXT
⚠ system.cpu.compression: baseline — Only baseline SIMD (SSE2) available, gzip recommended.
  Upgrade to a CPU with BMI2+AVX2 for optimal zstd performance.
```

### Performance Optimizations

**Large File Handling**: The server is configured for efficient large file uploads:
- Extended timeouts (`4800s`) prevent disconnections during GB-sized transfers
- Dynamic worker scaling (`enable_dynamic_workers = true`) adjusts capacity automatically
- Chunked uploads (`chunk_size = "10MB"`) improve reliability for large files

**Storage Efficiency**: 
- Deduplication (`enabled = true`, `maxsize = "1GB"`) uses hard links to save space
- Selective virus scanning (`scanfileextensions`) skips large media files for performance
- Automatic cleanup (`cleanup_interval = "24h"`) maintains disk space

**Network Configuration**:
- Reverse proxy timeouts must match server settings (`proxy_read_timeout 4800`)
- Upload size limits should accommodate `max_upload_size = "10GB"`
- Queue settings (`uploadqueuesize = 50`) balance memory usage and throughput

### Security Considerations

- Use strong HMAC secrets (64+ characters)
- Limit file extensions based on your use case
- Enable ClamAV for dangerous file types only
- Monitor logs for authentication failures

### Monitoring

- Prometheus metrics available at `/metrics` endpoint
- Log rotation configured to prevent disk space issues
- Worker scaling and queue metrics help identify bottlenecks

### XMPP Client Large File Upload (Gajim 1GB+ Multi-Upload Fix)

**Problem**: XMPP clients like Gajim experience "bad gateway" errors when uploading large files (>1GB) in multi-transfer scenarios.

**Root Cause**: When using nginx reverse proxy, conflicts occur between:
- CORS handling (nginx vs. server)
- Inadequate timeout settings for large files
- Session persistence issues during multi-upload

#### ✅ **Complete Solution**

**1. Enhanced CORS Configuration** (`cmd/server/helpers.go`):
```go
// Extended CORS headers for large file multi-upload scenarios
Access-Control-Allow-Headers: Authorization, Content-Type, Content-Length, 
    X-Requested-With, X-Upload-ID, X-Session-Token, X-File-Name, 
    X-File-Size, Range, Content-Range
Access-Control-Expose-Headers: Content-Length, Content-Range, 
    X-Upload-Status, X-Session-ID, Location, ETag
```

**2. Extended Server Timeouts** (`config.toml`):
```toml
# Large file upload timeouts (2 hours for 1GB+ files)
readtimeout = "7200s"           # 2 hours for reading large uploads
writetimeout = "7200s"          # 2 hours for writing large responses
idletimeout = "1800s"           # 30 minutes idle timeout
sessiontimeout = "60m"          # 60 minutes session persistence
upload_pause_timeout = "30m"    # 30 minutes upload pause tolerance
upload_retry_timeout = "60m"    # 60 minutes retry window
```

**3. Optimized Nginx Proxy Configuration**:
```nginx
server {
    listen 443 ssl http2;
    server_name your-server.com;
    
    # Enhanced large file upload settings for 1GB+ multi-transfer
    client_max_body_size 10G;          # Support up to 10GB files
    client_body_timeout 7200s;         # 2 hours for large uploads
    client_header_timeout 300s;
    client_body_buffer_size 2m;        # Increased buffer for large files
    send_timeout 7200s;                # 2 hours to match server timeouts

    location / {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # CRITICAL: Let server handle ALL CORS (remove nginx CORS)
        # Do NOT add nginx CORS headers here - causes conflicts!
        
        # Enhanced timeout settings for large file uploads (2 hours)
        proxy_connect_timeout 7200s;
        proxy_send_timeout    7200s;
        proxy_read_timeout    7200s;
        keepalive_timeout     1800s;    # 30 minutes for multi-upload sessions

        # Connection persistence and resilience for multi-transfer
        proxy_socket_keepalive on;
        proxy_next_upstream error timeout http_502 http_503 http_504;
        proxy_next_upstream_timeout 7200s;
        proxy_next_upstream_tries 3;   # Allow retries for large file failures
    }
}
```

**4. Multi-Upload Session Management** (`cmd/server/session_auth.go`):
- Session ID generation for connection persistence
- Enhanced error handling for large file scenarios
- Connection tracking across multiple uploads

#### 🧪 **Testing Large File Multi-Upload**

Use the provided test script to verify the fix:
```bash
# Test comprehensive large file multi-upload configuration
./test-large-file-multiupload.sh
```

**Expected Results**:
- ✅ All CORS preflight tests: PASSED
- ✅ Multi-upload simulation: PASSED  
- ✅ Large file headers: SUPPORTED
- ✅ Timeout configuration: OPTIMAL

#### 🚀 **Implementation Summary**

**Key Improvements**:
- **Removed nginx CORS conflicts** (server handles all CORS)
- **Extended all timeouts to 7200s** (2 hours for 1GB+ files)
- **Enhanced session management** for multi-upload persistence
- **Improved connection resilience** with retry mechanisms
- **10GB max file size support** with optimized buffers

**Result**: Gajim and other XMPP clients can now successfully upload files >1GB in multi-transfer scenarios without "bad gateway" errors.

**Files Modified**:
- `cmd/server/helpers.go` - Enhanced CORS with multi-upload headers
- `cmd/server/session_auth.go` - Session management for multi-upload tracking
- `/etc/nginx/conf.d/your-site.conf` - Nginx proxy optimization
- `config.toml` - Extended timeouts for large file handling

---

## Setup Instructions

### 1. HMAC File Server Installation

To install the HMAC File Server, follow these steps:

1. Clone the repository:
    ```sh
    git clone https://git.uuxo.net/uuxo/hmac-file-server.git
    cd hmac-file-server
    ```

2. Build the server:
    ```sh
    go build -o hmac-file-server ./cmd/server/
    ```

3. Create the necessary directories:
    ```sh
    mkdir -p /path/to/hmac-file-server/data/
    mkdir -p /path/to/hmac-file-server/deduplication/
    mkdir -p /path/to/hmac-file-server/iso/
    ```

4. Copy the example configuration file:
    ```sh
    cp config.example.toml config.toml
    ```

5. Edit the `config.toml` file to match your environment and preferences.

6. Start the server:
    ```sh
    ./hmac-file-server -config config.toml
    ```

### 2. Reverse Proxy Configuration

To set up a reverse proxy for the HMAC File Server, you can use either Apache2 or Nginx. Below are the configuration examples for both.

#### Apache2 Reverse Proxy

1. Enable the necessary Apache2 modules:
    ```sh
    sudo a2enmod proxy
    sudo a2enmod proxy_http
    sudo a2enmod headers
    sudo a2enmod rewrite
    ```

2. Create a new virtual host configuration file:
    ```sh
    sudo nano /etc/apache2/sites-available/hmac-file-server.conf
    ```

3. Add the following configuration to the file:
    ```apache
    <VirtualHost *:80>
        ServerName your-domain.com

        ProxyPreserveHost On
        ProxyPass / http://localhost:8080/
        ProxyPassReverse / http://localhost:8080/

        <Location />
            Require all granted
            Header always set X-Content-Type-Options "nosniff"
            Header always set X-Frame-Options "DENY"
            Header always set X-XSS-Protection "1; mode=block"
        </Location>
    </VirtualHost>
    ```

4. Enable the new site and restart Apache2:
    ```sh
    sudo a2ensite hmac-file-server.conf
    sudo systemctl restart apache2
    ```

#### Nginx Reverse Proxy

1. Install Nginx if not already installed:
    ```sh
    sudo apt-get update
    sudo apt-get install nginx
    ```

2. Create a new server block configuration file:
    ```sh
    sudo nano /etc/nginx/sites-available/hmac-file-server
    ```

3. Add the following configuration to the file:
    ```nginx
    server {
        listen 80;
        server_name your-domain.com;

        location / {
            proxy_pass http://localhost:8080;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_set_header X-Content-Type-Options "nosniff";
            proxy_set_header X-Frame-Options "DENY";
            proxy_set_header X-XSS-Protection "1; mode=block";
        }
    }
    ```

4. Enable the new site and restart Nginx:
    ```sh
    sudo ln -s /etc/nginx/sites-available/hmac-file-server /etc/nginx/sites-enabled/
    sudo systemctl restart nginx
    ```

---

### Proxy Best Practices & Recommendations

For production deployments, consider the following reverse proxy best practices:

- **Timeouts**: Set extended timeouts (e.g., `proxy_read_timeout 4800;` in Nginx) for large file handling.
- **Buffer Sizes**: Increase buffer sizes for large file uploads/downloads if needed (e.g., `client_max_body_size 2G;` in Nginx).
- **Headers**: Always set security headers (`X-Content-Type-Options`, `X-Frame-Options`, `X-XSS-Protection`).
- **Forwarded Headers**: Ensure `X-Forwarded-For` and `X-Forwarded-Proto` are set for correct client IP and protocol logging.
- **HTTP/2**: Enable HTTP/2 for better performance if supported by your proxy and clients.
- **SSL/TLS**: Terminate SSL at the proxy and use strong ciphers. Redirect HTTP to HTTPS.
- **Health Checks**: Configure health checks for the backend server to enable automatic failover or alerting.
- **Access Controls**: Restrict access to the management endpoints (e.g., `/metrics`) to trusted IPs only.

See the official Nginx and Apache documentation for more advanced tuning options.

---

#### 3. ejabberd Configuration

```yaml
hosts:
  - "your-domain.com"

listen:
  -
    port: 5222
    module: ejabberd_c2s
    certfile: "/etc/ejabberd/ejabberd.pem"
    starttls: true
    starttls_required: true
    protocol_options:
      - "no_sslv3"
      - "no_tlsv1"
      - "no_tlsv1_1"
    ciphers: "HIGH:!aNULL:!eNULL:!3DES:@STRENGTH"
    dhfile: "/etc/ejabberd/dhparams.pem"
    max_stanza_size: 65536
    shaper: c2s_shaper
    access: c2s

  -
    port: 5269
    module: ejabberd_s2s_in
    certfile: "/etc/ejabberd/ejabberd.pem"
    starttls: true
    starttls_required: true
    protocol_options:
      - "no_sslv3"
      - "no_tlsv1"
      - "no_tlsv1_1"
    ciphers: "HIGH:!aNULL:!eNULL:!3DES:@STRENGTH"
    dhfile: "/etc/ejabberd/dhparams.pem"
    max_stanza_size: 131072
    shaper: s2s_shaper
    access: s2s

acl:
  local:
    user_regexp: ""

access_rules:
  local:
    allow: local

mod_http_upload:
    max_size: 1073741824
    thumbnail: true
    put_url: https://share.uuxo.net
    get_url: https://share.uuxo.net
    external_secret: "changeme"
    custom_headers:
      "Access-Control-Allow-Origin": "*"
      "Access-Control-Allow-Methods": "GET,HEAD,PUT,OPTIONS"
      "Access-Control-Allow-Headers": "Content-Type"
```

4. Restart ejabberd:
    ```sh
    sudo systemctl restart ejabberd
    ```

### 4. Systemd Service Setup

To set up the HMAC File Server as a systemd service, follow these steps:

1. Create a new systemd service file:
    ```sh
    sudo nano /etc/systemd/system/hmac-file-server.service
    ```

2. Add the following configuration to the file:
    ```ini
    [Unit]
    Description=HMAC File Server
    After=network.target

    [Service]
    ExecStart=/path/to/hmac-file-server -config /path/to/config.toml
    WorkingDirectory=/path/to/hmac-file-server
    Restart=always
    User=www-data
    Group=www-data

    [Install]
    WantedBy=multi-user.target
    ```

3. Reload systemd and enable the service:
    ```sh
    sudo systemctl daemon-reload
    sudo systemctl enable hmac-file-server
    sudo systemctl start hmac-file-server
    ```

---

## Running with Docker & Docker Compose

You can run the HMAC File Server using Docker and Docker Compose for easy deployment and environment management.

### Automated Docker Deployment (Recommended)

The easiest way to deploy with Docker is using the automated installer:

```bash
git clone https://git.uuxo.net/uuxo/hmac-file-server.git
cd hmac-file-server
sudo ./installer.sh
```

When prompted, select **Option 2: Docker deployment (docker-compose)**. The installer will:
- Create a complete docker-compose.yml with Redis and ClamAV services
- Generate an optimized multi-stage Dockerfile
- Set up proper networking between services
- Create start/stop scripts (`start.sh`, `stop.sh`)
- Configure container-optimized configuration
- Provide an isolated deployment directory structure

After installation, manage your deployment with:
```bash
cd /path/to/your/deployment/directory
./start.sh    # Start all services
./stop.sh     # Stop all services
```

### Manual Docker Compose Setup

### Manual Docker Compose Setup

For manual Docker setup without the installer, use this docker-compose.yml:

```yaml
version: '3.8'

services:
  hmac-file-server:
    image: ghcr.io/plusone/hmac-file-server:latest
    ports:
      - "8080:8080"
    volumes:
      - ./config:/etc/hmac-file-server
      - ./data/uploads:/opt/hmac-file-server/data/uploads
      - ./data/duplicates:/opt/hmac-file-server/data/duplicates
      - ./data/temp:/opt/hmac-file-server/data/temp
      - ./data/logs:/opt/hmac-file-server/data/logs
    environment:
      - CONFIG_PATH=/etc/hmac-file-server/config.toml
    restart: unless-stopped
```

**Key paths:**
- `/etc/hmac-file-server/config.toml`: Main config file (mount your config here)
- `/opt/hmac-file-server/data/uploads`: Upload storage
- `/opt/hmac-file-server/data/duplicates`: Deduplication data
- `/opt/hmac-file-server/data/temp`: Temporary files
- `/opt/hmac-file-server/data/logs`: Log files

---

## Running with Podman

Podman is a daemonless container engine that's often preferred in enterprise environments for enhanced security and rootless capabilities. HMAC File Server 3.4.0 provides complete Podman support with optimized deployment scripts.

### Why Choose Podman?

| Feature | Docker | Podman |
|---------|--------|--------|
| **Daemon** | Requires Docker daemon | Daemonless architecture |
| **Root Access** | Requires root for Docker daemon | Can run completely rootless |
| **Security** | Good, but daemon runs as root | Enhanced security, no privileged daemon |
| **Systemd Integration** | Via Docker service | Native systemd integration |
| **Pod Support** | Requires docker-compose or swarm | Native Kubernetes-style pods |
| **Enterprise Use** | Popular in startups/mid-size | Preferred in enterprise environments |
| **SELinux** | Basic support | Excellent SELinux integration |

### Quick Start with Podman

```bash
# Clone repository
git clone https://git.uuxo.net/uuxo/hmac-file-server.git
cd hmac-file-server/dockerenv/podman

# One-command deployment
./deploy-podman.sh

# Check status
./deploy-podman.sh status

# View logs
./deploy-podman.sh logs
```

### Manual Directory & Permission Setup

#### **Understanding Container Security Model**
- **Container User**: `appuser` (UID: 1011, GID: 1011)
- **Security Principle**: Never run containers as root (UID 0)
- **Compatibility**: Works across different container runtimes and deployment modes

#### **Step-by-Step Manual Setup**

**Step 1: Create Directory Structure**
```bash
# Create organized directory layout
export HMAC_BASE="/opt/podman/hmac-file-server"
sudo mkdir -p ${HMAC_BASE}/{config,data,deduplication,logs}

# Create subdirectories for uploads and duplicates
sudo mkdir -p ${HMAC_BASE}/data/{uploads,temp}
sudo mkdir -p ${HMAC_BASE}/deduplication/store
sudo mkdir -p ${HMAC_BASE}/logs/{access,error,debug}
```

**Step 2: Set Ownership (CRITICAL)**
```bash
# For Podman Rootless (recommended)
podman unshare chown -R 1011:1011 ${HMAC_BASE}

# For Podman Rootful or Docker
sudo chown -R 1011:1011 ${HMAC_BASE}

# Alternative: Use numeric IDs to avoid user lookup issues
sudo chown -R 1011:1011 ${HMAC_BASE}
```

**Step 3: Set Permissions**
```bash
# Directory permissions (executable for traversal)
sudo chmod 755 ${HMAC_BASE}
sudo chmod 755 ${HMAC_BASE}/{config,data,deduplication,logs}
sudo chmod 755 ${HMAC_BASE}/data/{uploads,temp}
sudo chmod 755 ${HMAC_BASE}/deduplication/store
sudo chmod 755 ${HMAC_BASE}/logs/{access,error,debug}

# Configuration file (read-only)
sudo chmod 644 ${HMAC_BASE}/config/config.toml
```

**Step 4: Verify Ownership**
```bash
# Check ownership recursively
ls -laR ${HMAC_BASE}

# Expected output format:
# drwxr-xr-x 2 1011 1011 4096 Dec 20 10:30 data
# drwxr-xr-x 2 1011 1011 4096 Dec 20 10:30 deduplication  
# drwxr-xr-x 2 1011 1011 4096 Dec 20 10:30 logs
# -rw-r--r-- 1 1011 1011 1234 Dec 20 10:30 config/config.toml
```

#### **Container Volume Mapping**

| Host Path | Container Mount | Access Mode | SELinux Label | Purpose |
|-----------|-----------------|-------------|---------------|---------|
| `${HMAC_BASE}/config/config.toml` | `/app/config.toml` | `ro` | `:Z` | Configuration file |
| `${HMAC_BASE}/data/` | `/data/` | `rw` | `:Z` | File uploads |
| `${HMAC_BASE}/deduplication/` | `/deduplication/` | `rw` | `:Z` | Dedup cache |
| `${HMAC_BASE}/logs/` | `/logs/` | `rw` | `:Z` | Application logs |

#### **Complete Manual Run Command**
```bash
# Build container image
podman build -t localhost/hmac-file-server:latest -f dockerenv/podman/Dockerfile.podman .

# Run with proper volume mounts and SELinux labels
podman run -d --name hmac-file-server \
  --security-opt no-new-privileges \
  --cap-drop=ALL \
  --read-only \
  --tmpfs /tmp:rw,noexec,nosuid,size=100m \
  -p 8888:8888 \
  -p 9090:9090 \
  -v ${HMAC_BASE}/config/config.toml:/app/config.toml:ro,Z \
  -v ${HMAC_BASE}/data:/data:rw,Z \
  -v ${HMAC_BASE}/deduplication:/deduplication:rw,Z \
  -v ${HMAC_BASE}/logs:/logs:rw,Z \
  localhost/hmac-file-server:latest -config /app/config.toml
```

### Troubleshooting Path & Permission Issues

#### **Common Error: Permission Denied**
```bash
# Error in logs
{"level":"error","msg":"failed to create directories: mkdir /data: permission denied"}
```

**Root Cause**: Incorrect ownership or missing directories

**Solution**:
```bash
# Check current ownership
ls -la ${HMAC_BASE}

# Fix ownership (adjust command based on your setup)
# For rootless Podman:
podman unshare chown -R 1011:1011 ${HMAC_BASE}

# For rootful Podman/Docker:
sudo chown -R 1011:1011 ${HMAC_BASE}

# Verify fix
sudo -u "#1011" touch ${HMAC_BASE}/data/test-write
rm ${HMAC_BASE}/data/test-write
```

#### **Common Error: SELinux Denial**
```bash
# Error in logs or journalctl
SELinux is preventing access to 'write' on the file /data/test.txt
```

**Root Cause**: SELinux context not set for container volumes

**Solution**:
```bash
# Option 1: Use :Z labels (recommended - private volumes)
-v ${HMAC_BASE}/data:/data:rw,Z

# Option 2: Use :z labels (shared volumes between containers)  
-v ${HMAC_BASE}/data:/data:rw,z

# Option 3: Set SELinux boolean (system-wide change)
sudo setsebool -P container_manage_cgroup on

# Option 4: Manual context setting
sudo semanage fcontext -a -t container_file_t "${HMAC_BASE}(/.*)?"
sudo restorecon -R ${HMAC_BASE}
```

#### **Common Error: User Namespace Issues**
```bash
# Error starting container
Error: can't stat ${HMAC_BASE}/data: permission denied
```

**Root Cause**: User namespace mapping issues in rootless mode

**Solution**:
```bash
# Check user namespace configuration
cat /etc/subuid /etc/subgid | grep $USER

# If missing, add mappings (requires root)
sudo usermod --add-subuids 1000-65536 $USER
sudo usermod --add-subgids 1000-65536 $USER

# Restart user services
systemctl --user restart podman

# Use podman unshare for ownership
podman unshare chown -R 1011:1011 ${HMAC_BASE}
```

#### **Verification & Testing Commands**

```bash
# Test 1: Verify container can access all paths
podman exec hmac-file-server sh -c '
  echo "=== Container User ==="
  id
  echo "=== Directory Access ==="
  ls -la /app /data /deduplication /logs
  echo "=== Write Test ==="
  touch /data/write-test && echo "✅ Data write: OK" || echo "❌ Data write: FAILED"
  touch /deduplication/dedup-test && echo "✅ Dedup write: OK" || echo "❌ Dedup write: FAILED"  
  touch /logs/log-test && echo "✅ Log write: OK" || echo "❌ Log write: FAILED"
  echo "=== Config Read Test ==="
  head -3 /app/config.toml && echo "✅ Config read: OK" || echo "❌ Config read: FAILED"
'

# Test 2: External health check
curl -f http://localhost:8888/health && echo "✅ HTTP Health: OK" || echo "❌ HTTP Health: FAILED"

# Test 3: Metrics endpoint
curl -s http://localhost:9090/metrics | grep -E "hmac_|go_|process_" | wc -l
# Should return > 0 if metrics are working

# Test 4: File upload simulation (requires auth)
curl -X POST http://localhost:8888/upload \
  -H "Authorization: Bearer your-token" \
  -F "file=@test-file.txt" && echo "✅ Upload: OK" || echo "❌ Upload: FAILED"
```

#### **Advanced: Custom UID/GID Mapping**

If you need different UIDs (e.g., for existing file ownership):

```bash
# Option 1: Rebuild container with custom UID
podman build -t localhost/hmac-file-server:custom \
  --build-arg USER_UID=1500 \
  --build-arg USER_GID=1500 \
  -f dockerenv/podman/Dockerfile.podman .

# Option 2: Use --user flag (may have limitations)
podman run --user 1500:1500 [other options] localhost/hmac-file-server:latest

# Option 3: Host ownership adjustment
sudo chown -R 1500:1500 ${HMAC_BASE}
```

#### **Docker vs Podman Ownership Differences**

| Scenario | Docker | Podman Rootless | Podman Rootful |
|----------|--------|-----------------|----------------|
| **Host UID** | 1011:1011 | 1011:1011 | 1011:1011 |
| **Container UID** | 1011:1011 | 1011:1011 | 1011:1011 |
| **Volume Ownership** | `chown 1011:1011` | `podman unshare chown 1011:1011` | `chown 1011:1011` |
| **SELinux Labels** | `:Z` or `:z` | `:Z` or `:z` | `:Z` or `:z` |

### Podman Deployment Script Features

The `deploy-podman.sh` script provides complete automation:

- **✅ Interactive deployment** with colored output
- **✅ Auto-generates secure configuration** with random HMAC/JWT secrets
- **✅ Security-hardened settings**: `--cap-drop=ALL`, `--read-only`, `--no-new-privileges`
- **✅ Pod management** for XMPP integration
- **✅ Health monitoring** and status reporting
- **✅ Environment variable support** for customization

### Podman Commands Reference

```bash
# Build image
podman build -t localhost/hmac-file-server:latest -f dockerenv/podman/Dockerfile.podman .

# Run with basic settings
podman run -d --name hmac-file-server \
    -p 8888:8888 \
    -v ./config.toml:/app/config.toml:ro \
    -v ./data:/data:rw \
    localhost/hmac-file-server:latest -config /app/config.toml

# Create and manage pods for XMPP integration
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
```

### Podman Systemd Integration

#### User Service (Rootless - Recommended)
```bash
# Copy service file
cp dockerenv/podman/hmac-file-server.service ~/.config/systemd/user/

# Enable and start
systemctl --user daemon-reload
systemctl --user enable hmac-file-server.service
systemctl --user start hmac-file-server.service

# Check status
systemctl --user status hmac-file-server.service
```

#### System Service (Root)
```bash
# Copy service file
sudo cp dockerenv/podman/hmac-file-server.service /etc/systemd/system/

# Enable and start
sudo systemctl daemon-reload
sudo systemctl enable hmac-file-server.service
sudo systemctl start hmac-file-server.service
```

### Podman with XMPP Integration

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

### Deployment Script Commands

```bash
# Management commands
./deploy-podman.sh deploy    # Full deployment (default)
./deploy-podman.sh start     # Start services
./deploy-podman.sh stop      # Stop all services
./deploy-podman.sh restart   # Restart services
./deploy-podman.sh status    # Show service status
./deploy-podman.sh logs      # Show container logs
./deploy-podman.sh config    # Show configuration
./deploy-podman.sh build     # Build container image only
./deploy-podman.sh pod       # Create pod only
./deploy-podman.sh clean     # Remove containers and image
./deploy-podman.sh help      # Show help

# Environment variables
export APP_DATA="/custom/path/hmac-file-server"
export LISTEN_PORT="9999"
export METRICS_PORT="9998"
./deploy-podman.sh
```

### Podman Security Features

#### Container Security
- **Rootless operation**: Runs as non-root user (UID 1011)
- **Capability dropping**: `--cap-drop=ALL`
- **No new privileges**: `--security-opt no-new-privileges`
- **Read-only filesystem**: `--read-only` with tmpfs for /tmp
- **SELinux labels**: Volume mounts with `:Z` labels

#### Network Security
- **Pod isolation**: Containers run in isolated pods
- **Port binding**: Only necessary ports exposed
- **Health monitoring**: Built-in health checks

### Troubleshooting Podman

#### Common Issues

**Permission Errors:**
```bash
# Fix SELinux contexts
restorecon -R /opt/podman/hmac-file-server

# Check volume permissions
podman unshare ls -la /opt/podman/hmac-file-server
```

**Container Won't Start:**
```bash
# Check image exists
podman images | grep hmac-file-server

# Validate configuration
./deploy-podman.sh config

# Debug with interactive container
podman run -it --rm localhost/hmac-file-server:latest /bin/sh
```

**Network Issues:**
```bash
# Check pod networking
podman pod ps
podman port hmac-file-server

# Test connectivity
nc -zv localhost 8888
```

---

## Multi-Architecture Deployment

HMAC File Server 3.4.0 "Cascade" provides comprehensive multi-architecture support for modern deployment scenarios.

### Supported Architectures

#### **AMD64 (x86_64)**
- **Primary Platform**: Optimized for Intel and AMD processors
- **Performance**: Maximum performance optimization
- **Use Cases**: Data centers, cloud instances, desktop deployments
- **Binary**: `hmac-file-server-linux-amd64`

#### **ARM64 (aarch64)**
- **Modern ARM**: Apple Silicon (M1/M2/M3), AWS Graviton, cloud ARM instances
- **Performance**: Native ARM64 optimizations
- **Use Cases**: Cloud-native deployments, Apple Silicon development
- **Binary**: `hmac-file-server-linux-arm64`

#### **ARM32v7 (armhf)**
- **IoT & Edge**: Raspberry Pi, embedded systems, edge computing
- **Efficiency**: Optimized for resource-constrained environments
- **Use Cases**: IoT gateways, edge file servers, embedded applications
- **Binary**: `hmac-file-server-linux-arm32v7`

### Build Commands

```bash
# Interactive Multi-Architecture Builder (NEW in 3.4.0)
./build-multi-arch.sh

# Quick options:
# 1) All supported platforms (recommended)
# 2) Linux only (AMD64, ARM64, ARM32v7)  
# 3) Cross-platform (Linux, macOS, Windows)
# 4) Custom selection
# 5) Quick build (Linux AMD64 only)

# Manual build commands
GOOS=linux GOARCH=amd64 go build -o hmac-file-server-linux-amd64 ./cmd/server/
GOOS=linux GOARCH=arm64 go build -o hmac-file-server-linux-arm64 ./cmd/server/
GOOS=linux GOARCH=arm GOARM=7 go build -o hmac-file-server-linux-arm ./cmd/server/
```

### Docker Multi-Architecture

```bash
# Build multi-platform Docker images (NEW in 3.4.0)
./docker-multiarch-build.sh --local    # Local testing
./docker-multiarch-build.sh --push     # Push to registry

# Manual Docker buildx (advanced)
docker buildx build --platform linux/amd64,linux/arm64,linux/arm/v7 -t hmac-file-server:3.4.0 .

# Run platform-specific image
docker run --platform linux/arm64 hmac-file-server:3.4.0
```

### Architecture-Specific Optimizations

#### **AMD64 Optimizations**
- AVX2/SSE4 utilizations for hash calculations
- Memory prefetching optimizations
- Large file transfer optimizations

#### **ARM64 Optimizations**
- NEON SIMD instructions for crypto operations
- Apple Silicon memory management optimizations
- Energy-efficient processing patterns

#### **ARM32v7 Optimizations**
- Memory-constrained operation modes
- Reduced concurrent workers for stability
- Optimized for flash storage patterns

---

## Network Resilience & Queue Optimization

HMAC File Server 3.4.0 introduces advanced network resilience and queue optimization systems designed for enterprise-grade reliability.

### Network Resilience Features

#### **Connection Recovery**
- **Automatic Reconnection**: Seamless reconnection after network interruptions
- **Retry Logic**: Intelligent exponential backoff for failed operations
- **Timeout Management**: Extended 4800s timeouts prevent premature disconnections
- **Circuit Breaker**: Prevents cascade failures during network issues

#### **Network Switching Support**
- **Interface Detection**: Automatic detection of network interface changes
- **IP Migration**: Seamless handling of IP address changes
- **Connection Pooling**: Maintains connection pools across network changes
- **Health Checks**: Continuous connectivity monitoring

### Queue Optimization Engine

#### **Dynamic Worker Scaling**
- **Optimized Thresholds**: 40% scale-up, 10% scale-down for perfect balance
- **Load Prediction**: Proactive scaling based on historical patterns
- **Memory Management**: Intelligent memory allocation for queue operations
- **Priority Queuing**: Critical operations get processing priority

#### **Queue Intelligence**
- **Bottleneck Prevention**: Automatic queue rebalancing
- **Overflow Protection**: Graceful handling of queue overflow scenarios
- **Performance Analytics**: Real-time queue performance metrics
- **Auto-tuning**: Self-optimizing queue parameters

```toml
# Network resilience configuration
[network]
enable_resilience = true
max_retries = 5
retry_delay = "2s"
connection_timeout = "30s"
keepalive_interval = "60s"

# Queue optimization settings
[queue]
enable_optimization = true
scale_up_threshold = 40    # Scale up at 40% queue capacity
scale_down_threshold = 10  # Scale down at 10% queue capacity
min_workers = 2
max_workers = 16
prediction_window = "5m"
```

### Docker Build

The official Dockerfile supports multi-stage builds for minimal images:

```dockerfile
# Stage 1: Build
FROM golang:1.24-alpine AS builder

WORKDIR /build
RUN apk add --no-cache git
COPY go.mod go.sum ./
RUN go mod download
COPY . .
RUN CGO_ENABLED=0 go build -o hmac-file-server ./cmd/server/

# Stage 2: Runtime
FROM alpine:latest

RUN apk --no-cache add ca-certificates

RUN mkdir -p /opt/hmac-file-server/data/uploads \
    && mkdir -p /opt/hmac-file-server/data/duplicates \
    && mkdir -p /opt/hmac-file-server/data/temp \
    && mkdir -p /opt/hmac-file-server/data/logs

WORKDIR /opt/hmac-file-server

COPY --from=builder /build/hmac-file-server .

EXPOSE 8080

CMD ["./hmac-file-server", "--config", "/etc/hmac-file-server/config.toml"]
```

### Example Docker Config

A sample `config.toml` for Docker deployments:

```toml
[server]
listen_address = ":8080"
storage_path = "/srv/hmac-file-server/uploads"
metrics_enabled = true
metrics_path = "/metrics"
pid_file = "/var/run/hmac-file-server.pid"
max_upload_size = "10GB"
deduplication_enabled = true
min_free_bytes = "1GB"
file_naming = "original"
enable_dynamic_workers = true
worker_scale_up_thresh = 40   # 40% optimized threshold for 3.2
worker_scale_down_thresh = 10

[uploads]
allowed_extensions = [".zip", ".rar", ".7z", ".tar.gz", ".tgz", ".gpg", ".enc", ".pgp"]
chunked_uploads_enabled = true
chunk_size = "10MB"
resumable_uploads_enabled = true
max_resumable_age = "48h"

[downloads]
resumable_downloads_enabled = true
chunked_downloads_enabled = true
chunk_size = "8192"
allowed_extensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"]

[security]
secret = "f6g4ldPvQM7O2UTFeBEUUj33VrXypDAcsDt0yqKrLiOr5oQW"
enablejwt = false
jwtsecret = "f6g4ldPvQM7O2UTFeBEUUj33VrXypDAcsDt0yqKrLiOr5oQW"
jwtalgorithm = "HS256"
jwtexpiration = "24h"

[logging]
level = "info"
file = "/var/log/hmac-file-server.log"
max_size = 100
max_backups = 7
max_age = 30
compress = true

[deduplication]
enabled = true
directory = "./deduplication"
maxsize = "1GB"

[iso]
enabled = true
size = "1GB"
mountpoint = "/mnt/iso"
charset = "utf-8"
containerfile = "/mnt/iso/container.iso"

[timeouts]
readtimeout = "4800s"
writetimeout = "4800s"
idletimeout = "4800s"

[versioning]
enableversioning = false
maxversions = 1

[clamav]
clamavenabled = true
clamavsocket = "/var/run/clamav/clamd.ctl"
numscanworkers = 2
scanfileextensions = [".txt", ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".exe", ".zip", ".rar", ".7z", ".tar", ".gz"]
maxscansize = "200MB"

[redis]
redisenabled = true
redisdbindex = 0
redisaddr = "localhost:6379"
redispassword = ""
redishealthcheckinterval = "120s"

[workers]
numworkers = 4
uploadqueuesize = 50

[file]
# Add file-specific configurations here

[build]
version = "3.4.0"
```

### Quickstart with Docker Compose

1. Place your `config.toml` in the `./config` directory.
2. Run:

```zsh
docker compose up -d
```

3. The server will be available on `http://localhost:8080`.

---

## Simplified Configuration Examples

HMAC File Server 3.4.0 "Cascade" achieves **93% configuration reduction** through intelligent defaults. Here are minimal configurations for common scenarios:

### Minimal Production Configuration (93% Simplified)

```toml
# Minimal config - just 4 lines for full production deployment!
[server]
listen_address = ":8080"
storage_path = "/srv/uploads"
hmac_secret = "your-secret-key-here"
```

This minimal configuration automatically provides:
- ✅ Dynamic worker scaling (40%/10% thresholds)
- ✅ Extended timeouts (4800s)
- ✅ File deduplication
- ✅ Prometheus metrics
- ✅ Network resilience
- ✅ Queue optimization
- ✅ Security hardening

### Quick Development Setup

```toml
# Development - just 2 lines!
[server]
storage_path = "./uploads"
```

### Enterprise Cloud Configuration

```toml
# Enterprise cloud deployment
[server]
listen_address = ":8080"
storage_path = "/data/uploads"
hmac_secret = "${HMAC_SECRET}"
max_upload_size = "50GB"

[monitoring]
prometheus_enabled = true
metrics_port = "9090"
```

### XMPP Integration (XEP-0363)

```toml
# XMPP file sharing server
[server]
storage_path = "/srv/xmpp-uploads"
hmac_secret = "${HMAC_SECRET}"

[xmpp]
enabled = true
max_file_size = "10GB"
```

**Previous versions required 100+ configuration lines - 3.3 "Nexus Infinitum" does it with just a few!**

---
