This documentation provides detailed information on configuring, setting up, and maintaining the HMAC File Server. Whether you're a developer, system administrator, or an enthusiast, this guide will help you navigate through the server's features and configurations effectively.

---

## Table of Contents

1. [Introduction](#introduction)
2. [Configuration](#configuration)
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
3. [Example Configuration](#example-configuration)
4. [Setup Instructions](#setup-instructions)
    - [1. HMAC File Server Installation](#1-hmac-file-server-installation)
    - [2. Reverse Proxy Configuration](#2-reverse-proxy-configuration)
        - [Apache2 Reverse Proxy](#apache2-reverse-proxy)
        - [Nginx Reverse Proxy](#nginx-reverse-proxy)
    - [3. ejabberd Configuration](#3-ejabberd-configuration)
    - [4. Systemd Service Setup](#4-systemd-service-setup)
5. [Running with Docker & Docker Compose](#running-with-docker--docker-compose)
6. [Building for Different Architectures](#building-for-different-architectures)
7. [Additional Recommendations](#additional-recommendations)
8. [Notes](#notes)
9. [Using HMAC File Server for CI/CD Build Artifacts](#using-hmac-file-server-for-ci-cd-build-artifacts)
10. [Monitoring](#monitoring)

---

## Introduction

The **HMAC File Server** is a secure and efficient file management solution designed to handle file uploads, downloads, deduplication, and more. Built with a focus on security, scalability, and performance, it integrates seamlessly with various tools and services to provide a comprehensive file handling experience.

---

## Configuration

The HMAC File Server is configured using a `config.toml` file. Below are the detailed explanations of each configuration section and their respective options.

### Server Configuration

```toml
# Server configuration
listenport = "8080"  # TCP port for incoming requests
unixsocket = false   # Use Unix domain socket instead of TCP
storagepath = "/path/to/hmac-file-server/data/"  # Directory to store uploaded files
loglevel = "debug"   # Logging level: "debug", "info", "warn", "error"
logfile = "/path/to/hmac-file-server.log"  # Path to log file; leave empty to use stdout
metricsenabled = true   # Enable Prometheus metrics
metricsport = "9090"    # Port for Prometheus metrics
deduplicationenabled = true
minfreebytes = "5GB"     # Minimum free disk space required
filettl = "2Y"           # Time-to-live for files
filettlenabled = false   # Enable TTL checks and cleanup
autoadjustworkers = true # Automatically adjust worker threads based on load
networkevents = false    # Enable detailed network event logging
pidfilepath = "./hmac-file-server.pid" # Path to PID file
precaching = true        # Pre-cache file structures on startup

# New option to force network protocol
forceprotocol = "auto"  # Options: "ipv4", "ipv6", "auto"
```

#### Configuration Options

- **listenport**:  
  - *Type*: `String`  
  - *Description*: Specifies the TCP port on which the server listens for incoming requests.  
  - *Default*: `"8080"`
  
- **unixsocket**:  
  - *Type*: `Boolean`  
  - *Description*: Determines whether to use a Unix domain socket instead of a TCP port for communication.  
  - *Default*: `false`
  
- **storagepath**:  
  - *Type*: `String`  
  - *Description*: Defines the directory path where uploaded files are stored. Ensure this path exists and has appropriate permissions.  
  - *Default*: `"/path/to/hmac-file-server/data/"`
  
- **loglevel**:  
  - *Type*: `String`  
  - *Description*: Sets the verbosity level of logs.  
  - *Options*: `"debug"`, `"info"`, `"warn"`, `"error"`  
  - *Default*: `"debug"`
  
- **logfile**:  
  - *Type*: `String`  
  - *Description*: Specifies the file path for logging. If left empty, logs are output to `stdout`.  
  - *Default*: `"/path/to/hmac-file-server.log"`
  
- **metricsenabled**:  
  - *Type*: `Boolean`  
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
  
- **minfreebytes**:  
  - *Type*: `String`  
  - *Description*: Specifies the minimum free disk space required for the server to operate effectively.  
  - *Default*: `"5GB"`
  
- **filettl**:  
  - *Type*: `String`  
  - *Description*: Sets the default Time-to-Live (TTL) for files, determining how long files are retained before deletion.  
  - *Format*: Duration (e.g., `"2Y"` for two years)  
  - *Default*: `"2Y"`
  
- **filettlenabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables TTL checks and automatic file cleanup based on the `filettl` value.  
  - *Default*: `false`
  
- **autoadjustworkers**:  
  - *Type*: `Boolean`  
  - *Description*: Automatically adjusts the number of worker threads based on server load and system resources.  
  - *Default*: `true`
  
- **networkevents**:  
  - *Type*: `Boolean`  
  - *Description*: Enables detailed logging of network events, which can be useful for debugging but may increase log verbosity.  
  - *Default*: `false`
  
- **pidfilepath**:  
  - *Type*: `String`  
  - *Description*: Specifies the file path where the server writes its Process ID (PID) file. This is useful for managing the server process.  
  - *Default*: `"./hmac-file-server.pid"`
  
- **precaching**:  
  - *Type*: `Boolean`  
  - *Description*: Enables pre-caching of file structures on startup to improve access speed and performance.  
  - *Default*: `true`

- **forceprotocol**:  
  - *Type*: `String`  
  - *Description*: Specifies the network protocol to use for server communication.  
    - `"ipv4"`: Forces the server to use IPv4.  
    - `"ipv6"`: Forces the server to use IPv6.  
    - `"auto"`: Uses the system's default behavior (dual-stack).  
  - *Default*: `"auto"`

---

### Deduplication Settings

```toml
# Deduplication settings
[deduplication]
enabled = true
directory = "/path/to/hmac-file-server/deduplication/"  # Path to deduplication metadata store
```

#### Configuration Options

- **enabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables the deduplication feature, which helps in eliminating duplicate files to save storage space.  
  - *Default*: `true`
  
- **directory**:  
  - *Type*: `String`  
  - *Description*: Specifies the directory path where deduplication metadata is stored. Ensure this directory exists and has appropriate permissions.  
  - *Default*: `"/path/to/hmac-file-server/deduplication/"`

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
readtimeout = "3600s"    # Maximum time to read a request
writetimeout = "3600s"   # Maximum time to write a response
idletimeout = "3600s"    # Maximum keep-alive time for idle connections
```

#### Configuration Options

- **readtimeout**:  
  - *Type*: `String`  
  - *Description*: Sets the maximum duration for reading the entire request, including the body.  
  - *Format*: Duration (e.g., `"3600s"` for one hour)  
  - *Default*: `"3600s"`
  
- **writetimeout**:  
  - *Type*: `String`  
  - *Description*: Defines the maximum duration before timing out writes of the response.  
  - *Format*: Duration (e.g., `"3600s"` for one hour)  
  - *Default*: `"3600s"`
  
- **idletimeout**:  
  - *Type*: `String`  
  - *Description*: Specifies the maximum amount of time to wait for the next request when keep-alives are enabled.  
  - *Format*: Duration (e.g., `"3600s"` for one hour)  
  - *Default*: `"3600s"`

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
level = "debug"
file = "/path/to/hmac-file-server.log"
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
  - *Default*: `"debug"`

- **file**:  
  - *Type*: `String`  
  - *Description*: Specifies the file path for logging. If left empty, logs are output to `stdout`.  
  - *Default*: `"/path/to/hmac-file-server.log"`

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
resumableuploadsenabled = false
chunkeduploadsenabled = true
chunksize = "32MB"  # Chunk size for uploads
allowedextensions = [
    ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif",
    ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"
]
```

#### Configuration Options

- **resumableuploadsenabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables support for resumable (chunked) file uploads.  
  - *Default*: `false`
  
- **chunkeduploadsenabled**:  
  - *Type*: `Boolean`  
  - *Description*: Specifically enables or disables chunked uploads.  
  - *Default*: `true`
  
- **chunksize**:  
  - *Type*: `String`  
  - *Description*: Defines the size of each chunk in chunked uploads.  
  - *Format*: Size (e.g., `"32MB"`)  
  - *Default*: `"32MB"`
  
- **allowedextensions**:  
  - *Type*: `Array of Strings`  
  - *Description*: Lists the file extensions permitted for upload.  
  - *Default*:
    ```toml
    allowedextensions = [
        ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif",
        ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"
    ]
    ```

---

### Downloads Configuration

```toml
# Downloads settings
[downloads]
resumabledownloadsenabled = false
chunkeddownloadsenabled = true
chunksize = "32MB"
```

#### Configuration Options

- **resumabledownloadsenabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables support for resumable (chunked) downloads.  
  - *Default*: `false`
  
- **chunkeddownloadsenabled**:  
  - *Type*: `Boolean`  
  - *Description*: Specifically enables or disables chunked downloads.  
  - *Default*: `true`
  
- **chunksize**:  
  - *Type*: `String`  
  - *Description*: Defines the size of each chunk in chunked downloads.  
  - *Format*: Size (e.g., `"32MB"`)  
  - *Default*: `"32MB"`

> **Note**: Downloads inherit allowed extensions from the uploads configuration. There is no separate `allowedextensions` setting for downloads.

---

### ClamAV Settings

```toml
# ClamAV settings
[clamav]
clamavenabled = true
clamavsocket = "/path/to/clamav/clamd.ctl"  # Path to ClamAV socket
numscanworkers = 4  # Number of concurrent scan workers
scanfileextensions = [
    ".exe", ".dll", ".bin", ".com", ".bat",
    ".sh", ".php", ".js"
]
```

#### Configuration Options

- **clamavenabled**:  
  - *Type*: `Boolean`  
  - *Description*: Enables or disables ClamAV integration for virus scanning of uploaded files.  
  - *Default*: `true`
  
- **clamavsocket**:  
  - *Type*: `String`  
  - *Description*: Specifies the file path to the ClamAV socket (`.ctl` file). Ensure ClamAV is installed and the socket path is correct.  
  - *Default*: `"/path/to/clamav/clamd.ctl"`
  
- **numscanworkers**:  
  - *Type*: `Integer`  
  - *Description*: Sets the number of concurrent workers dedicated to scanning files with ClamAV.  
  - *Default*: `4`
  
- **scanfileextensions**:  
  - *Type*: `Array of Strings`  
  - *Description*: Lists the file extensions that should be scanned for viruses.  
  - *Default*:
    ```toml
    scanfileextensions = [
        ".exe", ".dll", ".bin", ".com", ".bat",
        ".sh", ".php", ".js"
    ]
    ```

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
numworkers = 10              # Number of worker threads
uploadqueuesize = 5000       # Size of upload queue
```

#### Configuration Options

- **numworkers**:  
  - *Type*: `Integer`  
  - *Description*: Specifies the number of worker threads to handle file operations.  
  - *Default*: `10`

- **uploadqueuesize**:  
  - *Type*: `Integer`  
  - *Description*: Sets the size of the upload queue buffer.  
  - *Default*: `5000`

---

#### Configuration Options

- **maxfilesize**:  
  - *Type*: `String`  
  - *Description*: Defines the maximum allowed file size for uploads.  
  - *Format*: Size (e.g., `"10GB"`)  
  - *Default*: `"10GB"`

---

## Configuration Validation

The HMAC File Server v3.2 includes a comprehensive configuration validation system with specialized command-line flags for different validation scenarios.

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

The enhanced command-line validation system provides comprehensive coverage with 50+ validation checks across all configuration areas, making HMAC File Server v3.2 production-ready with enterprise-grade configuration management.

---

## Example Configuration

Below is an example `config.toml` file with default settings:

```toml
# Example HMAC File Server configuration

# Server configuration
listenport = "8080"
bind_ip = "0.0.0.0"
unixsocket = false
storagepath = "/path/to/hmac-file-server/data/"
metricsenabled = true
metricsport = "9090"
deduplicationenabled = true
minfreebytes = "5GB"
filettl = "2Y"
filettlenabled = false
autoadjustworkers = true
networkevents = false
pidfilepath = "./hmac-file-server.pid"
precaching = true
filenaming = "HMAC"
forceprotocol = "auto"

# Logging settings
[logging]
level = "debug"
file = "/path/to/hmac-file-server.log"
max_size = 100
max_backups = 7
max_age = 30
compress = true

# Deduplication settings
[deduplication]
enabled = true
directory = "/path/to/hmac-file-server/deduplication/"

# ISO settings
[iso]
enabled = false
size = "1TB"
mountpoint = "/path/to/hmac-file-server/iso/"
charset = "utf-8"

# Timeout settings
[timeouts]
readtimeout = "3600s"
writetimeout = "3600s"
idletimeout = "3600s"

# Security settings
[security]
secret = "your-secure-secret-key"
enablejwt = false
jwtsecret = "your-jwt-secret"
jwtalgorithm = "HS256"
jwtexpiration = "24h"

# Versioning settings
[versioning]
enableversioning = false
maxversions = 1

# Upload settings
[uploads]
resumableuploadsenabled = false
chunkeduploadsenabled = true
chunksize = "32MB"
allowedextensions = [
    ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif",
    ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4",
    ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm",
    ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"
]

# Download settings
[downloads]
resumabledownloadsenabled = false
chunkeddownloadsenabled = true
chunksize = "32MB"

# ClamAV settings
[clamav]
clamavenabled = true
clamavsocket = "/path/to/clamav/clamd.ctl"
numscanworkers = 4
scanfileextensions = [
    ".exe", ".dll", ".bin", ".com", ".bat",
    ".sh", ".php", ".js"
]

# Redis settings
[redis]
redisenabled = true
redisdbindex = 0
redisaddr = "localhost:6379"
redispassword = ""
redishealthcheckinterval = "120s"

# Workers settings
[workers]
numworkers = 10
uploadqueuesize = 5000
```

---

## Setup Instructions

### 1. HMAC File Server Installation

To install the HMAC File Server, follow these steps:

1. Clone the repository:
    ```sh
    git clone https://github.com/PlusOne/hmac-file-server.git
    cd hmac-file-server
    ```

2. Build the server:
    ```sh
    go build -o hmac-file-server ./cmd/server/main.go
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

- **Timeouts**: Set reasonable timeouts (e.g., `proxy_read_timeout 300;` in Nginx) to avoid hanging connections.
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

### Docker Compose Example

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
RUN CGO_ENABLED=0 go build -o hmac-file-server ./cmd/server/main.go

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
listenport = "8080"
unixsocket = false
storagepath = "/opt/hmac-file-server/data/uploads"
metricsenabled = true
metricsport = "9090"
deduplicationenabled = true
minfreebytes = "5GB"
filettl = "2y"
filettlenabled = false
autoadjustworkers = true
networkevents = false
pidfilepath = "./hmac-file-server.pid"
precaching = false

[deduplication]
enabled = true
directory = "/opt/hmac-file-server/data/duplicates"

[logging]
level = "debug"
file = "./hmac-file-server.log"
max_size = 100
max_backups = 7
max_age = 30
compress = true

[iso]
enabled = false
size = "1TB"
mountpoint = "/mnt/nfs_vol01/hmac-file-server/iso/"
charset = "utf-8"

[timeouts]
readtimeout = "3600s"
writetimeout = "3600s"
idletimeout = "3600s"

[security]
secret = "hmac-file-server-is-the-win"

[versioning]
enableversioning = false
maxversions = 1

[uploads]
resumableuploadsenabled = false
chunkeduploadsenabled = true
chunksize = "32MB"
allowedextensions = [
  ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp",
  ".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg",
  ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"
]

[downloads]
chunkeddownloadsenabled = false
chunksize = "32MB"
allowedextensions = [
  ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp",
  ".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg",
  ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"
]

[clamav]
clamavenabled = false
clamavsocket = "/var/run/clamav/clamd.ctl"
numscanworkers = 4
scanfileextensions = [".exe", ".dll", ".bin", ".com", ".bat", ".sh", ".php", ".js"]

[redis]
redisenabled = false
redisdbindex = 0
redisaddr = "localhost:6379"
redispassword = ""
redishealthcheckinterval = "120s"

[workers]
numworkers = 4
uploadqueuesize = 5000

[file]
filerevision = 1
```

### Quickstart with Docker Compose

1. Place your `config.toml` in the `./config` directory.
2. Run:

```zsh
docker compose up -d
```

3. The server will be available on `http://localhost:8080`.

---
