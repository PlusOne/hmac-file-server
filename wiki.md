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
5. [Building for Different Architectures](#building-for-different-architectures)
6. [Additional Recommendations](#additional-recommendations)
7. [Notes](#notes)
8. [Using HMAC File Server for CI/CD Build Artifacts](#using-hmac-file-server-for-ci-cd-build-artifacts)
9. [Monitoring](#monitoring)

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

### Security Settings

```toml
# Security settings
[security]
secret = "your-secure-secret-key"  # HMAC shared secret key (change to a secure value)
```

#### Configuration Options

- **secret**:  
  - *Type*: `String`  
  - *Description*: The HMAC shared secret key used for signing requests and operations.  
  - *Default*: `"your-secure-secret-key"`  
  - *Warning*: **Change this immediately** to a unique, strong string in production environments to ensure the security of HMAC operations.

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

### Uploads Settings

```toml
# Upload settings
[uploads]
resumableuploadsenabled = false
chunkeduploadsenabled = true
chunksize = "32MB"  # Chunk size for uploads
allowedextensions = [
    ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif",
    ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4",
    ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm",
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
        ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4",
        ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm",
        ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"
    ]
    ```

---

### Downloads Settings

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

> **Note**: The `allowedextensions` key is **not** part of the `[downloads]` configuration based on the provided code. Ensure that it is omitted to prevent configuration errors.

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

### Worker Settings

```toml
# Worker settings
[worker]
numworkers = 10  # Number of worker threads
```

#### Configuration Options

- **numworkers**:  
  - *Type*: `Integer`  
  - *Description*: Specifies the number of worker threads to handle file operations.  
  - *Default*: `10`

---

#### Configuration Options

- **maxfilesize**:  
  - *Type*: `String`  
  - *Description*: Defines the maximum allowed file size for uploads.  
  - *Format*: Size (e.g., `"10GB"`)  
  - *Default*: `"10GB"`

---

## Example Configuration

Below is an example `config.toml` file with default settings:

```toml
# Example HMAC File Server configuration

# Server configuration
listenport = "8080"
unixsocket = false
storagepath = "/path/to/hmac-file-server/data/"
loglevel = "debug"
logfile = "/path/to/hmac-file-server.log"
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
forceprotocol = "auto"

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

# Worker settings
[worker]
numworkers = 10
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
    go build -o hmac-file-server
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

## Building for Different Architectures

To build the HMAC File Server for different architectures, you can use the following commands:

### Building for Linux (x86_64)

```sh
GOOS=linux GOARCH=amd64 go build -o hmac-file-server-linux-amd64
```

### Building for ARM (32-bit)

```sh
GOOS=linux GOARCH=arm GOARM=7 go build -o hmac-file-server-linux-arm
```

### Building for ARM (64-bit)

```sh
GOOS=linux GOARCH=arm64 go build -o hmac-file-server-linux-arm64
```

### Building the Monitoring Tool

The monitoring tool (`monitor.go`) is located in the `server/cmd/monitor/` directory and is compiled separately from the main HMAC File Server. Below are the instructions for building the monitoring tool:

#### Building for Linux (x86_64)

```sh
GOOS=linux GOARCH=amd64 go build -o monitor-linux-amd64 ./server/cmd/monitor/monitor.go
```

#### Building for ARM (32-bit)

```sh
GOOS=linux GOARCH=arm GOARM=7 go build -o monitor-linux-arm ./server/cmd/monitor/monitor.go
```

#### Building for ARM (64-bit)

```sh
GOOS=linux GOARCH=arm64 go build -o monitor-linux-arm64 ./server/cmd/monitor/monitor.go
```

Once built, the monitoring tool can be executed independently to track system performance, Prometheus metrics, and active processes.

---

## Additional Recommendations

- **Security**: Ensure that the `secret` key in the `config.toml` file is changed to a unique, strong value to secure HMAC operations.
- **Backups**: Regularly back up the `config.toml` file and any important data stored by the HMAC File Server.
- **Monitoring**: Use monitoring tools like Prometheus and Grafana to keep track of server performance and metrics.

---

## Notes

- The HMAC File Server is designed to be flexible and configurable. Adjust the settings in the `config.toml` file to match your specific requirements and environment.
- For any issues or questions, refer to the project's GitHub repository and documentation.

## Using HMAC File Server for CI/CD Build Artifacts

This guide explains how to use [HMAC File Server](https://github.com/PlusOne/hmac-file-server) to securely upload and download build artifacts in CI/CD pipelines.

---

## Why Use HMAC File Server?

- Secure, HMAC-authenticated access  
- Self-hosted, no third-party storage needed  
- Configurable TTL, versioning, and deduplication  
- Prometheus metrics for monitoring  
- Easily integrated into GitHub Actions, GitLab CI, Jenkins, etc.

---

## Step 1: Set Up HMAC File Server

Clone and build the server:

```bash
git clone https://github.com/PlusOne/hmac-file-server.git
cd hmac-file-server
go build -o hmac-file-server
cp config.example.toml config.toml
mkdir -p /data/artifacts
./hmac-file-server -config config.toml
```

Update `config.toml` with:

```toml
[hmac]
secret = "your-secret-key"

[upload]
enabled = true
path = "/data/artifacts"

[download]
enabled = true
```

---

## Step 2: Generate Signed URLs

Use HMAC to generate signed URLs for secure upload/download.

### Upload Script

```bash
#!/bin/bash

FILE_PATH="./build/output.tar.gz"
FILENAME="output.tar.gz"
SECRET="your-secret-key"
BASE_URL="https://your-hmac-server.com"

TIMESTAMP=$(date +%s)
SIGNATURE=$(echo -n "$FILENAME$TIMESTAMP" | openssl dgst -sha256 -hmac "$SECRET" | sed 's/^.* //')

curl -X PUT "$BASE_URL/upload/$FILENAME?ts=$TIMESTAMP&sig=$SIGNATURE" --data-binary "@$FILE_PATH"
```

### Download Script

```bash
#!/bin/bash

FILENAME="output.tar.gz"
SECRET="your-secret-key"
BASE_URL="https://your-hmac-server.com"

TIMESTAMP=$(date +%s)
SIGNATURE=$(echo -n "$FILENAME$TIMESTAMP" | openssl dgst -sha256 -hmac "$SECRET" | sed 's/^.* //')

curl -O "$BASE_URL/download/$FILENAME?ts=$TIMESTAMP&sig=$SIGNATURE"
```

---

## Step 3: Integrate into CI/CD

### GitHub Actions Example

```yaml
name: Build and Upload to HMAC

on:
  push:
    branches: [ "main" ]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Build
        run: |
          mkdir -p build
          echo "example artifact content" > build/output.tar.gz

      - name: Upload Artifact to HMAC Server
        run: bash scripts/upload-artifact.sh
```

---

## Optional Features

- **TTL**  
  Auto-delete artifacts after a set time.  
- **Deduplication**  
  Only store unique files.  
- **Versioning**  
  Track changes to files over time.  
- **Virus Scanning**  
  Integrate with ClamAV to scan uploaded files.

---

## Monitoring

The HMAC File Server provides a built-in monitoring interface to track system performance, Prometheus metrics, and active processes. Below is an overview of the monitoring features:

### System Data

The monitoring interface displays key system metrics, including:

- **CPU Usage**: Current CPU usage percentage.
- **Memory Usage**: Current memory usage percentage.
- **CPU Cores**: Number of CPU cores available.

### Prometheus Metrics

The server exposes Prometheus metrics for tracking upload and download statistics:
- **hmac_file_server_upload_errors_total**: Total number of upload errors.
- **hmac_file_server_uploads_total**: Total number of successful uploads.
- **hmac_file_server_downloads_total**: Total number of successful downloads.

These metrics can be integrated with Prometheus and visualized using tools like Grafana.

### Process List

The monitoring interface also provides a list of active processes, including:
- Process ID (PID)
- CPU usage percentage
- Memory usage percentage
- Command or service name

This information helps in identifying resource-intensive processes and debugging performance issues.

### Example Monitoring Output

Below is an example of the monitoring interface output:

```
System Data
Metric                Value
CPU Usage             2.78%
Memory Usage          26.49%
CPU Cores             4

Prometheus Metrics
hmac_file_server_upload_errors_total   1.00
hmac_file_server_uploads_total         4.00
hmac_file_server_downloads_total       15.00

Process List
PID       CPU   MEM   COMMAND
907752    0.12  2.69  /lib/systemd/systemd-journald
4055132   0.12  0.03  /usr/sbin/qemu-ga
2370782   0.11  0.00  kworker/0:2-wg-crypt-wg1
2371119   0.10  0.08  bash
2371096   0.10  0.14  sshd: root@pts/0
2369170   0.09  0.00  kworker/0:0-mm_percpu_wq
2371240   0.07  0.00  kworker/0:1-wg-crypt-wg1
2371099   0.06  0.13  systemd --user
868714    0.05  0.59  php-fpm: pool www
```

For more details on integrating Prometheus metrics, refer to the [Prometheus documentation](https://prometheus.io/docs/).

---

## License

HMAC File Server is open-source and MIT licensed.

---

## Resources

- [HMAC File Server GitHub Repo](https://github.com/PlusOne/hmac-file-server)
- [Configuration Docs](https://github.com/PlusOne/hmac-file-server/wiki)

## Version 3.0 Release Note

Version 2.8 is the last release before we begin integrating additional features and focusing on further stability patches.

## CI/CD with HMAC File Server – Summary

Sure! Here is a brief guide on how to use the HMAC File Server in your CI/CD pipeline:

---

### 1. Server Setup

```bash
git clone https://github.com/PlusOne/hmac-file-server.git
cd hmac-file-server
go build -o hmac-file-server
cp config.example.toml config.toml
mkdir -p /data/artifacts
./hmac-file-server -config config.toml
```

Update config.toml:
```toml
[hmac]
secret = "your-secret-key"

[upload]
enabled = true
path = "/data/artifacts"

[download]
enabled = true
```

---

### 2. Upload & Download with HMAC

#### Upload Script

```bash
FILE="output.tar.gz"
TS=$(date +%s)
SIG=$(echo -n "$FILE$TS" | openssl dgst -sha256 -hmac "$SECRET" | sed 's/^.* //')
curl -X PUT "$URL/upload/$FILE?ts=$TS&sig=$SIG" --data-binary "@build/$FILE"
```

#### Download Script

```bash
TS=$(date +%s)
SIG=$(echo -n "$FILE$TS" | openssl dgst -sha256 -hmac "$SECRET" | sed 's/^.* //')
curl -O "$URL/download/$FILE?ts=$TS&sig=$SIG"
```

---

### 3. Using in CI/CD (GitHub Actions)

```yaml
- name: Build
  run: |
    mkdir -p build
    echo "artifact" > build/output.tar.gz

- name: Upload
  env:
    SECRET: ${{ secrets.HMAC_SECRET }}
  run: bash scripts/upload.sh
```

---

### Advantages

- Secure (HMAC)
- Self-hosted
- Easy to integrate
- No dependencies on third-party providers
