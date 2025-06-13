# HMAC File Server 3.2

## Overview
The **HMAC File Server** ensures secure file uploads and downloads using HMAC authentication and JWT tokens. It incorporates comprehensive security features, file versioning, deduplication, ISO container support, virus scanning, and Unix socket support for enhanced flexibility. Redis integration provides efficient caching and session management. Prometheus metrics and graceful shutdown mechanisms ensure reliable and efficient file handling.

Special thanks to **Thomas Leister** for inspiration drawn from [prosody-filer](https://git.uuxo.net/uuxo/prosody-filer).

## Features
- **Multiple Authentication Methods**: HMAC-based authentication and JWT token support
- **Multiple Protocol Support**: v1 (legacy), v2, v3 (mod_http_upload_external), and token-based uploads
- **File Management**: Deduplication, configurable TTL for automatic file cleanup
- **Upload Methods**: POST multipart uploads, PUT uploads for legacy protocols, v3 protocol support
- **Security**: Virus scanning via ClamAV, configurable file extensions validation
- **Performance**: Chunked uploads and downloads, worker pool management with auto-scaling
- **Storage Options**: Local storage, ISO container mounting for specialized needs
- **Monitoring**: Prometheus metrics integration with detailed system and operation metrics
- **Network Support**: IPv4/IPv6 dual-stack support with protocol forcing options
- **Configuration**: Hot-reloading of logging settings via SIGHUP signal

## Table of Contents
1. [Installation](#installation)
2. [Configuration](#configuration)
3. [Authentication](#authentication)
4. [API Endpoints](#api-endpoints)
5. [Usage Examples](#usage-examples)
6. [Setup](#setup)
   - [Reverse Proxy](#reverse-proxy)
   - [Systemd Service](#systemd-service)
7. [Building](#building)
8. [Docker Support](#docker-support)
9. [Changelog](#changelog)
10. [License](#license)

---

## Installation

### Quick Installation for XMPP Operators

The easiest way to install HMAC File Server is using the automated installer:

```bash
git clone https://git.uuxo.net/uuxo/hmac-file-server.git
cd hmac-file-server
sudo ./installer.sh
```

The installer supports two deployment options:
- **Native Installation**: Traditional systemd service with Go build
- **Docker Deployment**: Containerized deployment with docker-compose

**Native installation** features:
- Install Go 1.24 (if needed)
- Create system user and directories
- Build and configure the server
- Set up systemd service
- Optionally install Redis and ClamAV

**Docker deployment** features:
- Complete docker-compose setup with Redis and ClamAV
- Multi-stage Dockerfile for optimized builds
- Automated container orchestration
- Easy start/stop scripts

For detailed installation instructions, see [INSTALL.MD](INSTALL.MD).

### Manual Installation

> **Tip:** The automated installer now supports both native systemd installation and Docker deployment. For automated Docker setup, run `sudo ./installer.sh` and select option 2. For manual Docker setup, see the Wiki for detailed instructions. The official image is available at `ghcr.io/plusone/hmac-file-server:latest`.

#### Prerequisites
- Go **1.24** or higher
- Redis server (optional, for caching)
- ClamAV (optional, for virus scanning)

#### Steps
1. Clone the repository:
   ```bash
   git clone https://git.uuxo.net/uuxo/hmac-file-server.git
   cd hmac-file-server
   ```

2. Build the server:
   ```bash
   go build -o hmac-file-server ./cmd/server/main.go
   ```

3. Generate example configuration:
   ```bash
   ./hmac-file-server -genconfig
   # or write to file:
   ./hmac-file-server -genconfig-path config.toml
   ```

4. Create necessary directories:
   ```bash
   mkdir -p /path/to/hmac-file-server/data/
   mkdir -p /path/to/hmac-file-server/deduplication/
   ```

5. Edit your `config.toml` file with appropriate settings.

6. Start the server:
   ```bash
   ./hmac-file-server -config config.toml
   ```

---

## Uninstallation

The installer script provides comprehensive uninstallation options with data preservation:

```bash
sudo ./installer.sh --uninstall
```

### Uninstall Options

The uninstaller offers five data handling options:

1. **🗑️ Delete all data** - Complete removal (requires typing "DELETE" to confirm)
2. **💾 Preserve uploads and deduplication data** - Keeps important files, removes logs
3. **📋 Preserve all data** - Keeps uploads, deduplication data, and logs
4. **🎯 Custom selection** - Choose exactly what to preserve
5. **❌ Cancel** - Exit without making changes

### Data Preservation

When preserving data, the uninstaller:
- Creates timestamped backups in `/var/backups/hmac-file-server-YYYYMMDD-HHMMSS/`
- Shows file counts and sizes before deletion decisions
- Safely moves data to backup locations
- Provides clear feedback on what was preserved or removed

### What Gets Removed

The uninstaller always removes:
- ✓ Systemd service and service file
- ✓ Installation directory (`/opt/hmac-file-server`)
- ✓ Configuration directory (`/etc/hmac-file-server`)
- ✓ System user (`hmac-server`)
- ✓ Binary files in common locations

Data directories are handled according to your selection.

---

## Configuration

The server uses a comprehensive `config.toml` file with the following main sections:

### Key Configuration Sections

- **[server]**: Basic server settings (port, storage, metrics)
- **[security]**: HMAC secrets, JWT configuration
- **[uploads/downloads]**: File handling settings, allowed extensions
- **[logging]**: Log levels, file rotation settings
- **[clamav]**: Virus scanning configuration
- **[redis]**: Cache and session management
- **[workers]**: Thread pool and performance tuning
- **[iso]**: ISO container mounting (specialized storage)
- **[timeouts]**: HTTP timeout configurations

### Example Configuration

```toml
[server]
bind_ip = "0.0.0.0"
listenport = "8080"
unixsocket = false
storagepath = "./uploads"
metricsenabled = true
metricsport = "9090"
deduplicationenabled = true
filenaming = "HMAC"  # Options: "HMAC", "original", "None"
forceprotocol = "auto"  # Options: "ipv4", "ipv6", "auto"

[security]
secret = "your-secure-hmac-secret"
enablejwt = false
jwtsecret = "your-jwt-secret"
jwtalgorithm = "HS256"
jwtexpiration = "24h"

[uploads]
allowedextensions = [".txt", ".pdf", ".jpg", ".png", ".zip"]
chunkeduploadsenabled = true
chunksize = "10MB"
```

For complete configuration details, see the [Wiki](./WIKI.MD).

---

## Authentication

The server supports multiple authentication methods:

### 1. HMAC Authentication (Default)
- **Legacy v1**: Basic HMAC with path + content length
- **v2**: Enhanced HMAC with content type validation
- **Token**: Alternative HMAC parameter name
- **v3**: mod_http_upload_external compatible with expiration

### 2. JWT Authentication
When `enablejwt = true`:
- Bearer tokens in Authorization header
- Query parameter `token` as fallback
- Configurable expiration and algorithm

### Authentication Examples

```bash
# HMAC v2 upload
curl -X PUT "http://localhost:8080/myfile.txt?v2=HMAC_SIGNATURE" -d @file.txt

# JWT upload
curl -X POST -H "Authorization: Bearer JWT_TOKEN" -F 'file=@myfile.txt' http://localhost:8080/upload

# v3 protocol (mod_http_upload_external)
curl -X PUT "http://localhost:8080/upload/file.txt?v3=SIGNATURE&expires=TIMESTAMP" -d @file.txt
```

---

## API Endpoints

### Upload Endpoints
- **POST /upload**: Multipart form uploads (modern clients)
- **PUT /{filename}**: Direct uploads with HMAC or JWT authentication
- **PUT with v3 protocol**: mod_http_upload_external compatible uploads

### Download Endpoints  
- **GET /{filename}**: Direct file downloads
- **HEAD /{filename}**: File metadata (size, type)
- **GET /download/{filename}**: Alternative download endpoint

### Management Endpoints
- **GET /health**: Health check endpoint for monitoring
- **GET /metrics**: Prometheus metrics (if enabled)
- **Various helper endpoints**: Defined in router setup

---

## Usage Examples

### Upload Examples

#### Multipart POST Upload
```bash
curl -X POST -F 'file=@example.jpg' \
  -H "X-Signature: HMAC_OF_PATH" \
  http://localhost:8080/upload
```

#### Legacy PUT Upload (v2)
```bash
# Calculate HMAC of: filename + "\x00" + content_length + "\x00" + content_type
curl -X PUT "http://localhost:8080/example.jpg?v2=CALCULATED_HMAC" \
  --data-binary @example.jpg
```

#### v3 Protocol Upload (mod_http_upload_external)
```bash
# HMAC of: "PUT\n{timestamp}\n/path/to/file"
curl -X PUT "http://localhost:8080/upload/file.txt?v3=SIGNATURE&expires=1234567890" \
  --data-binary @file.txt
```

#### JWT Upload
```bash
curl -X POST \
  -H "Authorization: Bearer YOUR_JWT_TOKEN" \
  -F 'file=@example.jpg' \
  http://localhost:8080/upload
```

### Download Examples

#### Direct Download
```bash
curl http://localhost:8080/example.jpg -o downloaded_file.jpg
```

#### Get File Info
```bash
curl -I http://localhost:8080/example.jpg
```

#### Health Check
```bash
curl http://localhost:8080/health
```

---

## Setup

### Reverse Proxy

#### Nginx Configuration
```nginx
server {
    listen 80;
    server_name your-domain.com;
    client_max_body_size 10G;  # Important for large uploads

    location / {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # Timeout settings for large uploads
        proxy_read_timeout 300;
        proxy_connect_timeout 60;
        proxy_send_timeout 300;
    }
}
```

#### Apache2 Configuration
```apache
<VirtualHost *:80>
    ServerName your-domain.com
    
    ProxyPreserveHost On
    ProxyPass / http://localhost:8080/
    ProxyPassReverse / http://localhost:8080/
    
    # Large upload support
    LimitRequestBody 10737418240  # 10GB
    ProxyTimeout 300
</VirtualHost>
```

### Systemd Service

```ini
[Unit]
Description=HMAC File Server
After=network.target redis.service

[Service]
Type=simple
ExecStart=/path/to/hmac-file-server -config /path/to/config.toml
ExecReload=/bin/kill -SIGHUP $MAINPID
WorkingDirectory=/path/to/hmac-file-server
Restart=always
RestartSec=10
User=hmac-server
Group=hmac-server

# Security settings
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ReadWritePaths=/path/to/uploads /path/to/logs

[Install]
WantedBy=multi-user.target
```

Enable and start:
```bash
sudo systemctl daemon-reload
sudo systemctl enable hmac-file-server
sudo systemctl start hmac-file-server

# Reload configuration (logging settings)
sudo systemctl reload hmac-file-server
```

---

## Building

### Local Build
```bash
go build -o hmac-file-server ./cmd/server/main.go
```

### Cross-Platform Builds
```bash
# Linux amd64
GOOS=linux GOARCH=amd64 go build -o hmac-file-server-linux-amd64 ./cmd/server/main.go

# Linux arm64
GOOS=linux GOARCH=arm64 go build -o hmac-file-server-linux-arm64 ./cmd/server/main.go

# Windows
GOOS=windows GOARCH=amd64 go build -o hmac-file-server-windows-amd64.exe ./cmd/server/main.go
```

---

## Docker Support

### Quick Start with Docker Compose
```yaml
version: '3.8'
services:
  hmac-file-server:
    image: ghcr.io/plusone/hmac-file-server:latest
    ports:
      - "8080:8080"
      - "9090:9090"  # Metrics
    volumes:
      - ./config:/etc/hmac-file-server
      - ./uploads:/opt/hmac-file-server/data/uploads
    environment:
      - CONFIG_PATH=/etc/hmac-file-server/config.toml
    restart: unless-stopped
```

### Docker Build
```bash
docker build -t hmac-file-server .
docker run -p 8080:8080 -v $(pwd)/config.toml:/etc/hmac-file-server/config.toml hmac-file-server
```

See the Wiki for detailed Docker setup instructions.

---

## Changelog

### Version 3.2 (Stable)
- **Development Version**: Active development branch with latest features
- **Enhanced Documentation**: Updated comprehensive documentation and protocol specifications
- **Configuration Improvements**: Better configuration validation and structure
- **Authentication System**: Full JWT and multi-protocol HMAC support

### Version 3.1-Stable (2025-06-08)
- **v3 Protocol Support**: Added mod_http_upload_external compatibility
- **Enhanced Authentication**: Improved HMAC validation with multiple protocol support
- **JWT Integration**: Complete JWT authentication system
- **File Naming Options**: HMAC-based or original filename preservation
- **Network Improvements**: IPv4/IPv6 dual-stack with protocol forcing

### Version 3.0-Stable (2025-06-07)
- **Docker Support**: Official Docker images and compose files
- **Go 1.24 Requirement**: Updated minimum Go version
- **Configuration Improvements**: Better validation and hot-reloading
- **Performance Enhancements**: Worker auto-scaling and memory optimization

### Previous Versions
See [CHANGELOG.MD](./CHANGELOG.MD) for complete version history.

---

## License

Apache License 2.0

Copyright (c) 2025 Alexander Renz

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
