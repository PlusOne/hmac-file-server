# HMAC File Server 2.9-Stable
## Overview
The **HMAC File Server** ensures secure file uploads and downloads using HMAC authentication. It incorporates rate limiting, CORS support, retries, file versioning, and Unix socket support for enhanced flexibility. Redis integration provides efficient caching and session management. Prometheus metrics and a graceful shutdown mechanism ensure reliable and efficient file handling.

Special thanks to **Thomas Leister** for inspiration drawn from [[prosody-filer](https://github.com/ThomasLeister/prosody-filer)](https://github.com/ThomasLeister/prosody-filer).

## Features
- File deduplication
- Configurable TTL for automatic file cleanup
- Secure HMAC-based authentication
- Chunked uploads and downloads
- Virus scanning via ClamAV
- Prometheus metrics integration
- Customizable worker management
- Support ISO-based storage

## Table of Contents
1. [Installation](#installation)
2. [Configuration](#configuration)
3. [Usage](#usage)
4. [Setup](#setup)
   - [Reverse Proxy](#reverse-proxy)
   - [Systemd Service](#systemd-service)
5. [Building](#building)
6. [Changelog](#changelog)
7. [License](#license)

---

## Installation

### Prerequisites
- Go **1.20** or higher
- Redis server (optional, for caching)
- ClamAV (optional, for virus scanning)

### Steps
1. Clone the repository:
   ```bash
   git clone https://github.com/PlusOne/hmac-file-server.git
   cd hmac-file-server
   ```

2. Build the server:
   ```bash
   go build -o hmac-file-server
   ```

3. Create necessary directories:
   ```bash
   mkdir -p /path/to/hmac-file-server/data/
   mkdir -p /path/to/hmac-file-server/deduplication/
   mkdir -p /path/to/hmac-file-server/iso/
   ```

4. Copy and edit the configuration file:
   ```bash
   cp config.example.toml config.toml
   ```

5. Start the server:
   ```bash
   ./hmac-file-server -config config.toml
   ```

---

## Configuration
The server is configured via a `config.toml` file. Key settings include:

- **Server Settings**: Port, logging, metrics
- **Security**: HMAC secret, TLS options
- **File Management**: TTL, deduplication, uploads, and downloads
- **ISO**: Generation and mounting settings
- **Workers**: Adjust thread management

For detailed configuration options, refer to the [Wiki](./wiki.md).

---

## Usage
Start the server and access it on the configured port. Use curl or a client library to interact with the API.

### Example
Upload a file:
```bash
curl -X POST -F 'file=@example.jpg' http://localhost:8080/upload
```

---

## Setup

### Reverse Proxy
Set up a reverse proxy using Apache2 or Nginx to handle requests.

#### Apache2 Example
```apache
<VirtualHost *:80>
    ServerName your-domain.com

    ProxyPreserveHost On
    ProxyPass / http://localhost:8080/
    ProxyPassReverse / http://localhost:8080/
</VirtualHost>
```

#### Nginx Example
```nginx
server {
    listen 80;
    server_name your-domain.com;

    location / {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

### Systemd Service
Create a systemd service file for the HMAC File Server:
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
Enable and start the service:
```bash
sudo systemctl daemon-reload
sudo systemctl enable hmac-file-server
sudo systemctl start hmac-file-server
```

---

## Building
To build for different architectures:

- **Linux (amd64)**:
  ```bash
  GOOS=linux GOARCH=amd64 go build -o hmac-file-server main.go
  ```

- **Linux (arm64)**:
  ```bash
  GOOS=linux GOARCH=arm64 go build -o hmac-file-server main.go
  ```

---

## Changelog

### Added
- **Deduplication Support:** Automatically remove duplicate files based on SHA256 hashing to save storage space.
- **ISO Container Management:** Create and mount ISO containers for specialized storage needs, enhancing flexibility in file management.
- **Prometheus Metrics Enhancements:** Added detailed metrics for deduplication and ISO container operations to improve monitoring and observability.
- **Redis Integration Improvements:** Enhanced caching mechanisms using Redis for faster access to file metadata and application states.
- **Precaching Feature:** Implemented precaching of file structures on startup to reduce access times for frequently used files.
- **Configuration Options:** Updated `config.toml` to include new settings for deduplication, ISO management, and worker scaling.

### Changed
- **Worker Pool Scaling:** Implemented dynamic adjustment of worker threads based on system resources to optimize performance.
- **Logging Enhancements:** Improved logging for file operations, including detailed information on file extensions and MIME types during uploads.
- **Temporary Path Configuration:** Replaced hardcoded temporary upload directories with a configurable `TempPath` parameter in `config.toml` for greater flexibility.

### Fixed
- **Temporary File Handling:** Resolved issues where temporary `.tmp` files caused "Unsupported file type" warnings by enhancing MIME type detection logic.
- **MIME Type Detection:** Improved MIME type detection to ensure better compatibility and accuracy during file uploads.

### Deprecated
- **Thumbnail Support (Previous Implementation):** Dropped the previous thumbnail support mechanism. This feature will not return in future releases.

---

**Important Update:**
- The minimum Go version required is now **1.20**. Please ensure your environment meets this requirement for successful compilation.

---

## License

MIT License

Copyright (c) 2025 Alexander Renz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
