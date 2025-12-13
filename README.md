# HMAC File Server

[![Version](https://img.shields.io/badge/version-3.3.0_Nexus_Infinitum-blue.svg)](https://git.uuxo.net/UUXO/hmac-file-server/)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Go Version](https://img.shields.io/badge/go-1.21+-00ADD8.svg)](https://golang.org/)

A high-performance, secure file server implementing XEP-0363 (HTTP File Upload) with HMAC authentication, deduplication, and multi-architecture support.

## Features

### Core Features
- XEP-0363 HTTP File Upload compliance
- HMAC-based authentication with JWT support
- File deduplication (SHA256 with hardlinks)
- Multi-architecture support (AMD64, ARM64, ARM32v7)
- Docker and Podman deployment
- XMPP client compatibility (Dino, Gajim, Conversations, Monal, Converse.js)
- Network resilience for mobile clients (WiFi/LTE switching)

### Security Features (v3.3.0)
- **Audit Logging** - Comprehensive security event logging (uploads, downloads, auth events)
- **Magic Bytes Validation** - Content type verification using file signatures
- **Per-User Quotas** - Storage limits per XMPP JID with Redis tracking
- **Admin API** - Protected endpoints for system management and monitoring
- **ClamAV Integration** - Antivirus scanning for uploaded files
- **Rate Limiting** - Configurable request throttling

## Installation

### Download Binary

```bash
# From Gitea (Primary)
wget https://git.uuxo.net/UUXO/hmac-file-server/releases/download/v3.3.0/hmac-file-server-linux-amd64
chmod +x hmac-file-server-linux-amd64
sudo mv hmac-file-server-linux-amd64 /usr/local/bin/hmac-file-server

# From GitHub (Mirror)
wget https://github.com/PlusOne/hmac-file-server/releases/download/v3.3.0/hmac-file-server-linux-amd64
chmod +x hmac-file-server-linux-amd64
sudo mv hmac-file-server-linux-amd64 /usr/local/bin/hmac-file-server
```

### Build from Source

```bash
git clone https://git.uuxo.net/UUXO/hmac-file-server.git
cd hmac-file-server
go build -o hmac-file-server ./cmd/server/
```

### Docker

```bash
docker pull ghcr.io/plusone/hmac-file-server:3.3.0
docker run -v /path/to/config:/config -v /path/to/uploads:/uploads \
  ghcr.io/plusone/hmac-file-server:3.3.0
```

## Quick Start

```bash
# Generate minimal config
./hmac-file-server -genconfig > config.toml

# Edit essential settings (listen_address, storage_path, secret)
# Start server
./hmac-file-server -config config.toml
```

## Configuration

### Minimal Configuration

```toml
[server]
listenport = "8080"
storagepath = "/srv/uploads"

[security]
secret = "your-hmac-secret-key"
```

### Generate Configuration

```bash
# Minimal config
./hmac-file-server -genconfig

# Advanced config with all options
./hmac-file-server -genconfig-advanced

# Validate configuration
./hmac-file-server -config config.toml -validate
```

### Configuration Sections

| Section | Description |
|---------|-------------|
| `[server]` | Bind address, port, storage path, timeouts |
| `[security]` | HMAC secret, JWT, TLS settings |
| `[uploads]` | Size limits, allowed extensions |
| `[downloads]` | Download settings, bandwidth limits |
| `[logging]` | Log file, log level |
| `[clamav]` | Antivirus scanning integration |
| `[redis]` | Redis caching backend |
| `[audit]` | Security audit logging |
| `[validation]` | Magic bytes content validation |
| `[quotas]` | Per-user storage quotas |
| `[admin]` | Admin API configuration |
| `[workers]` | Worker pool configuration |

See [templates/](templates/) for complete configuration templates.

## XMPP Server Integration

### Prosody

```lua
Component "upload.example.com" "http_upload_external"
    http_upload_external_base_url = "https://upload.example.com/upload/"
    http_upload_external_secret = "your-hmac-secret"
    http_upload_external_file_size_limit = 104857600
```

### Ejabberd

```yaml
mod_http_upload:
  put_url: "https://upload.example.com/upload/@HOST@/v/@TOKEN@/@FILENAME@"
  get_url: "https://upload.example.com/upload/@HOST@/v/@TOKEN@/@FILENAME@"
  external_secret: "your-hmac-secret"
  max_size: 104857600
```

## Nginx Reverse Proxy

```nginx
server {
    listen 443 ssl http2;
    server_name upload.example.com;

    ssl_certificate /etc/letsencrypt/live/upload.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/upload.example.com/privkey.pem;

    client_max_body_size 100M;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_request_buffering off;
        proxy_read_timeout 300s;
    }
}
```

## API Versions

| Version | Path Format | Authentication |
|---------|-------------|----------------|
| V1 | `/upload/{host}/v/{token}/{filename}` | HMAC token |
| V2 | `/upload/{host}/v2/{token}/{filename}` | Enhanced HMAC |
| V3 | `/v3/upload` | Bearer JWT token |

### HMAC Token Generation

```
token = HMAC-SHA256(secret, filename + filesize + timestamp)
```

## Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/upload/...` | PUT | File upload |
| `/download/...` | GET | File download |
| `/health` | GET | Health check |
| `/metrics` | GET | Prometheus metrics |
| `/admin/stats` | GET | Server statistics (auth required) |
| `/admin/files` | GET | List uploaded files (auth required) |
| `/admin/users` | GET | User quota information (auth required) |

## Enhanced Features (v3.3.0)

### Audit Logging

Security-focused logging for compliance and forensics:

```toml
[audit]
enabled = true
output = "file"
path = "/var/log/hmac-audit.log"
format = "json"
events = ["upload", "download", "auth_failure", "quota_exceeded"]
```

### Content Validation

Magic bytes validation to verify file types:

```toml
[validation]
check_magic_bytes = true
allowed_types = ["image/*", "video/*", "audio/*", "application/pdf"]
blocked_types = ["application/x-executable", "application/x-shellscript"]
```

### Per-User Quotas

Storage limits per XMPP JID with Redis tracking:

```toml
[quotas]
enabled = true
default = "100MB"
tracking = "redis"

[quotas.custom]
"admin@example.com" = "10GB"
"premium@example.com" = "1GB"
```

### Admin API

Protected management endpoints:

```toml
[admin]
enabled = true
path_prefix = "/admin"

[admin.auth]
type = "bearer"
token = "${ADMIN_TOKEN}"
```

## System Requirements

- Memory: 512MB minimum, 2GB+ recommended
- Storage: 100MB application + data
- Go 1.21+ (for building)
- Linux (primary), Windows/macOS (experimental)

## Contributing

Contributions are welcome. Please submit pull requests to the main repository.

## License

Apache License 2.0 - See [LICENSE](LICENSE) for details.

## Links

- Primary Repository: https://git.uuxo.net/UUXO/hmac-file-server
- GitHub Mirror: https://github.com/PlusOne/hmac-file-server
- Documentation: [WIKI.md](WIKI.md)
