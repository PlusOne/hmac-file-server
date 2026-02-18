# HMAC File Server

[![Version](https://img.shields.io/badge/version-3.4.0_Cascade-blue.svg)](https://git.uuxo.net/UUXO/hmac-file-server/)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Go Version](https://img.shields.io/badge/go-1.24+-00ADD8.svg)](https://golang.org/)

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

### Security & Operations (v3.4.0 "Cascade")
- **Rate Limiting** - Token bucket per JID/IP with configurable burst, whitelists, and 429 responses
- **HMAC Key Rotation** - Automatic secret rotation with grace period and JSON key persistence
- **Admin Dashboard** - HTMX + Tailwind CSS web UI with live stats, file browser, and user quotas
- **SQLite Metadata Store** - Tracks every upload/download with content types, JIDs, and download counts
- **SIMS Thumbnails** - XEP-0385 thumbnail generation (320×240 JPEG) for image uploads
- **Automatic File Cleanup** - TTL-based expiry with empty directory pruning
- **Audit Logging** - Comprehensive security event logging (uploads, downloads, auth events)
- **Magic Bytes Validation** - Content type verification using file signatures
- **Per-User Quotas** - Storage limits per XMPP JID with Redis tracking
- **Admin API** - Protected JSON endpoints for system management and monitoring
- **ClamAV Integration** - Antivirus scanning for uploaded files

## Installation

### Download Binary

```bash
# From Gitea (Primary)
wget https://git.uuxo.net/UUXO/hmac-file-server/releases/download/v3.4.0/hmac-file-server-linux-amd64
chmod +x hmac-file-server-linux-amd64
sudo mv hmac-file-server-linux-amd64 /usr/local/bin/hmac-file-server

# From GitHub (Mirror)
wget https://github.com/PlusOne/hmac-file-server/releases/download/v3.4.0/hmac-file-server-linux-amd64
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
docker pull ghcr.io/plusone/hmac-file-server:3.4.0
docker run -v /path/to/config:/config -v /path/to/uploads:/uploads \
  ghcr.io/plusone/hmac-file-server:3.4.0
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
| `[admin]` | Admin API and dashboard configuration |
| `[workers]` | Worker pool configuration |
| `[rate_limit]` | Per-JID/IP upload rate limiting |
| `[key_rotation]` | Automatic HMAC secret rotation |
| `[metadata]` | SQLite file metadata tracking |

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
| `/download/...?thumbnail=true` | GET | Image thumbnail (SIMS/XEP-0385) |
| `/health` | GET | Health check |
| `/metrics` | GET | Prometheus metrics |
| `/admin/stats` | GET | Server statistics (auth required) |
| `/admin/files` | GET | List uploaded files (auth required) |
| `/admin/users` | GET | User quota information (auth required) |
| `/admin/dashboard` | GET | Web dashboard UI (auth required) |
| `/admin/dashboard/files` | GET | File browser UI (auth required) |
| `/admin/dashboard/users` | GET | User quotas UI (auth required) |

## Enhanced Features (v3.4.0 "Cascade")

### Rate Limiting

Token bucket rate limiting per JID and/or IP address to prevent upload abuse:

```toml
[rate_limit]
enabled = true
requests_per_minute = 60
burst_size = 10
by_jid = true
by_ip = true
whitelisted_ips = ["10.0.0.0/8"]
whitelisted_jids = ["admin@example.com"]
```

Returns `429 Too Many Requests` with `Retry-After` header when limits are exceeded. Localhost (`127.0.0.1`, `::1`) is always whitelisted.

### HMAC Key Rotation

Automatic rotation of HMAC signing secrets with grace period for seamless transitions:

```toml
[key_rotation]
enabled = true
rotation_interval = "720h"   # Rotate every 30 days
grace_period = "48h"          # Accept old key for 48h after rotation
key_storage = "/etc/hmac-file-server/keys.json"
```

New uploads are signed with the current key. Downloads/validations accept both current and previous key during the grace period. Keys are persisted to disk and survive restarts.

### Admin Dashboard

HTMX-powered web UI with dark theme (Tailwind CSS), accessible at `/admin/dashboard`:

- **Live stats** — storage usage, user count, uptime, memory (auto-refresh every 30s)
- **File browser** — browse, search, and delete uploaded files
- **User quotas** — per-JID usage with progress bars
- **Feature status** — Redis, rate limiter, key rotation, cleanup status cards

No npm or build step required — uses CDN-loaded HTMX and Tailwind CSS.

### SQLite Metadata Store

Persistent file metadata tracking via embedded SQLite (pure Go, no CGO):

```toml
[metadata]
enabled = true
db_path = "/var/lib/hmac-file-server/metadata/files.db"
purge_age = "90d"   # Purge soft-deleted records after 90 days
```

Tracks: uploader JID/IP, content type, upload time, download count, checksums. Provides aggregate statistics (top uploaders, content type distribution) for the admin dashboard.

### SIMS Thumbnails (XEP-0385)

Automatic JPEG thumbnail generation for uploaded images:

- Generates 320×240 thumbnails on upload (async, non-blocking)
- Supports JPEG, PNG, GIF, BMP, TIFF, WebP source formats
- Serve via `?thumbnail=true` query parameter on download URLs
- On-demand generation if thumbnail doesn't exist yet
- Auto-cleanup when source file is deleted

### Automatic File Cleanup

TTL-based file expiry with configurable cleanup interval:

```toml
[server]
file_ttl_enabled = true
file_ttl = "720h"          # Delete files older than 30 days
cleanup_interval = "1h"    # Run cleanup every hour
max_file_age = "720h"      # Maximum file age
```

Automatically removes expired files and prunes empty parent directories.

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
- Go 1.24+ (for building)
- Linux (primary), Windows/macOS (experimental)
- SQLite: Embedded (pure Go, no CGO required)
- Redis: Optional (for quota tracking and session persistence)

## Contributing

Contributions are welcome. Please submit pull requests to the main repository.

## License

Apache License 2.0 - See [LICENSE](LICENSE) for details.

## Links

- Primary Repository: https://git.uuxo.net/UUXO/hmac-file-server
- GitHub Mirror: https://github.com/PlusOne/hmac-file-server
- Documentation: [WIKI.md](WIKI.md)
