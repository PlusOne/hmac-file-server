# HMAC File Server

[![Version](https://img.shields.io/badge/version-3.3.0-blue.svg)](https://git.uuxo.net/UUXO/hmac-file-server/)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Go Version](https://img.shields.io/badge/go-1.21+-00ADD8.svg)](https://golang.org/)

A high-performance, secure file server implementing XEP-0363 (HTTP File Upload) with HMAC authentication, deduplication, and multi-architecture support.

## Features

- XEP-0363 HTTP File Upload compliance
- HMAC-based authentication
- File deduplication
- Multi-architecture support (AMD64, ARM64, ARM32v7)
- Docker and Podman deployment
- XMPP client compatibility (Dino, Gajim, Conversations, Monal, Converse.js)
- Network resilience for mobile clients

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
| `[security]` | HMAC secret, TLS settings |
| `[uploads]` | Size limits, allowed extensions |
| `[downloads]` | Download settings, bandwidth limits |
| `[logging]` | Log file, log level |
| `[clamav]` | Antivirus scanning integration |
| `[redis]` | Redis caching backend |
| `[deduplication]` | File deduplication settings |
| `[workers]` | Worker pool configuration |

See [examples/](examples/) for complete configuration templates.

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
# CI trigger 1765622858
# CI trigger 1765622864
<- Fixes ARM64 build failures in CI/CD CI trigger 1765623134 -->
<- Fixes ARM64 build failures in CI/CD CI 1765623179 -->
