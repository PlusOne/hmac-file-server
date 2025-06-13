# HMAC File Server 3.2 Installation Guide

## Quick Installation for XMPP Operators

The HMAC File Server includes an automated installer script designed specifically for XMPP operators who want to quickly deploy a file sharing service for their chat servers. The installer now supports both **native systemd installation** and **Docker deployment**.

### Prerequisites

- Linux system with systemd (Ubuntu 18.04+, CentOS 7+, Debian 9+, etc.) for native installation
- Docker and Docker Compose for containerized deployment
- Root or sudo access
- At least 1GB free disk space
- Internet connection for downloading dependencies

### Installation Options

The installer provides two deployment methods:

1. **Native Installation** (systemd service) - Traditional installation with Go build and systemd service
2. **Docker Deployment** (docker-compose) - Containerized deployment with automatic service orchestration

### Installation

1. **Download or clone the repository:**
   ```bash
   git clone https://github.com/PlusOne/hmac-file-server.git
   cd hmac-file-server
   ```

2. **Run the installer:**
   ```bash
   sudo ./installer.sh
   ```

3. **Choose deployment type:**
   - **Option 1**: Native installation (systemd service)
   - **Option 2**: Docker deployment (docker-compose)

4. **Configure installation:**
   - **System user** (default: `hmac-server`)
   - **Installation/deployment directories**
   - **Configuration directory** (customizable)
   - **Server ports**
   - **HMAC secret**: Choose automatic generation (recommended) or enter manually
   - **Optional features** (JWT, Redis, ClamAV, SSL/TLS)

### Docker Deployment Features

When selecting Docker deployment, the installer will:
- Create a complete docker-compose.yml with Redis and ClamAV services
- Generate optimized Dockerfile for multi-stage builds
- Set up proper networking between services
- Create start/stop scripts for easy management
- Configure container-optimized paths and volumes
- Provide isolated deployment directory structure

### Native Installation Features

When selecting native installation, the installer will:
- Install Go 1.24 (if needed)
- Create system user and directories
- Build and configure the server
- Set up systemd service
- Optionally install Redis and ClamAV

   **Alternative: Pre-set secrets via environment variables:**
   ```bash
   # For automation or if interactive input doesn't work
   HMAC_SECRET='your-super-secret-hmac-key-here-minimum-32-characters' sudo -E ./installer.sh
   
   # With both HMAC and JWT secrets
   HMAC_SECRET='your-hmac-secret-32-chars-minimum' \
   JWT_SECRET='your-jwt-secret-also-32-chars-minimum' \
   sudo -E ./installer.sh
   ```

3. **Follow the interactive prompts:**
   - System user (default: `hmac-server`)
   - Installation directories
   - Server ports
   - **HMAC secret**: Choose automatic generation (recommended) or enter manually
   - **Optional features** (JWT, Redis, ClamAV, SSL/TLS)
     - **JWT secret**: Also supports automatic generation if enabled

### Configuration Options

#### Core Settings
- **Server Port**: Default 8080 (HTTP file server)
- **Metrics Port**: Default 9090 (Prometheus metrics)
- **HMAC Secret**: Strong secret for authentication
  - **Automatic generation** (recommended): Creates 48-character secure random key
  - **Manual entry**: Minimum 32 characters required
  - **Environment variable**: `HMAC_SECRET='your-secret'`

#### Optional Features
- **JWT Authentication**: Token-based auth for enhanced security
  - **Automatic generation** available for JWT secrets
  - Configurable expiration and algorithms
- **Redis Integration**: For session management and caching
- **ClamAV Scanning**: Real-time virus scanning of uploaded files
- **SSL/TLS**: Direct HTTPS support (or use reverse proxy)

### XMPP Server Integration

#### Prosody Configuration
Add to your Prosody configuration:
```lua
Component "upload.yourdomain.com" "http_file_share"
  http_file_share_url = "http://localhost:8080"
```

#### Ejabberd Configuration
Add to your Ejabberd configuration:
```yaml
mod_http_file_share:
  external_secret: "your-hmac-secret"
  service_url: "http://localhost:8080"
```

### Post-Installation

#### For Native Installation

1. **Start the service:**
   ```bash
   sudo systemctl start hmac-file-server
   ```

2. **Check status:**
   ```bash
   sudo systemctl status hmac-file-server
   ```

3. **View logs:**
   ```bash
   sudo journalctl -u hmac-file-server -f
   ```

#### For Docker Deployment

1. **Start the containers:**
   ```bash
   cd /path/to/your/docker/deployment
   ./start.sh
   # Or manually: docker-compose up -d
   ```

2. **Check status:**
   ```bash
   docker-compose ps
   ```

3. **View logs:**
   ```bash
   docker-compose logs -f hmac-file-server
   ```

4. **Stop the containers:**
   ```bash
   ./stop.sh
   # Or manually: docker-compose down
   ```

### Firewall Configuration

4. **Configure firewall (required for both deployment types):**
   ```bash
   # Example for ufw (Ubuntu/Debian)
   sudo ufw allow 8080/tcp comment "HMAC File Server"
   sudo ufw allow 9090/tcp comment "HMAC File Server Metrics"
   
   # Example for firewalld (CentOS/RHEL/Fedora)
   sudo firewall-cmd --permanent --add-port=8080/tcp
   sudo firewall-cmd --permanent --add-port=9090/tcp
   sudo firewall-cmd --reload
   
   # Example for iptables (manual)
   sudo iptables -A INPUT -p tcp --dport 8080 -j ACCEPT
   sudo iptables -A INPUT -p tcp --dport 9090 -j ACCEPT
   ```

5. **Configure reverse proxy (recommended):**
   ```nginx
   server {
       listen 443 ssl http2;
       server_name upload.yourdomain.com;
       
       ssl_certificate /path/to/cert.pem;
       ssl_certificate_key /path/to/key.pem;
       
       location / {
           proxy_pass http://localhost:8080;
           proxy_set_header Host $host;
           proxy_set_header X-Real-IP $remote_addr;
           proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
           proxy_set_header X-Forwarded-Proto $scheme;
           
           # File upload settings
           client_max_body_size 100M;
           proxy_request_buffering off;
       }
   }
   ```

### File Locations

After installation:
- **Binary**: `/opt/hmac-file-server/hmac-file-server`
- **Configuration**: `/etc/hmac-file-server/config.toml`
- **Uploads**: `/var/lib/hmac-file-server/uploads/`
- **Logs**: `/var/log/hmac-file-server/hmac-file-server.log`

### Management Commands

```bash
# Service management
sudo systemctl start hmac-file-server
sudo systemctl stop hmac-file-server
sudo systemctl restart hmac-file-server
sudo systemctl reload hmac-file-server

# View logs
sudo journalctl -u hmac-file-server -f
sudo tail -f /var/log/hmac-file-server/hmac-file-server.log

# Edit configuration
sudo nano /etc/hmac-file-server/config.toml
sudo systemctl reload hmac-file-server  # Apply changes
```

### Uninstallation

The HMAC File Server installer includes a comprehensive uninstallation system with advanced data preservation options:

```bash
sudo ./installer.sh --uninstall
```

#### Safe Uninstallation Features

🔒 **Interactive Confirmation System**
- Multiple confirmation steps prevent accidental data loss
- Automatic detection of data directories from configuration
- Smart backup system with timestamped backups in `/var/backups/hmac-file-server-*`
- Detailed reporting showing file counts and directory sizes

#### Five Data Handling Options

**1. 🗑️ Complete Removal**
- Deletes all data including uploads, deduplication files, and logs
- Requires typing "DELETE" for final confirmation
- Provides comprehensive warning about permanent data loss

**2. 💾 Preserve Uploads and Deduplication**
- Preserves critical user files and deduplication data
- Removes logs (typically not needed for data recovery)
- Ideal for system migration or reinstallation

**3. 📋 Preserve All Data**
- Keeps uploads, deduplication data, and logs
- Comprehensive data preservation option
- Best for troubleshooting or temporary removal

**4. 🎯 Custom Selection**
- Interactive selection of which directories to preserve
- Shows detailed information for each directory before decision
- Allows granular control over data preservation

**5. ❌ Cancel Operation**
- Safely exits without making any changes
- No system modifications performed

#### What Gets Removed (Service Components)
- ✓ Systemd service (stopped and disabled)
- ✓ Installation directory (`/opt/hmac-file-server/`)
- ✓ Configuration files (`/etc/hmac-file-server/`)
- ✓ System user (`hmac-server`)
- ✓ Any remaining binaries

#### Data Backup Location
When data preservation is selected, files are moved to:
- `/var/backups/hmac-file-server-TIMESTAMP/`
- Timestamped directories for multiple backup versions
- Preserves original directory structure

**⚠️ Important**: The uninstaller provides multiple safety checks and data preservation options. Choose wisely based on your needs!

### Security Considerations

1. **Configure firewall properly** - Only allow necessary ports (8080, 9090) to authorized networks
2. **Use strong HMAC secrets** (minimum 32 characters, use random generators)
3. **Enable JWT authentication** for enhanced security
4. **Set up SSL/TLS** either directly or via reverse proxy
5. **Enable ClamAV** for virus scanning if handling untrusted files
6. **Regular backups** of configuration and uploaded files
7. **Monitor logs** for suspicious activity
8. **Restrict network access** - Consider limiting access to internal networks only

### Monitoring

The server provides Prometheus metrics at `/metrics` endpoint:
```bash
curl http://localhost:9090/metrics
```

Key metrics to monitor:
- `hmac_requests_total` - Total requests
- `hmac_upload_size_bytes` - Upload sizes
- `hmac_errors_total` - Error counts
- `hmac_active_connections` - Active connections

### Troubleshooting

#### Service won't start
1. Check logs: `sudo journalctl -u hmac-file-server -f`
2. Verify configuration: `sudo nano /etc/hmac-file-server/config.toml`
3. Check permissions on data directories
4. Ensure ports are not in use: `sudo netstat -tlnp | grep :8080`

#### High memory usage
1. Adjust worker settings in configuration
2. Enable Redis for session management
3. Check for large file uploads in progress

#### Files not uploading
1. Verify HMAC secret matches between XMPP server and file server
2. Check file size limits in configuration
3. Ensure sufficient disk space
4. Review ClamAV logs if virus scanning enabled

### Support

- **Documentation**: See `README.MD` and `WIKI.MD`
- **Protocol Details**: See `PROTOCOL_SPECIFICATIONS.MD`
- **Issues**: GitHub issue tracker
- **Configuration**: All options documented in `WIKI.MD`

### Example Production Setup

For a production XMPP server with 1000+ users:

```toml
[server]
listenport = "8080"
metricsenabled = true
deduplicationenabled = true

[security]
enablejwt = true
# Strong secrets here

[uploads]
maxfilesize = "50MB"
ttlenabled = true
ttl = "720h"  # 30 days

[workers]
max = 200
autoscaling = true

[redis]
enabled = true
host = "localhost"
port = 6379

[clamav]
enabled = true
```

This setup provides robust file sharing with deduplication, automatic cleanup, virus scanning, and scalable worker management.
