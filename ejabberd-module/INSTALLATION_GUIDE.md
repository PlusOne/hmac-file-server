# 📖 INSTALLATION GUIDE: mod_http_upload_hmac
## Ejabberd Module for HMAC File Server Integration

### 🎯 Overview
This module enables seamless file uploads in XMPP clients by integrating ejabberd with HMAC File Server 3.3.0. Users get zero-configuration file sharing with automatic authentication.

---

## 🔧 ADMINISTRATOR INSTALLATION

### Prerequisites
- **ejabberd server** (version 20.01 or later)
- **Erlang/OTP** (version 22 or later)  
- **HMAC File Server 3.3.0** with Bearer token support
- **Network connectivity** between ejabberd and HMAC server

### Step 1: Install HMAC File Server 3.3.0
```bash
# Download and install HMAC File Server
wget https://git.uuxo.net/uuxo/hmac-file-server/releases/v3.3.0/hmac-file-server-linux-amd64
chmod +x hmac-file-server-linux-amd64
sudo mv hmac-file-server-linux-amd64 /usr/local/bin/hmac-file-server

# Create configuration
sudo mkdir -p /etc/hmac-file-server
sudo cat > /etc/hmac-file-server/config.toml << EOF
[server]
interface = "0.0.0.0"
port = 8080
upload_path = "/var/lib/hmac-uploads"
log_file = "/var/log/hmac-file-server.log"
log_level = "info"

[auth]
shared_secret = "YOUR-SECURE-SECRET-HERE"
bearer_tokens_enabled = true
token_expiry = 3600
jwt_enabled = true
hmac_enabled = true

[upload]
max_file_size = "100MB"
max_files_per_user = 1000
allowed_mime_types = ["image/*", "video/*", "audio/*", "application/pdf"]

[storage]
cleanup_interval = "24h"
retention_days = 30
EOF

# Create upload directory
sudo mkdir -p /var/lib/hmac-uploads
sudo chown hmac:hmac /var/lib/hmac-uploads

# Create systemd service
sudo cat > /etc/systemd/system/hmac-file-server.service << EOF
[Unit]
Description=HMAC File Server 3.3.0
After=network.target

[Service]
Type=simple
User=hmac
Group=hmac
ExecStart=/usr/local/bin/hmac-file-server -config /etc/hmac-file-server/config.toml
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
EOF

# Start service
sudo systemctl enable hmac-file-server
sudo systemctl start hmac-file-server
```

### Step 2: Install ejabberd Module
```bash
# Copy module to ejabberd
sudo cp mod_http_upload_hmac.erl /opt/ejabberd/lib/ejabberd-*/ebin/

# Compile module
cd /opt/ejabberd/lib/ejabberd-*/ebin/
sudo erlc mod_http_upload_hmac.erl

# Verify compilation
ls -la mod_http_upload_hmac.beam
```

### Step 3: Configure ejabberd
Add to `/etc/ejabberd/ejabberd.yml`:

```yaml
modules:
  mod_http_upload_hmac:
    hmac_server_url: "http://localhost:8080"
    hmac_shared_secret: "YOUR-SECURE-SECRET-HERE"  # Must match HMAC server
    max_size: 104857600  # 100MB
    quota_per_user: 1073741824  # 1GB per user
    token_expiry: 3600  # 1 hour
    iqdisc: one_queue

# Disable default mod_http_upload if enabled
# mod_http_upload: false
```

### Step 4: Restart ejabberd
```bash
sudo systemctl restart ejabberd

# Check logs
sudo tail -f /var/log/ejabberd/ejabberd.log
```

### Step 5: Configure Reverse Proxy (Optional but Recommended)
For HTTPS support with nginx:

```nginx
server {
    listen 443 ssl http2;
    server_name files.yourdomain.com;
    
    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;
    
    client_max_body_size 100M;
    
    location / {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_read_timeout 300;
        proxy_send_timeout 300;
    }
}
```

Update ejabberd config:
```yaml
modules:
  mod_http_upload_hmac:
    hmac_server_url: "https://files.yourdomain.com"
    # ... other settings
```

---

## 👤 USER GUIDE

### What This Enables
- **Automatic file uploads** in XMPP clients (Conversations, Dino, Gajim, etc.)
- **No manual configuration** required in clients
- **Secure authentication** using your XMPP credentials
- **Large file support** up to configured limits

### Supported XMPP Clients
✅ **Conversations** (Android)  
✅ **Dino** (Linux/Desktop)  
✅ **Gajim** (Cross-platform)  
✅ **ChatSecure** (iOS)  
✅ **Monal** (iOS/macOS)  
✅ **Movim** (Web)  
✅ **Any XEP-0363 compatible client**

### How to Use

1. **No setup required** - your XMPP client will automatically discover the upload service
2. **Send files normally** - use your client's attachment/file sharing feature
3. **Files upload automatically** - authentication handled transparently
4. **Recipients get download links** - works across different clients and servers

### File Limits (Default Configuration)
- **Maximum file size**: 100MB per file
- **Storage quota**: 1GB per user
- **File retention**: 30 days
- **Supported types**: Images, videos, audio, documents

### Troubleshooting for Users

**Problem**: File uploads fail  
**Solution**: Check with your server administrator - the service may be temporarily unavailable

**Problem**: Files too large  
**Solution**: Compress files or ask administrator about size limits

**Problem**: Client doesn't show upload option  
**Solution**: Ensure your client supports XEP-0363 HTTP File Upload

---

## 🔍 TESTING AND VALIDATION

### Quick Health Check
```bash
# Test HMAC server
curl http://localhost:8080/health

# Test ejabberd module loading
sudo ejabberdctl modules_available | grep http_upload

# Check ejabberd logs
sudo tail /var/log/ejabberd/ejabberd.log
```

### Integration Test
```bash
# Run comprehensive test suite
cd /path/to/ejabberd-module/
./comprehensive_integration_test.sh
```

### Manual Upload Test
```bash
# Generate test token (simulate ejabberd)
USER="testuser@yourdomain.com"
FILENAME="test.txt"
SECRET="YOUR-SECURE-SECRET-HERE"

# Test upload endpoint
curl -X POST "http://localhost:8080/upload/test-uuid/test.txt" \
     -H "Authorization: Bearer $(echo -n "$USER\0$FILENAME\0$(date +%s)" | openssl dgst -sha256 -hmac "$SECRET" -binary | base64)" \
     -H "Content-Type: text/plain" \
     -d "Test upload content"
```

---

## 🔒 SECURITY CONSIDERATIONS

### For Administrators
- **Use strong shared secrets** (minimum 32 characters)
- **Enable HTTPS** for production deployments  
- **Configure appropriate file size limits**
- **Set up log monitoring** for upload activities
- **Regular security updates** for both ejabberd and HMAC server
- **Network isolation** - HMAC server doesn't need internet access

### Network Security
```bash
# Firewall configuration example
sudo ufw allow from [ejabberd-ip] to any port 8080  # HMAC server
sudo ufw allow 5222/tcp  # XMPP client connections
sudo ufw allow 5269/tcp  # XMPP server-to-server
sudo ufw allow 443/tcp   # HTTPS file uploads (if using reverse proxy)
```

---

## 📊 MONITORING AND MAINTENANCE

### Log Monitoring
```bash
# HMAC server logs
sudo tail -f /var/log/hmac-file-server.log

# ejabberd logs  
sudo tail -f /var/log/ejabberd/ejabberd.log

# nginx logs (if using reverse proxy)
sudo tail -f /var/log/nginx/access.log
```

### Performance Monitoring
- Monitor disk usage in upload directory
- Check memory usage of HMAC server process
- Monitor ejabberd performance impact
- Track upload/download statistics

### Backup Recommendations
- **Configuration files**: `/etc/ejabberd/`, `/etc/hmac-file-server/`
- **Upload data**: `/var/lib/hmac-uploads/` (optional, based on retention policy)
- **ejabberd database**: Standard ejabberd backup procedures

---

## 🆘 TROUBLESHOOTING

### Common Issues

**Module fails to load**
```bash
# Check Erlang compilation
sudo erlc /opt/ejabberd/lib/ejabberd-*/ebin/mod_http_upload_hmac.erl

# Check ejabberd syntax
sudo ejabberdctl check_config
```

**HMAC server not responding**
```bash
# Check service status
sudo systemctl status hmac-file-server

# Check port binding
sudo netstat -tlnp | grep :8080

# Test connectivity
curl -v http://localhost:8080/health
```

**Token authentication fails**
- Verify shared secrets match between ejabberd and HMAC server
- Check system time synchronization
- Review token expiry settings

### Debug Mode
Enable debug logging in ejabberd:
```yaml
loglevel: debug
log_modules_fully: [mod_http_upload_hmac]
```

---

## 📈 SCALING AND PRODUCTION

### High Availability Setup
- Run multiple HMAC server instances behind load balancer
- Use shared storage (NFS/GlusterFS) for upload directory
- Configure ejabberd clustering if needed

### Performance Optimization
- Tune Erlang VM parameters for ejabberd
- Configure nginx caching for downloads
- Use SSD storage for upload directory
- Monitor and adjust file retention policies

---

## 🔄 UPDATES AND MAINTENANCE

### Updating the Module
1. Download new `mod_http_upload_hmac.erl`
2. Backup existing module
3. Replace and recompile
4. Restart ejabberd

### Updating HMAC File Server
1. Stop service: `sudo systemctl stop hmac-file-server`
2. Backup configuration and data
3. Replace binary
4. Start service: `sudo systemctl start hmac-file-server`

---

## 📞 SUPPORT

- **GitHub Issues**: Report bugs and feature requests
- **Documentation**: Check project wiki for updates
- **Community**: Join XMPP development discussions
- **Security Issues**: Report privately to security contact

---

*Last updated: August 25, 2025*  
*Version: HMAC File Server 3.3.0 + ejabberd integration*
