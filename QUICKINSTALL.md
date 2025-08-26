# HMAC File Server 3.3.0 "Nexus Infinitum" - Quick Install Guide ⚡

**Get started in under 2 minutes!**

---

## 🚀 **Super Quick Start (30 seconds)**

```bash
# Download and start immediately
wget https://git.uuxo.net/uuxo/hmac-file-server/releases/download/v3.3.0/hmac-file-server-linux-amd64
chmod +x hmac-file-server-linux-amd64
./hmac-file-server-linux-amd64 -genconfig > config.toml
./hmac-file-server-linux-amd64 -config config.toml
```

**That's it!** Your server is running on `http://localhost:8080` 🎉

---

## 📦 **Choose Your Installation Method**

### 1. **Binary Download** (Recommended)
```bash
# Download for your architecture
wget https://git.uuxo.net/uuxo/hmac-file-server/releases/download/v3.3.0/hmac-file-server-linux-amd64
# ARM64: hmac-file-server-linux-arm64  
# ARM32: hmac-file-server-linux-arm

chmod +x hmac-file-server-linux-amd64
./hmac-file-server-linux-amd64 -genconfig > config.toml

# Edit these 3 essential settings in config.toml:
# bind_ip = "0.0.0.0"          # Listen on all interfaces  
# listenport = "8080"          # Your desired port
# storage_path = "./uploads"   # Where to store files

./hmac-file-server-linux-amd64 -config config.toml
```

### 2. **Docker** (Container Deployment)
```bash
# Pull and run
docker pull hmac-file-server:3.3.0
docker run -d --name hmac-server \
  -p 8080:8080 \
  -v ./uploads:/app/uploads \
  hmac-file-server:3.3.0
```

### 3. **Automated Installer** (Full Setup)
```bash
# Download and run installer
wget https://raw.githubusercontent.com/uuxo/hmac-file-server/main/installer.sh
chmod +x installer.sh
sudo ./installer.sh
```

### 4. **Build from Source** (Developers)
```bash
git clone https://git.uuxo.net/uuxo/hmac-file-server.git
cd hmac-file-server
go build -o hmac-file-server ./cmd/server/
./hmac-file-server -genconfig > config.toml
./hmac-file-server -config config.toml
```

---

## ⚙️ **Essential Configuration (2 minutes)**

### Minimal Configuration (Just Works!)
```toml
# config.toml - Only 2 lines needed!
[server]
storage_path = "./uploads"
```

### Basic Production Configuration
```toml
[server]
bind_ip = "0.0.0.0"
listenport = "8080"
storage_path = "/data/uploads"
hmac_secret = "your-secret-key-here"
max_upload_size = "100MB"

[security]
require_hmac = true
```

### Mobile-Optimized Configuration
```toml
[server]
bind_ip = "0.0.0.0"
listenport = "8080"
storage_path = "./uploads"

[network_resilience]
enable_network_resilience = true
grace_period_hours = 72
detect_network_changes = true

[client_network_support]
enable_client_network_support = true
mobile_grace_hours = 72
desktop_grace_hours = 48
```

---

## 🔧 **Quick Configuration Options**

### Generate Configuration Templates
```bash
./hmac-file-server -genconfig > config.toml              # Basic config
./hmac-file-server -genconfig-mobile > mobile.toml       # Mobile-optimized  
./hmac-file-server -genconfig-enterprise > enterprise.toml # Enterprise config
```

### Validate Configuration
```bash
./hmac-file-server -config config.toml --validate        # Check configuration
./hmac-file-server -config config.toml --validate-quiet  # Silent validation
```

### Test Configuration
```bash
./hmac-file-server -config config.toml --check          # Dry run test
```

---

## 🌐 **Integration with XMPP**

### ejabberd Configuration
```yaml
# ejabberd.yml - Add to modules section
modules:
  mod_http_upload:
    put_url: "http://your-server:8080/upload"
    get_url: "http://your-server:8080/file"
    secret: "your-hmac-secret"
    max_size: 104857600  # 100MB
```

### Prosody Configuration  
```lua
-- prosody.cfg.lua
Component "upload.yourdomain.com" "http_upload"
    http_upload_url = "http://your-server:8080/upload"
    http_upload_file_size_limit = 100 * 1024 * 1024 -- 100MB
```

---

## 🔍 **Verify Installation**

### Check Server Status
```bash
# Test server is running
curl http://localhost:8080/health

# Check version
./hmac-file-server -version

# View configuration
./hmac-file-server -config config.toml --validate
```

### Test Upload (with XMPP client)
1. **Configure your XMPP client** with the server URL
2. **Send a file** in any chat
3. **Verify upload** in the `uploads` directory

---

## 🆘 **Troubleshooting**

### Common Issues

**❌ Port already in use**
```bash
# Change port in config.toml
listenport = "8081"  # Use different port
```

**❌ Permission denied**
```bash
# Create uploads directory with proper permissions
mkdir -p uploads
chmod 755 uploads
```

**❌ XMPP upload fails**
```bash
# Use the XMPP client fixing tool
./fix_xmpp_clients.sh
```

**❌ Network switching issues**
```bash
# Test network resilience
./verify_network_resilience.sh
```

### Get Help

- **Documentation**: [Complete WIKI](WIKI.MD)
- **Issues**: [Git Issues](https://git.uuxo.net/uuxo/hmac-file-server/issues)
- **Support**: [Git Repository](https://git.uuxo.net/uuxo/hmac-file-server/)

---

## 🎯 **Next Steps**

### Production Deployment
1. **Set up reverse proxy** (nginx/Apache)
2. **Configure SSL/TLS** certificates  
3. **Set up systemd service** for auto-start
4. **Configure monitoring** and logging
5. **Set up backup** for uploads directory

### Advanced Features
- **Multi-architecture deployment** with `./build-multi-arch.sh`
- **Docker multi-platform** with `./docker-multiarch-build.sh`
- **Network resilience testing** with `./verify_network_resilience.sh`
- **Desktop client optimization** with `./fix_xmpp_clients.sh`

---

## 🚀 **You're Ready!**

Your HMAC File Server 3.3.0 "Nexus Infinitum" is now running and ready for infinite connectivity! 

**What you get:**
- ✅ **Secure file uploads** with HMAC authentication
- ✅ **Multi-architecture support** (AMD64, ARM64, ARM32v7)
- ✅ **Network resilience** for mobile scenarios  
- ✅ **Desktop XMPP client** optimization
- ✅ **Zero-downtime** network switching
- ✅ **Enterprise-grade** reliability

**Server URL**: `http://your-server:8080`  
**Health Check**: `http://your-server:8080/health`  

Enjoy boundless file sharing! 🌟

---

*HMAC File Server 3.3.0 "Nexus Infinitum" - Where Infinite Connectivity Meets Simplicity*
