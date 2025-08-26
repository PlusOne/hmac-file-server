# Ejabberd HMAC File Server Integration Module

This directory contains `mod_http_upload_hmac`, an ejabberd module that provides seamless integration between XMPP clients and the HMAC File Server, implementing XEP-0363 HTTP File Upload with automatic authentication.

## 🎯 Problem Solved

**Before**: XMPP clients needed manual HMAC secret configuration, suffered from re-authentication failures, and experienced 404 upload errors.

**After**: Zero client configuration, automatic authentication via existing XMPP session, and seamless file uploads for all XEP-0363 compatible clients.

## ✨ Features

- 🔐 **Seamless Authentication** - Uses existing XMPP user session
- 🎫 **Bearer Token Generation** - Temporary, secure upload tokens
- 📱 **Universal Client Support** - Works with Conversations, Dino, Gajim, Monal
- 👥 **User Management** - Per-user quotas and permissions
- 📊 **Audit Logging** - Complete upload tracking
- 🔒 **Enhanced Security** - No shared secrets in clients
- ⚡ **XEP-0363 Compliant** - Standard HTTP File Upload protocol

## 🏗️ Architecture

```
XMPP Client → Ejabberd → mod_http_upload_hmac → HMAC File Server
     ↓           ↓              ↓                    ↓
  XEP-0363    Auth Check    Generate Token       Store File
  Request     & Quotas      & Upload URL         & Validate
```

## 📦 Installation

### Quick Install
```bash
cd ejabberd-module
sudo ./install.sh
```

### Manual Installation

1. **Compile the module:**
```bash
erlc -I /opt/ejabberd/lib/ejabberd-*/include -o . mod_http_upload_hmac.erl
```

2. **Install to ejabberd:**
```bash
sudo cp mod_http_upload_hmac.beam /opt/ejabberd/lib/ejabberd-*/ebin/
sudo chown ejabberd:ejabberd /opt/ejabberd/lib/ejabberd-*/ebin/mod_http_upload_hmac.beam
```

3. **Configure ejabberd.yml:**
```yaml
modules:
  mod_http_upload_hmac:
    hmac_server_url: "http://localhost:8080"
    hmac_shared_secret: "your-secure-secret"
    max_size: 104857600  # 100MB
    quota_per_user: 1073741824  # 1GB  
    token_expiry: 3600  # 1 hour
    allowed_extensions: 
      - ".jpg"
      - ".png"
      - ".pdf"
      - ".mp4"
      - ".mp3"
    iqdisc: one_queue

# Disable default mod_http_upload
# mod_http_upload: []
```

4. **Update HMAC File Server:**
```toml
[ejabberd_integration]
enabled = true
bearer_token_auth = true
shared_secret = "your-secure-secret"  # Same as ejabberd
```

5. **Restart services:**
```bash
sudo systemctl restart ejabberd
sudo systemctl restart hmac-file-server
```

## 🔧 Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `hmac_server_url` | string | `"http://localhost:8080"` | HMAC File Server base URL |
| `hmac_shared_secret` | string | `"default-secret-change-me"` | Shared secret for token generation |
| `max_size` | integer | `104857600` | Maximum file size in bytes (100MB) |
| `quota_per_user` | integer | `1073741824` | User storage quota in bytes (1GB) |
| `token_expiry` | integer | `3600` | Token validity in seconds (1 hour) |
| `allowed_extensions` | list | `[]` | Allowed file extensions (empty = all) |
| `iqdisc` | atom | `one_queue` | IQ processing discipline |

## 🚀 Usage

### For XMPP Clients

**No configuration required!** Just use your XMPP client as normal:

1. Open any XEP-0363 compatible client (Conversations, Dino, Gajim)
2. Send a file in any chat
3. File uploads automatically using your XMPP credentials
4. No HMAC secrets or special configuration needed

### For Administrators

Monitor uploads and manage users:

```bash
# Check ejabberd logs
journalctl -u ejabberd -f

# Check HMAC server logs  
journalctl -u hmac-file-server -f

# View user quotas (if implemented)
ejabberdctl get_user_quota username@domain.tld
```

## 🔐 Security

### Authentication Flow

1. **XMPP Client** requests upload slot via XEP-0363
2. **Ejabberd** validates user via existing XMPP session
3. **Module** generates time-limited Bearer token with HMAC
4. **Client** uploads file with Bearer token to HMAC server
5. **HMAC Server** validates token and stores file

### Token Format

```
Bearer <base64(hmac-sha256(user + filename + size + timestamp, secret))>
```

### Security Benefits

- ✅ **No shared secrets** in XMPP clients
- ✅ **Time-limited tokens** (default 1 hour)
- ✅ **User-based authentication** via XMPP session
- ✅ **Per-user quotas** and permissions
- ✅ **Audit trail** for all uploads

## 🧪 Testing

### Test Installation
```bash
# Check module loading
sudo ejabberdctl module_check mod_http_upload_hmac

# Test with XMPP client
# 1. Connect to your ejabberd server
# 2. Try uploading a file
# 3. Check logs for Bearer token authentication
```

### Debug Mode
```yaml
# In ejabberd.yml
log_level: debug

# Check detailed logs
journalctl -u ejabberd -f | grep "mod_http_upload_hmac"
```

## 📱 Client Compatibility

| Client | Platform | Status | Notes |
|--------|----------|--------|-------|
| **Conversations** | Android | ✅ Full Support | Native XEP-0363 |
| **Dino** | Linux/Windows | ✅ Full Support | Native XEP-0363 |
| **Gajim** | Cross-platform | ✅ Full Support | Plugin required |
| **Monal** | iOS/macOS | ✅ Full Support | Native XEP-0363 |
| **Movim** | Web | ✅ Full Support | Web interface |
| **Siskin IM** | iOS | ✅ Full Support | Native XEP-0363 |

## 🔄 Migration from Manual HMAC

### Gradual Migration
1. **Install module** alongside existing setup
2. **Test with one client** to verify functionality  
3. **Remove HMAC config** from clients one by one
4. **Monitor logs** to ensure all clients switch over
5. **Disable legacy HMAC** when all clients migrated

### Backward Compatibility

The HMAC File Server supports both authentication methods simultaneously:
- ✅ **Bearer tokens** (ejabberd module)
- ✅ **Legacy HMAC** (manual client configuration)
- ✅ **JWT tokens** (if enabled)

## 🐛 Troubleshooting

### Common Issues

**"Module compilation failed"**
```bash
# Install Erlang development tools
sudo apt-get install erlang-dev erlang-tools
```

**"Authentication failed"**
```bash
# Check shared secret matches
grep "hmac_shared_secret" /opt/ejabberd/conf/ejabberd.yml
grep "shared_secret" /etc/hmac-file-server/config.toml
```

**"404 Upload errors"**
```bash
# Verify HMAC server is running
systemctl status hmac-file-server

# Check URLs are correct
curl -I http://localhost:8080/health
```

### Debug Steps

1. **Check module loading:**
```bash
sudo ejabberdctl modules | grep http_upload
```

2. **Verify configuration:**
```bash
sudo ejabberdctl get_option modules
```

3. **Test token generation:**
```bash
# Enable debug logging in ejabberd.yml
log_level: debug
```

4. **Monitor both services:**
```bash
# Terminal 1
journalctl -u ejabberd -f

# Terminal 2  
journalctl -u hmac-file-server -f
```

## 📋 Requirements

- **ejabberd** 20.01+ (tested with 23.x)
- **Erlang/OTP** 23+ 
- **HMAC File Server** 3.2.2+
- **XMPP Client** with XEP-0363 support

## 🔄 Updates

### Version Compatibility

| Module Version | ejabberd | HMAC Server | Features |
|----------------|----------|-------------|----------|
| 1.0.0 | 20.01+ | 3.2.2+ | Bearer tokens, basic auth |
| 1.1.0 | 23.01+ | 3.2.2+ | User quotas, audit logging |

### Upgrade Path
```bash
# Stop services
sudo systemctl stop ejabberd

# Update module
sudo cp new_mod_http_upload_hmac.beam /opt/ejabberd/lib/ejabberd-*/ebin/

# Start services  
sudo systemctl start ejabberd
```

## 🤝 Contributing

1. **Fork** the repository
2. **Create** feature branch
3. **Test** with multiple XMPP clients
4. **Submit** pull request

### Development Setup
```bash
# Clone repository
git clone https://github.com/PlusOne/hmac-file-server.git
cd hmac-file-server/ejabberd-module

# Test compilation
erlc -I /opt/ejabberd/lib/ejabberd-*/include mod_http_upload_hmac.erl

# Run tests
./test.sh
```

## 📄 License

Same as HMAC File Server - see main repository LICENSE file.

## 🆘 Support

- **Issues**: [GitHub Issues](https://github.com/PlusOne/hmac-file-server/issues)
- **Discussions**: [GitHub Discussions](https://github.com/PlusOne/hmac-file-server/discussions)  
- **XMPP Chat**: `hmac-support@conference.example.org`

---

**🎉 Enjoy seamless XMPP file uploads with zero client configuration!**
