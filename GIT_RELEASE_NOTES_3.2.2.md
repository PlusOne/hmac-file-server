## HMAC File Server 3.3.0 - Enhanced MIME Types & XMPP Compatibility

### 🚀 New Features
- **Enhanced MIME Types**: Added 80+ file format mappings (.flac, .webm, .epub, .docx, .py, .go, etc.)
- **XMPP Client Ecosystem**: Comprehensive compatibility with Conversations, Dino, Gajim, Monal
- **Network Resilience**: Optimized mobile WLAN ↔ 5G switching

### 🔧 Improvements
- Better Content-Type headers for downloads
- Enhanced browser file handling
- Future-proof file format support
- Zero breaking changes

### 📦 Deployment
```bash
# Docker
docker pull hmac-file-server:3.3.0

# Binary
wget https://git.uuxo.net/uuxo/hmac-file-server/releases/download/v3.3.0/hmac-file-server-linux-amd64
```

### 🛡️ Security
- HMAC authentication core unchanged
- 100% backward compatible
- All XMPP protocols supported

**Drop-in upgrade** - no configuration changes required!
