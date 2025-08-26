# HMAC File Server 3.2.2 Release Notes

**Release Date**: August 24, 2025  
**Codename**: Nexus Infinitum  

## 🚀 New Features

### Enhanced MIME Type Support
- **80+ Additional File Types**: Added comprehensive MIME type detection for modern file formats
- **Extended Format Coverage**: Support for audio (.flac, .opus), video (.webm, .mkv), archives (.7z, .zst), documents (.epub, .docx), programming files (.py, .go, .rs), and more
- **Improved Browser Compatibility**: Better Content-Type headers for downloads and XMPP clients

### XMPP Client Ecosystem
- **Comprehensive Compatibility Analysis**: Complete compatibility matrix for Android, iOS, Linux, Windows, and web XMPP clients
- **Enhanced Client Support**: Verified compatibility with Conversations, Dino, Gajim, Monal, and other major XMPP clients
- **Network Resilience**: Optimized mobile network switching (WLAN ↔ 5G) for better reliability

## 🔧 Technical Improvements

### Core Enhancements
- **HMAC Authentication**: Core functions remain untouched and fully compatible
- **Backward Compatibility**: 100% compatible with existing configurations and clients
- **Performance Optimization**: Enhanced MIME detection with O(1) lookup performance

### Infrastructure
- **Documentation Updates**: All documentation updated to version 3.2.2
- **Docker Images**: Updated container tags to `hmac-file-server:3.2.2`
- **Build System**: Version consistency across all components

## 🎯 Benefits

- **Better File Handling**: Improved browser and client file type recognition
- **Enhanced XMPP Integration**: Superior compatibility with mobile XMPP clients
- **Future-Proof**: Support for emerging file formats and protocols
- **Zero Breaking Changes**: Drop-in upgrade from previous versions

## 📦 Deployment

### Docker
```bash
docker pull hmac-file-server:3.2.2
```

### Binary Download
```bash
wget https://git.uuxo.net/uuxo/hmac-file-server/releases/download/v3.2.2/hmac-file-server-linux-amd64
```

### Upgrade Notes
- **No configuration changes required**
- **Automatic MIME type improvements**
- **Maintains all existing functionality**

## 🛡️ Security & Compatibility

- ✅ HMAC authentication core preserved
- ✅ All XMPP protocol versions supported (v1, v2, v3, token)
- ✅ Backward compatible with existing clients
- ✅ No security regressions

---

**Full Changelog**: [3.2.1...3.2.2](https://git.uuxo.net/uuxo/hmac-file-server/compare/v3.2.1...v3.2.2)
