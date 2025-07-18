# HMAC File Server 3.2 – Configuration Revolution

## 🚀 Major Highlights

### Simplified Configuration (93% Reduction)
- **Before**: 112-line complex configs
- **After**: 8-line minimal configs
- **Smart Defaults**: Production-ready settings built-in
- **Auto-Generation**: `--genconfig` creates minimal configs instantly

### Enhanced File Processing
- **Fixed Deduplication**: Existing files return success (not "file not found")
- **Queue Optimization**: Doubled capacity, faster scaling
- **Extended Timeouts**: 4800s defaults for large file reliability
- **Session Persistence**: 60-minute timeouts for enterprise transfers

### Multi-Architecture Support
- **AMD64, ARM64, ARM32v7**: Full cross-compilation support
- **Interactive Builder**: Easy architecture targeting
- **Production Ready**: All platforms enterprise-grade

## 📦 Quick Migration

### Keep Existing Config (Recommended)
Your 3.1.x config works unchanged with enhanced performance.

### Migrate to Simplified Config
```bash
./hmac-file-server -genconfig > config-simple.toml
# Edit just 3 lines: listen_address, storage_path, secret
./hmac-file-server -config config-simple.toml
```

## 🎯 New User Experience

```bash
# Zero-config startup
./hmac-file-server
# Creates minimal config automatically

# Production deployment
./hmac-file-server -genconfig > production.toml
# Edit 3 essential settings
./hmac-file-server -config production.toml
```

## 💡 Key Benefits

- **🎯 User-Friendly**: 3 settings vs 15+ previously required
- **🚀 Performance**: Optimized defaults eliminate configuration guesswork  
- **🔧 Maintainable**: Defaults in code, overrides in config
- **🔄 Compatible**: Zero breaking changes for existing installations
- **🌍 Cross-Platform**: True multi-architecture support

---

*Download 3.2 "Tremora del Terra" – Where enterprise power meets user simplicity* ⚡
