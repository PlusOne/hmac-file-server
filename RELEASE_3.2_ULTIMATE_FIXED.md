# HMAC File Server 3.2 Ultimate Fixed - Release Notes

## 🚀 Major Release: Complete Configuration Modernization & Enhanced Multi-Platform Support

**Release Date:** July 18, 2025  
**Version:** 3.2 Ultimate Fixed  
**Codename:** "Architecture Revolution"

---

## 🎯 What's New in 3.2 Ultimate Fixed

This release represents a **complete modernization** of HMAC File Server with comprehensive configuration updates, enhanced multi-architecture support, and improved project organization. Every aspect of the server has been refined for better performance, reliability, and ease of deployment.

### 🔧 Configuration System Overhaul

#### **Modernized Field Names**
All configuration fields have been updated to use consistent, modern naming conventions:

| **Old Field Name** | **New Field Name** | **Purpose** |
|-------------------|-------------------|-------------|
| `listenport` | `listen_address` | Server binding address and port |
| `storagepath` | `storage_path` | File storage directory |
| `metricsenabled` | `metrics_enabled` | Prometheus metrics toggle |
| `readtimeout` | `read_timeout` | HTTP read timeout |
| `writetimeout` | `write_timeout` | HTTP write timeout |
| `idletimeout` | `idle_timeout` | HTTP idle timeout |

#### **Extended Timeout Support**
- **4800-second timeouts** for large file handling (up from 30s)
- Perfect for multi-gigabyte file transfers
- Eliminates timeout errors during long uploads/downloads
- Configurable per operation type

#### **Enhanced Deduplication System**
```toml
[deduplication]
enabled = true
directory = "./deduplication"
maxsize = "1GB"  # Files larger than 1GB bypass deduplication for performance
```

#### **Dynamic Worker Scaling**
```toml
[server]
enable_dynamic_workers = true
worker_scale_up_thresh = 50    # Scale up when queue exceeds 50
worker_scale_down_thresh = 10  # Scale down when queue drops below 10
```

### 🏗️ Multi-Architecture Build System

#### **New Build Script Features**
The `buildgo.sh` script now supports:

- **Interactive Architecture Selection Menu**
- **Cross-compilation Support** for:
  - **AMD64** (x86_64) - Standard servers and desktops
  - **ARM64** (AArch64) - Modern ARM processors, Raspberry Pi 4+
  - **ARM32v7** - Older ARM devices, Raspberry Pi 3 and earlier
- **Build-All Option** - Creates all architectures in one command
- **Smart Binary Naming** - `hmac-file-server_amd64`, `hmac-file-server_arm64`, etc.
- **Color-coded Output** for better user experience

#### **Usage Examples**
```bash
# Interactive mode with menu
./buildgo.sh

# Menu options:
# 1) AMD64 (x86_64)
# 2) ARM64 (AArch64) 
# 3) ARM32v7
# 4) Build All Architectures
# 5) Native Build
```

### 📁 Project Organization Improvements

#### **Test Suite Reorganization**
- All test scripts moved to dedicated `tests/` directory
- Comprehensive test documentation in `tests/README.md`
- Organized test categories:
  - **Upload Tests** - Various file sizes and types
  - **Network Tests** - Connection resilience and recovery
  - **Performance Tests** - Load testing and benchmarks
  - **Integration Tests** - Full system validation

#### **Test Files Available**
- `test_1mb.bin` / `test_1mb.txt` - Small file testing
- `test_50mb.bin` - Medium file testing  
- `test_215mb.bin` - Large file testing
- `test_4gb.bin` / `test_4gb.txt` - Massive file testing
- `chunk_0.bin` - Chunked upload testing

### 🛡️ Security & Performance Enhancements

#### **ClamAV Selective Scanning**
```toml
[clamav]
# Only scan potentially dangerous file types
scanfileextensions = [".txt", ".pdf", ".doc", ".docx", ".exe", ".zip", ".rar"]
# Skip files larger than 200MB (ClamAV performance limit)
maxscansize = "200MB"
```

#### **Smart File Handling**
- **Deduplication** with hard-link optimization
- **Pre-caching** for frequently accessed files
- **Resumable uploads/downloads** for network resilience
- **Chunked transfer** support for large files

### 🐳 Docker & Deployment Improvements

#### **Enhanced Docker Configuration**
- Updated `dockerenv/config/config.toml` with all modern settings
- Optimized container resource usage
- Better volume mapping for persistent storage
- Improved health check configurations

#### **Production-Ready Defaults**
```toml
[server]
max_upload_size = "10GB"
cleanup_interval = "24h"
max_file_age = "720h"  # 30 days
min_free_bytes = "1GB"
```

### 📖 Documentation Overhaul

#### **Completely Updated Documentation**
- **README.md** - Modern configuration examples and usage
- **WIKI.md** - Comprehensive configuration reference
- **INSTALL.md** - Production deployment guide
- **BUILD_GUIDE.md** - Multi-architecture build instructions
- **NETWORK_RESILIENCE_GUIDE.md** - Network handling best practices

#### **Configuration Best Practices**
All documentation now includes:
- **Timeout configuration** for different use cases
- **Performance tuning** recommendations
- **Security hardening** guidelines
- **Troubleshooting** common issues

---

## 🔄 Migration Guide

### From 3.1.x to 3.2 Ultimate Fixed

#### **Configuration Updates Required**
1. **Update field names** in your `config.toml`:
   ```bash
   # Old format
   listenport = ":8080"
   storagepath = "/uploads"
   metricsenabled = true
   
   # New format  
   listen_address = ":8080"
   storage_path = "/uploads"
   metrics_enabled = true
   ```

2. **Update timeout values** for better large file support:
   ```toml
   [timeouts]
   readtimeout = "4800s"
   writetimeout = "4800s" 
   idletimeout = "4800s"
   ```

3. **Enable new features**:
   ```toml
   [server]
   enable_dynamic_workers = true
   
   [deduplication]
   enabled = true
   maxsize = "1GB"
   ```

#### **No Breaking Changes**
- Backward compatibility maintained for core functionality
- Old configuration files will work with warnings
- Gradual migration supported

---

## 🚀 Quick Start

### **1. Download & Build**
```bash
# Clone repository
git clone https://github.com/your-org/hmac-file-server.git
cd hmac-file-server

# Build for your architecture
./buildgo.sh
# Select option from interactive menu

# Or build all architectures
./buildgo.sh
# Select option 4 "Build All Architectures"
```

### **2. Configure**
```bash
# Copy example configuration
cp config-example.toml config.toml

# Edit for your environment
nano config.toml
```

### **3. Run**
```bash
# Start server
./hmac-file-server -config config.toml

# Or with Docker
cd dockerenv
docker-compose up -d
```

---

## 🧪 Testing

### **Run Test Suite**
```bash
# Run all tests
cd tests
./run_all_tests.sh

# Run specific test category
./test_upload_performance.sh
./test_network_resilience.sh
```

### **Available Tests**
- **Upload/Download** functionality
- **Network resilience** and recovery
- **Multi-architecture** binary validation
- **Configuration** validation
- **Performance** benchmarking

---

## 📊 Performance Improvements

| **Feature** | **3.1.x** | **3.2 Ultimate** | **Improvement** |
|-------------|-----------|------------------|-----------------|
| Upload Timeout | 30s | 4800s | **160x longer** |
| Large File Support | Limited | 10GB+ | **Unlimited** |
| Worker Scaling | Static | Dynamic | **Auto-scaling** |
| Deduplication | Basic | Smart (1GB limit) | **Performance optimized** |
| Architecture Support | AMD64 only | AMD64/ARM64/ARM32 | **Multi-platform** |
| Build Time | Manual | Automated menu | **User-friendly** |

---

## 🛠️ Technical Specifications

### **System Requirements**
- **Minimum RAM:** 512MB
- **Recommended RAM:** 2GB+ for large files
- **Disk Space:** 100MB + storage for files
- **Go Version:** 1.19+ for building

### **Supported Platforms**
- **Linux AMD64** (x86_64)
- **Linux ARM64** (AArch64) 
- **Linux ARM32** (ARMv7)
- **Docker** containers
- **Kubernetes** deployments

### **Network Protocols**
- **HTTP/HTTPS** with configurable redirect
- **XEP-0363** compliant file upload
- **Chunked transfer** encoding
- **Resumable** uploads/downloads

---

## 🤝 Contributing

### **Development Setup**
1. Fork the repository
2. Create feature branch
3. Use `./buildgo.sh` for testing builds
4. Run test suite: `cd tests && ./run_all_tests.sh`
5. Submit pull request

### **Documentation Updates**
- Update relevant `.md` files
- Test configuration examples
- Validate cross-references

---

## 📝 Changelog Summary

### **Added**
- ✅ Multi-architecture build system (AMD64/ARM64/ARM32)
- ✅ Interactive build script with menu selection
- ✅ Dynamic worker scaling with configurable thresholds
- ✅ Extended timeout support (4800s) for large files
- ✅ Smart deduplication with size limits
- ✅ Comprehensive test suite organization
- ✅ Modern configuration field naming
- ✅ Enhanced ClamAV selective scanning

### **Changed**
- 🔄 Configuration field names modernized
- 🔄 Timeout defaults increased for large file support
- 🔄 Documentation completely updated
- 🔄 Project structure reorganized with tests/ folder
- 🔄 Docker configuration optimized

### **Fixed**
- 🐛 Large file upload timeout issues
- 🐛 Configuration inconsistencies across documentation
- 🐛 Build script platform limitations
- 🐛 Test script organization and discoverability

### **Deprecated**
- ⚠️ Old configuration field names (still supported with warnings)

---

## 🏆 Credits

**Development Team:**
- Core server enhancements
- Multi-architecture build system
- Configuration modernization
- Documentation overhaul
- Test suite organization

**Special Thanks:**
- Community feedback on timeout issues
- Multi-platform deployment requests
- Configuration consistency improvements

---

## 📞 Support

- **Documentation:** [WIKI.md](WIKI.md)
- **Installation:** [INSTALL.md](INSTALL.md)  
- **Build Guide:** [BUILD_GUIDE.md](BUILD_GUIDE.md)
- **Network Setup:** [NETWORK_RESILIENCE_GUIDE.md](NETWORK_RESILIENCE_GUIDE.md)
- **Issues:** GitHub Issues
- **Discussions:** GitHub Discussions

---

**HMAC File Server 3.2 Ultimate Fixed** - *Powering reliable file transfers across all architectures* 🚀
