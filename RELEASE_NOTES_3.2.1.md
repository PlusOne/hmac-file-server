# HMAC File Server 3.2.1 – Critical Fixes Release 🔧

**Release Date**: July 20, 2025  
**Type**: Critical Bug Fix Release  
**Focus**: Network Resilience Configuration & XMPP Integration Fixes

---

## 🚨 Critical Fixes

### **Configuration Loading Regression (CRITICAL)**
- **Issue**: Server used hardcoded default extensions instead of config file settings
- **Root Cause**: TOML key mismatch (`allowedextensions` vs `allowed_extensions`)  
- **Impact**: XMPP file uploads failing with "File extension not allowed" errors
- **Status**: ✅ **RESOLVED** - Configuration loading now works correctly

### **XMPP File Upload Failure**
- **Issue**: MP4 uploads from Conversations/Gajim clients returning HTTP 400 errors
- **Root Cause**: Network resilience changes broke configuration field mapping
- **Impact**: Mobile XMPP file sharing completely broken
- **Status**: ✅ **RESOLVED** - MP4 uploads now work perfectly (HTTP 201)

### **Mobile Network Switching**
- **Issue**: WLAN ↔ IPv6 5G switching configuration not loading properly
- **Root Cause**: Extension validation using wrong configuration source
- **Impact**: Network resilience features not fully functional
- **Status**: ✅ **RESOLVED** - Seamless network switching operational

---

## 🎯 What Was Fixed

### **Technical Resolution**
```bash
# Before (BROKEN)
Server Log: "🔥 DEBUG: Extension .mp4 not found in allowed list"
HTTP Response: 400 "File extension .mp4 not allowed"

# After (FIXED) 
Server Log: "✅ File extension .mp4 is allowed"
HTTP Response: 201 "Upload successful"
```

### **Configuration Fix Applied**
```toml
# BEFORE: Not working (wrong key name)
[uploads]
allowedextensions = [".mp4", ".mkv", ".avi"]  # ❌ Wrong key

# AFTER: Working (correct key name)  
[uploads]
allowed_extensions = [".mp4", ".mkv", ".avi"]  # ✅ Correct key
```

---

## 🧪 Comprehensive Testing Suite

### **New Testing Infrastructure**
- **✅ Consolidated Testing**: All scattered test scripts merged into single comprehensive suite
- **✅ 8 Test Scenarios**: Complete coverage of core functionality
- **✅ Auto-Detection**: Automatically finds local vs remote servers
- **✅ 100% Pass Rate**: All tests passing after fixes

### **Test Coverage**
```bash
./test  # Run all comprehensive tests

Test Results:
✅ Server Health Check (200)
✅ Basic HMAC Validation (201) 
✅ MP4 Upload for XMPP (201) ← CRITICAL FIX VALIDATED
✅ Image Upload (201)
✅ Large File Upload (201)
✅ Invalid HMAC Rejection (401) 
✅ Unsupported Extension Block (400)
✅ Network Resilience Metrics (200)
```

---

## 📁 Project Structure Cleanup

### **Root Directory Organization**
- **❌ Removed**: 10+ redundant backup files, duplicate configs, empty documentation
- **✅ Consolidated**: All test files moved to `/tests/` directory
- **✅ Enhanced**: README.md with complete installation and testing documentation
- **✅ Simplified**: Easy access via `./test` and `./quick-test` symlinks

### **Before/After Comparison**
```bash
# BEFORE: Cluttered root directory
comprehensive_upload_test.sh, debug-uploads.sh, test-*.sh
config-*.toml.backup.*, BUILD_GUIDE.md (empty)
LICENSE_NEW, xep0363_analysis.ipynb (empty)

# AFTER: Clean, organized structure
README.md, WIKI.MD, CHANGELOG.MD, LICENSE
tests/ (all test files consolidated)
./test → tests/comprehensive_test_suite.sh
./quick-test → tests/test-hmac-fixed.sh
```

---

## 🚀 Immediate Benefits

### **For XMPP Users**
- **✅ Conversations**: MP4 uploads working again
- **✅ Gajim**: Video file sharing restored  
- **✅ Mobile Users**: Seamless network switching between WiFi and 5G
- **✅ Large Files**: Multi-MB uploads functional

### **For Developers**
- **✅ Testing**: Single comprehensive test suite
- **✅ Debugging**: Clear, organized project structure
- **✅ Documentation**: All info consolidated in README.md
- **✅ Configuration**: Proper validation and error reporting

### **For System Administrators**
- **✅ Deployment**: All methods (SystemD, Docker, Podman) verified
- **✅ Monitoring**: Network resilience features operational
- **✅ Troubleshooting**: Comprehensive test suite for validation
- **✅ Maintenance**: Clean project structure for easier management

---

## ⚡ Upgrade Instructions

### **Critical Update (Recommended for All Users)**
```bash
# 1. Backup current setup
cp config.toml config-backup.toml

# 2. Update configuration key names
sed -i 's/allowedextensions/allowed_extensions/g' config.toml

# 3. Replace binary with 3.2.1 version
# Download new binary and restart service

# 4. Validate fix
./test  # Should show 100% pass rate
```

### **Validation Commands**
```bash
# Quick test - should return HTTP 201
./quick-test

# Full validation - all 8 tests should pass
./test

# Check XMPP specifically
curl -X PUT -H "Content-Type: video/mp4" \
  --data-binary "@test.mp4" \
  "https://your-server/path/test.mp4?v=hmac_value"
# Should return HTTP 201 instead of 400
```

---

## 🔧 Technical Details

### **Root Cause Analysis**
1. **Network Resilience Implementation**: Enhanced mobile switching features in 3.2
2. **Configuration Structure Changes**: Modified field mapping for new features  
3. **TOML Key Mismatch**: `allowedextensions` config vs `allowed_extensions` struct tag
4. **Fallback Behavior**: Server fell back to hardcoded defaults when config loading failed

### **Resolution Strategy**
1. **Configuration Fix**: Corrected TOML key naming to match struct expectations
2. **Validation Enhancement**: Added comprehensive configuration validation
3. **Testing Framework**: Created unified test suite to prevent regressions
4. **Documentation Update**: Consolidated all information for better maintenance

---

## 📊 Impact Assessment

### **Before 3.2.1 (BROKEN)**
- ❌ XMPP file uploads failing
- ❌ Mobile network switching unreliable  
- ❌ Configuration validation inconsistent
- ❌ Scattered test files, difficult debugging

### **After 3.2.1 (FIXED)**
- ✅ XMPP integration fully functional
- ✅ Network resilience features operational
- ✅ Configuration loading reliable
- ✅ Comprehensive testing infrastructure

---

## 🎉 Success Metrics

- **✅ 100% Test Pass Rate**: All functionality validated
- **✅ XMPP Compatibility**: Conversations & Gajim working perfectly
- **✅ Network Resilience**: 1-second mobile detection operational
- **✅ Project Quality**: Clean, organized, maintainable structure

---

> **3.2.1 restores full functionality while establishing a comprehensive testing framework to prevent future regressions. This critical fix ensures XMPP integration and mobile network resilience work as designed.**

---

*HMAC File Server 3.2.1 – Reliability Restored* 🛠️
