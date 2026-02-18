# HMAC File Server 3.4.0 Test Suite

This directory contains comprehensive testing tools for the HMAC File Server 3.4.0 "Cascade".

## 🚀 Quick Start

Run the complete test suite:
```bash
./comprehensive_test_suite.sh
```

## 📋 Test Coverage

The comprehensive test suite covers:

### ✅ Core Functionality
- **HMAC Validation**: Ensures proper authentication
- **File Extensions**: Tests allowed/blocked file types
- **Upload Mechanics**: Validates upload process
- **Server Health**: Checks service availability

### 🎥 XMPP Integration
- **MP4 Upload**: Tests video file sharing for XMPP clients
- **Image Upload**: Tests image sharing (PNG, JPEG)
- **File Size Limits**: Validates large file handling

### 🌐 Network Resilience (3.4.0 Features)
- **Health Monitoring**: Tests network resilience endpoints
- **Metrics Collection**: Validates monitoring capabilities
- **Mobile Switching**: Supports seamless network transitions

### 🚫 Security Testing
- **Invalid HMAC**: Ensures rejected authentication fails
- **Unsupported Extensions**: Confirms blocked file types
- **Path Validation**: Tests file path sanitization

## 🔧 Commands

```bash
# Run all tests
./comprehensive_test_suite.sh

# Setup test files only
./comprehensive_test_suite.sh setup

# Clean up test files
./comprehensive_test_suite.sh clean

# Show help
./comprehensive_test_suite.sh help
```

## 📊 Test Results

Tests generate detailed logs with:
- ✅ **Pass/Fail status** for each test
- 🕒 **Timestamps** for performance tracking
- 📝 **Detailed output** saved to `/tmp/hmac_test_results_*.log`
- 📈 **Summary statistics** (passed/failed counts)

## 🎯 Expected Results

When all systems are working correctly:
- **✅ PASS**: HMAC validation
- **✅ PASS**: MP4 upload (XMPP)
- **✅ PASS**: Image upload  
- **✅ PASS**: Large file upload
- **✅ PASS**: Server health check
- **❌ FAIL**: Invalid HMAC (should fail)
- **❌ FAIL**: Unsupported extension (should fail)

## 🔍 Troubleshooting

### Common Issues
1. **Connection refused**: Check if server is running
2. **403 Forbidden**: Verify HMAC key configuration
3. **400 Bad Request**: Check file extension configuration
4. **Timeout**: Large files may need adjusted timeouts

### Debug Mode
For detailed debugging, check server logs:
```bash
sudo journalctl -u hmac-file-server -f
```

## 📁 File Cleanup

The test suite automatically cleans up temporary files, but if needed:
```bash
rm -f /tmp/test_*.{txt,mp4,bin,png,xyz}
rm -f /tmp/hmac_test_results_*.log
```

## 🔧 Configuration

Tests use these defaults (modify in script if needed):
- **Base URL**: `https://xmpp.uuxo.net`
- **Test User**: `c184288b79f8b7a6f7d87ac7f1fb1ce6dcf49a80`
- **HMAC Key**: Configured in script

## 📝 Legacy Test Files

This comprehensive suite replaces these scattered root-level test files:
- `test-hmac-fixed.sh` → Integrated into comprehensive suite
- `test-upload.sh` → Covered by upload tests
- `debug-uploads.sh` → Debug logging integrated
- `comprehensive_upload_test.sh` → Replaced by this suite
- Various monitor scripts → Health checks integrated

## 🎉 3.4.0 "Cascade" Features Tested

- ✅ **Enhanced Network Resilience**: 1-second detection
- ✅ **Mobile Network Switching**: WLAN ↔ IPv6 5G seamless transitions
- ✅ **XMPP File Sharing**: Conversations/Gajim compatibility
- ✅ **Configuration Validation**: Proper extension loading
- ✅ **Production Deployment**: SystemD, Docker, Podman support
