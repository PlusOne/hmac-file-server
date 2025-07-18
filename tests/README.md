# HMAC File Server Test Suite

This directory contains test scripts, monitoring tools, and test data files for the HMAC File Server.

## Test Scripts

### Protocol Testing
- `test_final_xmpp.sh` - Complete XEP-0363 protocol testing (all variants: v1, v2, v3, token)
- `test_xmpp_simulation.sh` - XMPP client simulation for upload testing
- `test_url_formats.sh` - URL format validation and testing
- `verify_xmpp_upload.sh` - XMPP upload verification script

### Performance Testing
- `comprehensive_upload_test.sh` - Comprehensive upload performance testing
- `test_upload_queue.sh` - Queue performance and concurrent upload testing
- `test_upload_completion.sh` - Upload completion and reliability testing

### Feature Testing
- `test_deduplication.sh` - File deduplication functionality testing
- `test_direct_connection.sh` - Direct server connection testing
- `test_path_discovery.sh` - Path discovery and routing testing

### Debugging & Monitoring
- `debug_upload.sh` - Upload debugging and troubleshooting script
- `monitor_server.sh` - Server status and performance monitoring
- `monitor_nginx.sh` - Nginx proxy monitoring
- `monitor_uploads.sh` - Upload activity monitoring

## Test Data Files

### Small Test Files
- `test_1mb.txt` / `test_1mb.bin` - 1MB test files for basic functionality
- `test_upload.txt` - Small text file for quick testing
- `chunk_0.bin` - Chunked upload test data

### Large Test Files
- `test_50mb.bin` - 50MB file for medium-size upload testing
- `test_215mb.bin` - 215MB file for large upload testing
- `test_4gb.bin` / `test_4gb.txt` - 4GB files for stress testing

## Analysis Tools

- `xep0363_analysis.ipynb` - Jupyter notebook for XEP-0363 protocol analysis

## Usage Examples

### Quick Protocol Test
```bash
cd tests
./test_final_xmpp.sh
```

### Performance Testing
```bash
cd tests
./comprehensive_upload_test.sh
./test_upload_queue.sh
```

### Deduplication Testing
```bash
cd tests
./test_deduplication.sh
```

### Monitor Server
```bash
cd tests
./monitor_server.sh
```

## Test Environment

These tests are designed to work with:
- HMAC File Server 3.2
- nginx reverse proxy (standard configuration)
- Extended timeout settings (4800s)
- Deduplication enabled
- Dynamic worker scaling

## Notes

- Large test files (4GB) are intended for stress testing extended timeout configurations
- All scripts include proper error handling and cleanup
- Monitor scripts provide real-time status information
- Test scripts validate both success and failure scenarios
