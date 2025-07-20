# Changelog

> **Note:** This file is a technical changelog for developers and maintainers. For user-focused highlights, migration notes, and upgrade instructions, see [README.MD](./README.MD).

All notable changes to this project will be documented in this file.

## [3.2.1] - Bug Fix Release - 2025-07-20

### Fixed (3.2.1)
- 🐛 **CRITICAL: Configuration Loading Regression**: Fixed TOML key mismatch where `allowedextensions` in config didn't map to `allowed_extensions` struct tag, causing server to use hardcoded default extensions instead of config file settings
- 🐛 **XMPP File Upload Failure**: Resolved 400 "File extension .mp4 not allowed" errors for XMPP clients (Conversations, Gajim) - MP4 uploads now work correctly
- 🐛 **Network Resilience Configuration**: Fixed configuration loading issues introduced with network resilience features that prevented proper extension validation
- 🐛 **Mobile Network Switching**: Ensured seamless WLAN ↔ IPv6 5G switching functionality works correctly with proper configuration loading

### Added (3.2.1)
- ✨ **Comprehensive Test Suite**: Consolidated all scattered test scripts into single `/tests/comprehensive_test_suite.sh` with 8 comprehensive test scenarios
- ✨ **Auto-Detection Testing**: Test suite automatically detects local vs remote server endpoints
- ✨ **Project Structure Cleanup**: Removed 10+ redundant files, organized all tests in `/tests/` directory
- ✨ **Universal Installation Documentation**: Enhanced README.md with complete installation framework and testing information

### Changed (3.2.1)
- 🔄 **Root Directory Organization**: Cleaned up project root by consolidating documentation and removing backup files
- 🔄 **Test Accessibility**: Added convenient `./test` and `./quick-test` symlinks for easy testing
- 🔄 **Documentation Consolidation**: Merged installation framework and release notes into main README.md

### Validated (3.2.1)
- ✅ **XMPP Integration**: MP4 uploads working for Conversations and Gajim clients
- ✅ **Network Resilience**: 1-second mobile network detection functional
- ✅ **Large File Support**: 1MB+ file uploads working with proper extensions
- ✅ **Security Testing**: Invalid HMAC and unsupported extensions correctly rejected
- ✅ **Multi-Architecture**: SystemD, Docker, and Podman deployments verified

## [3.2] - Stable Release - 2025-06-13

### Added (3.2)
- **Enhanced Installer Script**: Major improvements to the automated installer
  - **Docker Deployment Option**: Complete Docker deployment workflow as alternative to native installation
  - **Selectable Configuration Directory**: Users can now customize config directory instead of hardcoded paths
  - **Installation Type Selection**: Choice between native systemd service or Docker deployment
  - **Automated Docker Setup**: Generates docker-compose.yml, Dockerfile, and management scripts
- **Enhanced Documentation**: Comprehensive update of all documentation files to match current codebase
- **Protocol Specification Updates**: Detailed protocol documentation with implementation examples
- **Configuration Validation**: Improved configuration structure validation and error reporting
- **Developer Resources**: Updated build instructions and development setup guides
- **Repository Cleanup**: Comprehensive .gitignore for all major IDEs and development tools

### Changed (3.2)
- **Installer User Experience**: 
  - Removed all Unicode symbols and emoticons for universal terminal compatibility
  - Eliminated duplicate output during installation completion
  - Streamlined configuration process with better prompts
- **Documentation Structure**: Reorganized documentation for better clarity and maintenance
- **Configuration Examples**: Updated all configuration examples to reflect current options
- **API Documentation**: Enhanced API endpoint documentation with comprehensive examples

### Fixed (3.2)
- **Installer Compatibility**: Removed Unicode dependencies ensuring compatibility with all terminal types
- **Output Duplication**: Fixed duplicate completion messages in installer workflow
- **Path Configuration**: Enhanced flexibility in directory structure setup

### Completed (3.2)
- **Feature Development**: Active development of new features and improvements
- **Testing Enhancements**: Expanded test coverage and validation
- **Performance Optimizations**: Ongoing performance improvements and monitoring

---

## [3.1-Stable] - 2025-06-08

### Added (3.1)
- **v3 (mod_http_upload_external) Support**: Implemented secure file uploads using HMAC-SHA256 validation and expiration checks, specifically designed for Prosody's mod_http_upload_external compatibility.
- **JWT Authentication**: Complete JWT token authentication system with configurable algorithms and expiration times.
- **Multiple Authentication Protocols**: Support for legacy v1, enhanced v2, token-based, and v3 HMAC protocols alongside JWT authentication.
- **File Naming Strategy**: Configurable file naming options including HMAC-based, original filename preservation, or no specific naming convention.
- **Advanced Configuration Structure**: Comprehensive configuration sections including server, security, uploads, downloads, logging, deduplication, ISO, timeouts, versioning, ClamAV, Redis, and workers.

### Changed (3.1)
- **Enhanced HMAC Validation**: Improved validation logic to support multiple protocol versions (v1, v2, token, v3) with proper fallback mechanisms.
- **Authentication Priority**: Implemented authentication priority system with JWT taking precedence when enabled, falling back to HMAC protocols.
- **Network Protocol Support**: Enhanced IPv4/IPv6 dual-stack support with protocol forcing options (ipv4, ipv6, auto).
- **Configuration Hot-Reloading**: Added support for reloading logging configuration via SIGHUP signal without full server restart.

### Fixed (3.1)
- **Protocol Compatibility**: Addressed compatibility issues with different HMAC protocol versions and mod_http_upload_external clients.
- **Error Handling**: Improved error handling for invalid or expired signatures during file uploads.
- **Configuration Validation**: Enhanced configuration validation to prevent common misconfigurations.

---

## [3.0-Stable] - 2025-06-07

### Added (3.0)
- Official Docker Compose support and example (`dockerenv/docker-compose.yml`).
- Multi-stage Dockerfile for minimal images (`dockerenv/dockerbuild/Dockerfile`).
- Extended documentation for Docker, Compose, and deployment paths.
- Quickstart and configuration examples for containerized environments.
- Monitoring and Prometheus metrics documentation improvements.
- **Seamless IPv4 and IPv6 support:** The server now automatically supports both IPv4 and IPv6 connections out of the box, with improved dual-stack handling and configuration via `forceprotocol`.

### Changed (3.0)
- Minimum Go version is now **1.24** (was 1.20).
- Updated all documentation and config examples to reflect new version and Docker usage.
- Improved configuration normalization and environment variable overrides for containers.
- Enhanced worker pool and resource auto-scaling logic.

### Fixed (3.0)
- Minor bugfixes for config parsing and Docker path handling.
- Improved error messages for missing or invalid configuration in container environments.

---

## [2.8-Stable] - 2026-05-01

### Added (2.8)
- Version check history for improved tracking.
- Enhanced ClamAV scanning with concurrent workers.

### Changed (2.8)
- Improved ISO-based storage for specialized use cases.
- Auto-scaling workers for optimized performance.

### Fixed (2.8)
- Minor issues in worker thread adjustments under high load.

---

## [2.7] - 2026-02-10

### Added (2.7)
- Concurrency improvements and auto-scaling worker enhancements
- Cleanup and removal of unused parameters in sorting functions

### Changed (2.7)
- Additional logging for file scanning operations

### Fixed (2.7)
- Minor stability issues related to ISO container mounting
- Fixed dual stack for upload (IPv4/IPv6)

---

## [2.6-Stable] - 2025-12-01

### Added (2.6)
- Deduplication support (removes duplicate files).
- ISO Container management.
- Dynamic worker scaling based on CPU & memory.
- PreCaching feature for faster file access.

### Changed (2.6)
- Worker pool scaling strategies for better performance.
- Enhanced logging with rotating logs using lumberjack.

### Fixed (2.6)
- Temporary file handling issues causing "Unsupported file type" warnings.
- MIME type checks for file extension mismatches.

---

## [2.5] - 2025-09-15

### Added (2.5)
- Redis caching integration for file metadata.
- ClamAV scanning for virus detection before finalizing uploads.

### Changed (2.5)
- Extended the default chunk size for chunked uploads.
- Updated official documentation links.

### Fixed (2.5)
- Edge case with versioning causing file rename conflicts.

---

## [2.0] - 2025-06-01

### Added (2.0)
- Chunked file uploads and downloads.
- Resumable upload support with partial file retention.

### Changed (2.0)
- Moved configuration management to Viper.
- Default Prometheus metrics for tracking memory & CPU usage.

### Fixed (2.0)
- Race conditions in file locking under heavy concurrency.

---

## [1.0] - 2025-01-01

### Added (1.0)
- Initial release with HMAC-based authentication.
- Basic file upload/download endpoints.
- Logging and fundamental configuration using .toml files.
