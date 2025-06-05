# Release Notes for HMAC File Server 3.0

## [3.0] - 2025-06-05

### Added
- `--genconfig` and `--genconfig-path` flags to generate example configurations on demand.
- Robust dual-stack HTTP client with idle connection timeouts and keep-alive settings for seamless IPv4/IPv6 support.
- Automated end-to-end tests for `--genconfig` generation and IPv4/IPv6 startup behavior.
- Proxy best practices and recommendations (timeouts, buffer sizes, security headers) in documentation.
- Enhanced startup logging of `BindIP`, `FileNaming`, and `ForceProtocol` settings.

### Changed
- Consolidated all configuration structs into a single `Config` type; removed duplicated and unused fields.
- Improved `parseSize` and `parseTTL` functions with strict unit parsing and error handling.
- Fixed Markdown lint issues in `CHANGELOG.md` and `wiki.md` for consistent headings and formatting.
- Refined PID file write/remove logic and added graceful shutdown logging enhancements.

### Fixed
- Ensured seamless IPv4/IPv6 dual-stack handling with appropriate timeouts and error control.
- Validated `--config` flag usage with clear error messages and fallback behavior.
- Resolved edge cases in network protocol initialization preventing unexpected errors.

---

# Release Notes for HMAC File Server 2.7-Stable

## Summary
Version 2.6-Stable focuses on improving the overall stability and performance of the HMAC File Server. Significant changes have been made to prioritize reliability and scalability for production environments.

## Key Changes

### Breaking Changes
- **Thumbnail Generation Dropped**: Support for automatic thumbnail generation has been removed in this release. This decision was made to enhance system stability and reduce resource consumption. Users requiring thumbnails are encouraged to use external tools.

### New Features
- **ISO-Based Storage Support**: Introduced support for ISO-based storage to accommodate specialized use cases.
- **Enhanced ClamAV Integration**: Improved ClamAV scanning with concurrent workers, providing better performance for large-scale deployments.
- **Timeout Configuration**: Added granular timeout settings for read, write, and idle connections, improving connection management.
- **FileNaming Configuration**: Added support for a "None" option in the `FileNaming` configuration. When set to "None", the filename remains unchanged.
- **Example Configuration Generation**: If no configuration file is found, the server will output an example configuration for the user to copy and paste.
- **Prometheus Metrics**: Enhanced Prometheus metrics for better monitoring and performance tracking. New metrics include upload and download durations, error counts, memory and CPU usage, and more.

### Improvements
- **Worker Management**: Auto-scaling worker threads based on system load for optimal performance.
- **Logging Enhancements**: Improved log verbosity control, making debugging and monitoring easier.

### Bug Fixes
- Resolved minor issues affecting deduplication and file upload performance.
- Fixed a rare crash scenario during high-concurrency file uploads.

## Migration Notes
1. **Thumbnail Settings**: Remove `[thumbnails]` configuration blocks from your `config.toml` file to avoid errors.
2. **Updated Configuration**: Review new timeout settings in `[timeouts]` and adjust as needed.
3. **ISO Integration**: Configure the new `[iso]` block for environments utilizing ISO-based storage.
4. **FileNaming Configuration**: Update the `FileNaming` setting in `[server]` to use the new "None" option if you want filenames to remain unchanged.

[server]
# FileNaming options: "HMAC", "None"
FileNaming = "HMAC"

## Recommendations
- **Security**: Ensure that the HMAC secret key in `config.toml` is updated to a strong, unique value.
- **Backups**: Regularly back up your `config.toml` and important data directories.
- **Monitoring**: Leverage Prometheus metrics for real-time monitoring of server performance.

For a detailed guide on setting up and configuring the HMAC File Server, refer to the [README.md](./README.md).

---

Thank you for using HMAC File Server! If you encounter any issues, feel free to report them on our GitHub repository.

## Version 2.7

- Refinements in worker scaling logic
- Removed obsolete parameters for sorting
- Further improvements to ISO-based storage handling
- Fixed dual stack for upload (IPv4/IPv6)
