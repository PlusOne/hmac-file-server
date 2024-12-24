# Release Notes - hmac-file-server v2.1-stable

**Release Date:** April 27, 2024

## Overview

We are excited to announce the release of **hmac-file-server v2.1-stable**. This version brings significant enhancements, new features, and important bug fixes to improve the performance, security, and usability of the HMAC File Server. Below are the detailed changes and updates included in this release.

## New Features

### 1. **ClamAV Integration**
- **Description:** Integrated ClamAV for enhanced malware scanning capabilities.
- **Benefits:**
  - Improved security by scanning uploaded files for viruses and malware.
  - Configurable number of scan workers to optimize performance.
- **Configuration:**
  ```toml
  [clamav]
  clamavenabled = true
  clamavsocket = "/var/run/clamav/clamd.ctl"
  numscanworkers = 4
  scanfileextensions = [".exe", ".dll", ".bin", ".com", ".bat", ".sh", ".php", ".js"]
  ```

### 2. **Redis Support**
- **Description:** Added support for Redis to enhance caching and data management.
- **Benefits:**
  - Improved performance with Redis as an external cache.
  - Enhanced scalability for handling high loads.
- **Configuration:**
  ```toml
  [redis]
  redisenabled = true
  redisdbindex = 0
  redisaddr = "localhost:6379"
  redispassword = ""
  redishealthcheckinterval = "120s"
  ```

### 3. **Enhanced Configuration Management**
- **Description:** Expanded configuration options for greater flexibility.
- **New Configuration Options:**
  - `precaching`: Enables pre-caching of frequently accessed files.
  - `networkevents`: Toggles the logging of network events for better monitoring.
- **Updated `config.toml`:**
  ```toml
  [server]
  precaching = true
  networkevents = false
  ```

### 4. **Improved Logging**
- **Description:** Enhanced logging capabilities with configurable log levels and formats.
- **Benefits:**
  - Better insights into server operations and issues.
  - Support for JSON-formatted logs for easier integration with log management systems.
- **Configuration:**
  ```toml
  [server]
  loglevel = "debug"
  logfile = "/var/log/hmac-file-server.log"
  loggingjson = false
  ```

## Enhancements

### 1. **Graceful Shutdown**
- **Description:** Implemented graceful shutdown procedures to ensure all ongoing processes complete before the server stops.
- **Benefits:**
  - Prevents data corruption and ensures consistency.
  - Enhances reliability during server restarts and shutdowns.

### 2. **Auto-Adjusting Worker Pools**
- **Description:** Introduced auto-adjustment for worker pools based on current load and resource availability.
- **Benefits:**
  - Optimizes resource usage.
  - Maintains optimal performance under varying loads.

### 3. **Extended Timeout Configurations**
- **Description:** Added configurable timeouts for read, write, and idle connections.
- **Configuration:**
  ```toml
  [timeouts]
  readtimeout = "3600s"
  writetimeout = "3600s"
  idletimeout = "3600s"
  ```

## Bug Fixes

- **Fixed:** Resolved issues with unused parameters and function calls in the handler modules.
- **Fixed:** Addressed syntax errors related to constant declarations and import paths in `main.go`.
- **Fixed:** Corrected configuration parsing to handle new and updated configuration fields effectively.
- **Fixed:** Improved error handling during Redis and ClamAV client initialization to prevent server crashes.

## Performance Improvements

- **Optimized:** Enhanced the upload and download handlers for faster file processing and reduced latency.
- **Optimized:** Improved caching mechanisms to decrease load times and increase throughput.

## Security Enhancements

- **Enhanced:** Strengthened security configurations by integrating ClamAV and enabling secure Redis connections.
- **Improved:** Secured sensitive information handling, ensuring secrets are managed appropriately.

## Configuration Changes

### Removed Deprecated Options
- **Deprecated:** Removed outdated configuration options that are no longer supported to streamline the configuration process.

### Updated Configuration Structure
- **Change:** Updated the configuration structure to align with the new features and enhancements, ensuring better clarity and maintainability.

## Known Issues

- **Issue:** Some users may experience delays in file processing when auto-adjusting worker pools under extreme loads. We are actively working on optimizing this feature.
- **Issue:** JSON-formatted logs may require additional parsing tools for integration with certain logging systems.

## Upgrade Instructions

1. **Backup Configuration:**
   - Ensure you have a backup of your current `config.toml` before proceeding with the upgrade.

2. **Update Application:**
   - Pull the latest version from the repository:
     ```sh
     git pull origin v2.1-stable
     ```
   - Alternatively, download the latest release from the [releases page](https://github.com/PlusOne/hmac-file-server/releases).

3. **Update Dependencies:**
   - Navigate to the project directory and run:
     ```sh
     go mod tidy
     ```

4. **Review Configuration:**
   - Compare your existing `config.toml` with the updated configuration file to incorporate new settings.

5. **Restart the Server:**
   - Restart the HMAC File Server to apply the updates:
     ```sh
     systemctl restart hmac-file-server
     ```

## Acknowledgments

We would like to thank our contributors and the community for their continuous support and valuable feedback, which have been instrumental in shaping this release.

## Support

For any issues or questions regarding this release, please open an issue on our [GitHub repository](https://github.com/PlusOne/hmac-file-server/issues) or contact our support team.

---

*Thank you for using hmac-file-server! We hope this release enhances your experience and meets your needs effectively.*
