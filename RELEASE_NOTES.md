# HMAC-FILE-SERVER 2.0-dev (Pre-release)

> **Note:** This is a **development/pre-release** version for testing purposes.

---

## Key Features and Updates

1. **Auto-Adjust Worker Scaling**: Introduced a new `AutoAdjustWorkers` option under the `[server]` section. When enabled, the server automatically determines the optimal number of HMAC and ClamAV workers based on system resources.
   - This feature overrides manually specified `NumWorkers` and `NumScanWorkers` settings, ensuring efficient resource utilization without manual tuning.

2. **MinFreeBytes Configuration**: Added a `MinFreeBytes` field to the `ServerConfig` struct, specifying the minimum free bytes required.
   - Example: `MinFreeBytes = "100MB"`

3. **Deduplication Feature**: Introduced a deduplication feature that automatically removes duplicate files based on hashing.
   - Functions added: `DeduplicateFiles(storeDir string) error` and `computeFileHash(filePath string) (string, error)`

4. **Network Monitoring**: Added functionality to monitor network changes and handle network events.
   - Added `NetworkEvents` configuration option to toggle this feature on or off.
   - Functions added: `monitorNetwork(ctx context.Context)` and `handleNetworkEvents(ctx context.Context)`

5. **ISO Mounting Feature**: Added support for managing ISO files with configurable options.
   - Configuration example:
     ```toml
     [iso]
     enabled = false
     size = "1TB"
     mountpoint = "/mnt/nfs_vol01/hmac-file-server/iso/"
     charset = "utf-8"
     ```

6. **ClamAV File Scanning Enhancements**: Support for specifying file extensions for ClamAV scanning, optimizing performance.
   - Example Configuration:
     ```toml
     [clamav]
     clamavenabled = false
     clamavsocket = "/var/run/clamav/clamd.ctl"
     numscanworkers = 4
     scanfileextensions = [".exe", ".dll", ".bin", ".com", ".bat", ".sh", ".php", ".js"]
     ```

7. **Improved Error Handling and Logging**: Enhanced error handling and logging mechanisms throughout the codebase.

8. **Log Rate Limiter**: Introduced a new `LogRateLimiter` option under the `[server]` section. When enabled, the server limits repetitive log messages to prevent log flooding.
   - Set `LogRateLimiter = true` to enable this feature.
   - Helps maintain cleaner logs in high-traffic environments.

---

## Configuration Details

The server is configured via a `config.toml` file, allowing fine-grained control over various settings. The `AutoAdjustWorkers` option has been moved into the `[server]` block. When set to `true`, this feature automatically tunes both HMAC and ClamAV worker counts, ignoring manual worker settings.

The new `NetworkEvents` option controls whether the server logs and handles network-related events. Set `NetworkEvents = false` to disable monitoring and handling of IP changes or network activity logging.

A new `LogRateLimiter` option has been added to the `[server]` section. When set to `true`, it enables rate limiting for certain log messages, preventing excessive logging in high-traffic scenarios.

### Example `config.toml`

```toml
[server]
ListenPort = "8080"
UnixSocket = false
StoragePath = "./uploads"
LogLevel = "info"
LogFile = ""
MetricsEnabled = true
MetricsPort = "9090"
FileTTL = "1y"
DeduplicationEnabled = true
MinFreeBytes = "100MB"
AutoAdjustWorkers = true    # Enable automatic tuning of worker counts
NetworkEvents = false       # Disable monitoring and logging of network-related events
LogRateLimiter = true       # Enable log rate limiting

[timeouts]
ReadTimeout = "480s"
WriteTimeout = "480s"
IdleTimeout = "65s"

[security]
Secret = "changeme"

[versioning]
EnableVersioning = false
MaxVersions = 1

[uploads]
ResumableUploadsEnabled = true
ChunkedUploadsEnabled = true
ChunkSize = "32MB"
AllowedExtensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp",
".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v",
".3gp", ".3g2", ".mp3", ".ogg"]

[clamav]
ClamAVEnabled = false
ClamAVSocket = "/var/run/clamav/clamd.ctl"
NumScanWorkers = 2
ScanFileExtensions = [".exe", ".dll", ".bin", ".com", ".bat", ".sh", ".php", ".js"]

[redis]
RedisEnabled = false
RedisAddr = "localhost:6379"
RedisPassword = ""
RedisDBIndex = 0
RedisHealthCheckInterval = "120s"

[workers]
NumWorkers = 4
UploadQueueSize = 5000

[iso]
Enabled = false
Size = "2TB"
MountPoint = "/mnt/iso"
Charset = "utf-8"
```

---

## Additional Features

- **Deduplication**: Automatically removes duplicate files based on hashing.
- **Network Monitoring**: Monitors network changes and handles network events (configurable via `NetworkEvents`).
- **ISO Support**: Manages ISO files with customizable mount options, size, and character sets.
- **ClamAV Integration**: Optional file scanning, focusing on critical file types to optimize performance.
- **Improved Logging**: Enhanced logging with rate limiting to prevent log flooding in high-traffic environments.
- **Log Rate Limiter**: New feature to limit repetitive log messages, improving log manageability.

---

> For testing and feedback, ensure all configurations are adapted to your environment and note that this is a development/pre-release version.

### Release Notes (Enhanced Multi-Page Monitoring Dashboard)

#### New Features:
- **Multi-Page Monitoring Dashboard**:
  - Introduced a **dual-page** interface accessible via keybindings:
    - **System Page**: Provides real-time system metrics, Prometheus data, and top processes.
    - **HMAC Page**: Focused on `hmac-file-server` metrics, with relevant Prometheus data.
    - **Log File Page**: Displays the log file content dynamically.

- **Keybindings for Seamless Navigation**:
  - **`s` or `S`**: Navigate to the **System Page**.
  - **`h` or `H`**: Switch to the **HMAC Page**.
  - **`l` or `L`**: Switch to the **Log File Page**.
  - **`q` or `Q`**: Exit the application.

#### Enhancements:
- **Dynamic Real-Time Updates**:
  - Refreshes every 2 seconds, ensuring the latest metrics are displayed.
  
- **Optimized Process Display**:
  - Top processes sorted by **CPU usage**, limited to 20 entries for improved clarity.

- **Color-Coded Metric Thresholds**:
  - Intuitive visuals for CPU and memory utilization.

- **Configurable Prometheus URL and Log File Path**:
  - Dynamically determined from a configuration file:
    - Primary: `/etc/hmac-file-server/config.toml`
    - Secondary: `../config.toml`
  - Ensures flexibility and adaptability to varying setups.

- **Fallback Logic for Configuration**:
  - Uses default values if `server.metrics_port` or `server.log_file` is missing in the configuration.
  - Logs a fatal error if no valid configuration file is found.

#### Known Limitations:
- **Dependency on `hmac-file-server`**:
  - Displays a message if the `hmac-file-server` process is not detected.

- **Monitor Bundled in Deb Package**:
  - The monitor is bundled in the deb package with the release stable 2.0.
