# HMAC-FILE-SERVER 2.0-dev (Pre-release)

> **Note:** This is a **development/pre-release** version for testing purposes.

---

## Key Features and Updates

1. **Auto-Adjust Worker Scaling**: Introduced a new `AutoAdjustWorkers` option under the `[server]` section. When enabled, the server automatically determines the optimal number of HMAC and ClamAV workers based on system resources.  
   - This feature overrides manually specified `NumWorkers` and `NumScanWorkers` settings, ensuring efficient resource utilization without manual tuning.

2. **MinFreeBytes Configuration**: Added a `MinFreeBytes` field to the `ServerConfig` struct, specifying the minimum free bytes required.
   - Example: `MinFreeBytes = 100 * (1 << 30) // 100 GB`

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

---

## Configuration Details

The server is configured via a `config.toml` file, allowing fine-grained control over various settings. The `AutoAdjustWorkers` option has been moved into the `[server]` block. When set to `true`, this feature automatically tunes both HMAC and ClamAV worker counts, ignoring manual worker settings.

Additionally, a new `NetworkEvents` option has been added to control whether the server logs and handles network-related events. Set `NetworkEvents = false` to disable monitoring and handling of IP changes or network activity logging.

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
AutoAdjustWorkers = true # Enable automatic tuning of worker counts
NetworkEvents = false # Disable monitoring and logging of network-related events

[timeouts]
ReadTimeout = "480s"
WriteTimeout = "480s"
IdleTimeout = "480s"

[security]
Secret = "changeme"

[versioning]
EnableVersioning = false
MaxVersions = 1

[uploads]
ResumableUploadsEnabled = true
ChunkedUploadsEnabled = true
ChunkSize = "32MB"
AllowedExtensions = [".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"]

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
- **Improved Logging**: Enhanced logging for better debugging during development.

---

> For testing and feedback, ensure all configurations are adapted to your environment and note that this is a development/pre-release version.
