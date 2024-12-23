# HMAC-FILE-SERVER 2.0-stable

## Key Features and Updates

1. **MinFreeBytes Configuration**: Added a `MinFreeBytes` field to the `ServerConfig` struct, specifying the minimum free bytes required.  
   - Example: `MinFreeBytes = 100 * (1 << 30) // 100 GB`

2. **Deduplication Feature**: Introduced a deduplication feature that automatically removes duplicate files based on hashing.  
   - Function: `DeduplicateFiles(storeDir string) error`  
   - Helper: `computeFileHash(filePath string) (string, error)`

3. **Network Monitoring**: Added functionality to monitor network changes and handle network events.  
   - Functions: `monitorNetwork(ctx context.Context)` and `handleNetworkEvents(ctx context.Context)`  
   - New Configuration: `NetworkEvents = false` disables monitoring of network-related events such as IP changes or logging network activity.

4. **ISO Mounting Feature**: Added support for managing ISO files with configurable options.  
   - Example:
     ```toml
     [iso]
     enabled = false
     size = "1TB"
     mountpoint = "/mnt/nfs_vol01/hmac-file-server/iso/"
     charset = "utf-8"
     ```

5. **ClamAV File Scanning Enhancements**: Support for specifying file extensions for ClamAV scanning, optimizing performance.  
   - Example:
     ```toml
     [clamav]
     clamavenabled = false
     clamavsocket = "/var/run/clamav/clamd.ctl"
     numscanworkers = 4
     scanfileextensions = [".exe", ".dll", ".bin", ".com", ".bat", ".sh", ".php", ".js"]
     ```

6. **Improved Error Handling and Logging**: Enhanced error handling and logging mechanisms throughout the codebase.

7. **Auto-Adjust Worker Scaling**: Introducing `AutoAdjustWorkers` under the `[server]` section automatically calculates and adjusts the number of workers (and ClamAV scan workers) based on system resources. When `AutoAdjustWorkers` is enabled, manual values for `NumWorkers` and `NumScanWorkers` are overridden.

---

## Configuration Details

The server is configured via a `config.toml` file. Below is the updated example configuration, including `AutoAdjustWorkers` and `NetworkEvents`:

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
AutoAdjustWorkers = true # When true, automatically calculates and adjusts worker counts
NetworkEvents = false # Disable monitoring of network-related events

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

- **Deduplication**: Removes duplicate files based on hashing.
- **Network Monitoring**: Monitors network changes and handles network events (can be disabled via `NetworkEvents`).
- **ISO Support**: Manage ISO files with customizable mount options, size, and charset.
- **ClamAV Integration**: Optional file scanning for critical file types.
- **AutoAdjustWorkers**: Dynamically adjusts worker counts based on system resources.
- **Improved Logging**: Enhanced logging for better debugging and development experience.
```
