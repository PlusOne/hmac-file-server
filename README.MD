# HMAC File Server

**HMAC File Server** is a secure, scalable, and feature-rich file server with advanced capabilities like HMAC authentication, resumable uploads, chunked uploads, file versioning, and optional ClamAV scanning for file integrity and security. This server is built with extensibility and operational monitoring in mind, including Prometheus metrics support and Redis integration.

> **Credits:** The **HMAC File Server** is based on the source code of [Thomas Leister's prosody-filer](https://github.com/ThomasLeister/prosody-filer). Many features and design elements have been inspired or derived from this project.

---

## Features

- **HMAC Authentication:** Secure file uploads and downloads with HMAC tokens.
- **File Versioning:** Enable versioning for uploaded files with configurable retention.
- **Chunked and Resumable Uploads:** Handle large files efficiently with support for resumable and chunked uploads.
- **ClamAV Scanning:** Optional virus scanning for uploaded files.
- **Prometheus Metrics:** Monitor system and application-level metrics.
- **Redis Integration:** Use Redis for caching or storing application states.
- **File Expiration:** Automatically delete files after a specified TTL.
- **Graceful Shutdown:** Handles signals and ensures proper cleanup.
- **Deduplication:** Remove duplicate files based on hashing for storage efficiency.
- **Auto-Adjust Worker Scaling:** Dynamically optimize HMAC and ClamAV workers based on system resources when enabled.

---

## Repository

- **Primary Repository**: [GitHub Repository](https://github.com/PlusOne/hmac-file-server)
- **Alternative Repository**: [uuxo.net Git Repository](https://git.uuxo.net/uuxo/hmac-file-server)

---

## Installation

### Prerequisites

- Go 1.20+
- Redis (optional, if Redis integration is enabled)
- ClamAV (optional, if file scanning is enabled)

### Clone and Build

```bash
# Clone from the primary repository
git clone https://github.com/PlusOne/hmac-file-server.git

# OR clone from the alternative repository
git clone https://git.uuxo.net/uuxo/hmac-file-server.git

cd hmac-file-server
go build -o hmac-file-server main.go
```

---

## Configuration

The server configuration is managed through a `config.toml` file. Below are the supported configuration options:

### Auto-Adjust Feature

When `AutoAdjustWorkers` is enabled, the number of workers for HMAC operations and ClamAV scans is dynamically determined based on system resources. This ensures efficient resource utilization.

If `AutoAdjustWorkers = true`, the values for `NumWorkers` and `NumScanWorkers` in the configuration file will be ignored, and the server will automatically adjust these values.

### Network Events Monitoring

Setting `NetworkEvents = false` in the server configuration disables the logging and tracking of network-related events within the application. This means that functionalities such as monitoring IP changes or recording network activity will be turned off.

### Precaching

The `precaching` feature allows the server to pre-cache storage paths for faster access. This can improve performance by reducing the time needed to access frequently used storage paths.

### Thumbnail Creation

Set `thumbnail` to true in the `[server]` section of `config.toml` to enable image thumbnail generation.

### Deduplication

Set `enabled` to true in the `[deduplication]` section of `config.toml` to enable file deduplication. Specify the `directory` where deduplicated files will be stored.

---

## Example `config.toml`

Below is an example configuration file (config.toml) you can use as a reference (with sensitive data removed):

```toml
[server]
listenport = "8080"
unixsocket = false
storagepath = "/mnt/nfs_vol01/hmac-file-server/"
loglevel = "debug"
logfile = "/var/log/hmac-file-server.log"
metricsenabled = true
metricsport = "9090"
deduplicationenabled = true
minfreebytes = "5GB"
filettl = "2Y"
filettlenabled = true  # Enable or disable file TTL
autoadjustworkers = true
networkevents = false
pidfilepath = "./hmac-file-server.pid"
precaching = true

[deduplication]
enabled = true
directory = "/mnt/nfs_vol01/hmac-storage/deduplication/"

[thumbnails]
enabled = true
directory = "/mnt/nfs_vol01/hmac-file-server/thumbnails/"
size = "200x200"

[iso]
enabled = false
size = "1TB"
mountpoint = "/mnt/nfs_vol01/hmac-file-server/iso/"
charset = "utf-8"

[timeouts]
readtimeout = "3600s"
writetimeout = "3600s"
idletimeout = "3600s"

[security]
secret = "changeme"

[versioning]
enableversioning = false
maxversions = 1

[uploads]
resumableuploadsenabled = false
chunkeduploadsenabled = true
chunksize = "32MB"
allowedextensions = [
    ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif",
    ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4",
    ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm",
    ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"
]

[downloads]
chunkeddownloadsenabled = false
chunksize = "32MB"
allowedextensions = [
    ".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif",
    ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4",
    ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm",
    ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"
]

[clamav]
clamavenabled = true
clamavsocket = "/var/run/clamav/clamd.ctl"
numscanworkers = 4
scanfileextensions = [
    ".exe", ".dll", ".bin", ".com", ".bat",
    ".sh", ".php", ".js"
]

[redis]
redisenabled = true
redisdbindex = 0
redisaddr = "localhost:6379"
redispassword = ""
redishealthcheckinterval = "120s"

[workers]
numworkers = 4
uploadqueuesize = 5000

[file]
filerevision = 1
```

---

## Running the Server

### Basic Usage

Run the server with a configuration file:

```bash
./hmac-file-server -config ./config.toml
```

---

### Metrics Server

If `MetricsEnabled` is set to `true`, the Prometheus metrics server will be available on the port specified in `MetricsPort` (default: `9090`).

---

## Testing

To run the server locally for development:

```bash
go run main.go -config ./config.toml
```

Use tools like **cURL** or **Postman** to test file uploads and downloads.

### Example File Upload with HMAC Token

```bash
curl -X PUT -H "Authorization: Bearer <HMAC-TOKEN>" -F "file=@example.txt" http://localhost:8080/uploads/example.txt
```

Replace `<HMAC-TOKEN>` with a valid HMAC signature generated using the configured `Secret`.

---

## Monitoring

Prometheus metrics include:
- File upload/download durations
- Memory usage
- CPU usage
- Active connections
- HTTP requests metrics (total, method, path)

---

## Additional Features

- **Deduplication**: Automatically remove duplicate files based on hashing.
- **Versioning**: Store multiple versions of files and keep a maximum of `MaxVersions` versions.
- **ClamAV Integration**: Scan uploaded files for viruses using ClamAV.
- **Redis Caching**: Utilize Redis for caching file metadata for faster access.
- **Auto-Adjust Worker Scaling**: Optimize the number of workers dynamically based on system resources.

---

### Overview of other Projects (xep0363)

| Feature/Project             | HMAC FS | mod_http_upload_ext | xmpp-http-upload (horazont) | Prosody Filer | ngx_http_upload | xmpp-http-upload (nyovaya) |
|-----------------------------|---------|----------------------|-----------------------------|---------------|----------------|----------------------------|
| **Lang**                    | Go      | PHP                  | Python                      | Go            | C (Nginx)       | Python                     |
| **Env**                     | Standalone | Prosody module       | Standalone                 | Standalone    | Nginx          | Standalone                 |
| **XMPP**                    | No      | Yes                  | Yes                         | Yes           | No             | Yes                        |
| **Ext. Storage**            | Yes     | No                   | Possible via plugins        | No            | No             | Yes                        |
| **Auth / Security**         | HMAC    | Token-based          | Token-based                 | None          | Basic / None    | Token-based                |

---

## Build & Run
1. Clone the repository.
2. Build the server:  
   go build -o hmac-file-server cmd/server/main.go
3. Run the server:  
   ./hmac-file-server --config ./cmd/server/config.toml