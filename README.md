# HMAC File Server

A secure file server implementing HMAC authentication.

## Features

- HMAC-based authentication
- Chunked uploads and downloads
- ClamAV scanning
- Redis integration for caching
- Prometheus metrics

## Installation

```bash
go build -o hmac-file-server ./cmd/server
```

## Configuration

See [config.toml](./config/config.toml) for configuration options.

### FileTTL

The `FileTTL` parameter defines the time-to-live for files on the server. It supports the following units:

- `y` for years (e.g., `1y` for one year)
- `d` for days (e.g., `365d` for 365 days)
- `h` for hours
- `m` for minutes
- `s` for seconds

**Example:**

## Usage

```bash
./hmac-file-server -config=config.toml
```

## Testing

```bash
go test ./...
```

## ClamAV Virus Scanning

The server can optionally scan uploaded files for viruses using ClamAV. To enable this feature, set `ClamAVEnabled` to `true` in the configuration. Specify which file extensions should be scanned using the `ScanFileExtensions` setting.

```toml
[clamav]
ClamAVEnabled = true
ClamAVSocket = "/var/run/clamav/clamd.ctl"
ScanFileExtensions = [".exe", ".dll", ".js", ".php", ".scr", ".bat"]
```

Ensure that ClamAV is installed and the `clamd` service is running and accessible via the specified socket.

func handleUpload(w http.ResponseWriter, r *http.Request, conf *config.Config) {
// Use helper for worker pool handling
acquireWorker(HMACWorkerPool)
defer releaseWorker(HMACWorkerPool)

	queryParams := r.URL.Query()
	absFilename := queryParams.Get("file")
	if absFilename == "" {
		http.Error(w, "File parameter is missing", http.StatusBadRequest)
		return
	}

	storagePath := getStoragePath(absFilename, conf)
	logrus.Infof("Using storage path: %s", storagePath)

	// Extract HMAC Validation
	protocolVersion, err := getHMACProtocol(queryParams)
	if err != nil {
		http.Error(w, err.Error(), http.StatusForbidden)
		return
	}
	if !validateHMAC(r, storagePath, protocolVersion, conf) {
		http.Error(w, "Invalid HMAC", http.StatusForbidden)
		return
	}

	if !isExtensionAllowed(storagePath) {
		http.Error(w, "Invalid file extension", http.StatusBadRequest)
		return
	}

	// Handle storage space check
	minFreeBytes, err := parseSize(conf.Server.MinFreeBytes)
	if err != nil {
		logrus.Fatalf("Invalid MinFreeBytes: %v", err)
	}
	if err := checkStorageSpace(conf.Server.StoragePath, minFreeBytes); err != nil {
		http.Error(w, "Not enough free space", http.StatusInsufficientStorage)
		return
	}

	// Save uploaded file and calculate hash
	sha256Hash, tempFilePath, err := saveUploadedFile(r, storagePath)
	if err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}
	logrus.Infof("File uploaded and saved to temp path: %s", tempFilePath)

	// Handle Deduplication
	if conf.Server.DeduplicationEnabled {
		if handleDeduplication(storagePath, sha256Hash, conf) {
			return
		}
	}

	// Move temp file to final location
	if err := os.Rename(tempFilePath, storagePath); err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}

	// Handle ClamAV scanning
	if conf.ClamAV.ClamAVEnabled {
		ext := filepath.Ext(storagePath)
		if shouldScanExtension(ext, conf.ClamAV.ScanFileExtensions) {
			if !scanFile(storagePath) {
				http.Error(w, "Uploaded file is infected", http.StatusBadRequest)
				os.Remove(storagePath)
				return
			}
			logrus.Infof("ClamAV scan passed for file: %s", storagePath)
		}
	}

	// Handle ISO creation
	if conf.ISO.Enabled {
		go createISOAsync(storagePath, conf.ISO.Charset)
	}

	// Final response for successful upload
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("File uploaded successfully"))
}

## ISO Storage

The server can optionally store files in an ISO format. To enable this feature, set `Enabled` to `true` in the ISO configuration. Specify the size, mount point, and charset for the ISO.

```toml
[iso]
Enabled = true
Size = "2TB"
MountPoint = "/mnt/iso"
Charset = "utf-8"
```

Ensure that the specified mount point is writable and has sufficient space for the ISO size.

## Running the Server

To build and run the HMAC File Server, follow these steps:

1. **Build the Server:**

    ```bash
    go build -o hmac-file-server ./cmd/server
    ```

2. **Run the Server:**

    ```bash
    ./hmac-file-server -config config.toml
    ```

## Configuration

The server can be configured using the `config.toml` file or environment variables. Environment variables override the configuration file settings.

### Environment Variables

- `HMAC_SERVER_LISTENPORT`: Port the server listens on (default: `8080`)
- `HMAC_SECURITY_SECRET`: Secret key for HMAC authentication (default: `changeme`)
- `HMAC_REDIS_REDISENABLED`: Enable Redis (`true` or `false`, default: `true`)
- `HMAC_ISO_ENABLED`: Enable ISO storage (`true` or `false`, default: `false`)
- // ...additional environment variables...

## How It Works

The HMAC File Server is designed to securely handle file uploads and downloads using HMAC-based authentication. Below is an overview of its core functionalities:

1. **Authentication:**
   - Utilizes HMAC tokens to authenticate and authorize users for uploading and downloading files.

2. **File Handling:**
   - Supports chunked uploads and downloads to efficiently manage large files.
   - Implements deduplication to avoid storing duplicate files, saving storage space.

3. **Virus Scanning:**
   - Integrates with ClamAV to scan uploaded files for viruses, ensuring only clean files are stored.

4. **Caching:**
   - Uses Redis for caching frequently accessed data, enhancing performance and reducing latency.

5. **Metrics and Monitoring:**
   - Exposes Prometheus metrics to monitor server performance, worker pool status, and other key metrics.

6. **Configuration and Management:**
   - Offers a flexible configuration system via `config.toml` and environment variables.
   - Supports graceful shutdown to ensure all ongoing processes are completed before stopping the server.

## Functions

### `LoadConfig(configFile string) (*Config, error)`
Loads the server configuration from a TOML file and environment variables. It sets default values, reads the configuration, and validates the settings.

### `setDefaults()`
Defines default configuration values for various server settings to ensure the server operates with sensible defaults if specific configurations are not provided.

### `validateConfig(conf *Config) error`
Validates the loaded configuration to ensure all required fields are set correctly and that there are no conflicting or invalid settings.

### `initClamAV(socket string) (*clamd.Clamd, error)`
Initializes the ClamAV client by connecting to the specified socket and ensuring ClamAV is reachable.

### `initRedis(ctx context.Context, conf config.RedisConfig) *redis.Client`
Initializes the Redis client based on the provided configuration and verifies the connection by pinging the Redis server.

### `SetupLogging(level string, file string)`
Configures the logging settings based on the specified log level and log file path.

### `LogSystemInfo(version string)`
Logs system information, including the server version and other relevant details.

### `CleanupExpiredFiles(ctx context.Context, storagePath string, fileTTL string)`
Periodically cleans up expired files from the storage path based on the defined time-to-live (TTL).

### `SetupRouter(conf *Config, deps *HandlerDependencies) *mux.Router`
Sets up the HTTP router with all necessary routes and handlers, injecting required dependencies.

### `GracefulShutdown(server *http.Server, ctx context.Context, cancel context.CancelFunc)`
Handles graceful shutdown of the server, ensuring all ongoing requests are completed before stopping.

## Configuration Options

The HMAC File Server can be configured using the `config.toml` file or environment variables. Below are the available configuration options along with their descriptions:

### Server Configuration

- **ListenPort** (`string`):  
  Port on which the server listens for incoming HTTP requests.  
  *Default:* `8080`

- **UnixSocket** (`bool`):  
  Enable listening on a Unix socket instead of a TCP port.  
  *Default:* `false`

- **StoragePath** (`string`):  
  Directory path where uploaded files are stored.  
  *Default:* `./uploads`

- **LogLevel** (`string`):  
  Specifies the logging level (e.g., `info`, `debug`, `error`).  
  *Default:* `info`

- **LogFile** (`string`):  
  Path to the log file. If not set, logs are output to the console.  
  *Default:* `""`

- **MetricsEnabled** (`bool`):  
  Enable or disable Prometheus metrics endpoint.  
  *Default:* `true`

- **MetricsPort** (`string`):  
  Port on which the metrics are exposed.  
  *Default:* `9090`

- **FileTTL** (`string`):  
  Time-to-live for files stored on the server. Supports units like `y` (years), `d` (days), `h` (hours), `m` (minutes), `s` (seconds).  
  *Default:* `365d`

- **MinFreeBytes** (`string`):  
  Minimum free disk space required in the storage path.  
  *Default:* `100MB`

- **DeduplicationEnabled** (`bool`):  
  Enable or disable file deduplication to prevent storing duplicate files.  
  *Default:* `true`

- **AutoAdjustWorkers** (`bool`):  
  Automatically adjust the number of worker pool workers based on system resources.  
  *Default:* `true`

- **NetworkEvents** (`bool`):  
  Enable or disable logging of network events.  
  *Default:* `false`

- **TempPath** (`string`):  
  Path for storing temporary files during upload and download processes.  
  *Default:* Not set

### Timeout Configuration

- **ReadTimeout** (`string`):  
  Maximum duration for reading the entire request, including the body.  
  *Default:* `4800s`

- **WriteTimeout** (`string`):  
  Maximum duration before timing out writes of the response.  
  *Default:* `4800s`

- **IdleTimeout** (`string`):  
  Maximum amount of time to wait for the next request when keep-alives are enabled.  
  *Default:* `65s`

### Security Configuration

- **Secret** (`string`):  
  Secret key used for generating and validating HMAC tokens.  
  *Default:* `changeme`

### Versioning Configuration

- **EnableVersioning** (`bool`):  
  Enable or disable file versioning to keep track of multiple versions of the same file.  
  *Default:* `false`

- **MaxVersions** (`int`):  
  Maximum number of file versions to retain.  
  *Default:* `1`

### Uploads Configuration

- **ResumableUploadsEnabled** (`bool`):  
  Enable or disable support for resumable uploads.  
  *Default:* `false`

- **ChunkedUploadsEnabled** (`bool`):  
  Enable or disable support for chunked uploads.  
  *Default:* `true`

- **ChunkSize** (`string`):  
  Size of each upload chunk.  
  *Default:* `8192`

- **AllowedExtensions** (`[]string`):  
  List of permitted file extensions for upload.  
  *Default:*  
  `[".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp", ".wav", ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".mpeg", ".mpg", ".m4v", ".3gp", ".3g2", ".mp3", ".ogg"]`

### Downloads Configuration

- **ResumableDownloadsEnabled** (`bool`):  
  Enable or disable support for resumable downloads.  
  *Default:* `false`

- **ChunkedDownloadsEnabled** (`bool`):  
  Enable or disable support for chunked downloads.  
  *Default:* `false`

- **ChunkSize** (`string`):  
  Size of each download chunk.  
  *Default:* Not set

### ClamAV Configuration

- **ClamAVEnabled** (`bool`):  
  Enable or disable virus scanning of uploaded files using ClamAV.  
  *Default:* `false`

- **ClamAVSocket** (`string`):  
  Socket path for connecting to the ClamAV daemon.  
  *Default:* `/var/run/clamav/clamd.ctl`

- **NumScanWorkers** (`int`):  
  Number of worker pools dedicated to scanning files with ClamAV.  
  *Default:* `4`

- **ScanFileExtensions** (`[]string`):  
  File extensions that should be scanned for viruses.  
  *Default:*  
  `[".exe", ".dll", ".js", ".php", ".scr", ".bat"]`

### Redis Configuration

- **RedisEnabled** (`bool`):  
  Enable or disable Redis for caching purposes.  
  *Default:* `true`

- **RedisAddr** (`string`):  
  Address of the Redis server.  
  *Default:* `localhost:6379`

- **RedisPassword** (`string`):  
  Password for authenticating with the Redis server.  
  *Default:* `""`

- **RedisDBIndex** (`int`):  
  Redis database index to use.  
  *Default:* `0`

- **RedisHealthCheckInterval** (`string`):  
  Interval at which Redis health checks are performed.  
  *Default:* `120s`

### Workers Configuration

- **NumWorkers** (`int`):  
  Number of workers in the HMAC worker pool.  
  *Default:* `4`

- **UploadQueueSize** (`int`):  
  Size of the upload queue to handle concurrent uploads.  
  *Default:* `50`

### File Configuration

- **FileRevision** (`int`):  
  Revision number for file versioning.  
  *Default:* Not set

### ISO Configuration

- **Enabled** (`bool`):  
  Enable or disable storing files in ISO format.  
  *Default:* `false`

- **Size** (`string`):  
  Size of the ISO to be created.  
  *Default:* `2TB`

- **MountPoint** (`string`):  
  Mount point for the ISO storage.  
  *Default:* `/mnt/iso`

- **Charset** (`string`):  
  Character set used for the ISO.  
  *Default:* `utf-8`

// ...existing content...