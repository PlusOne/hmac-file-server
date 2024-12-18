# HMAC File Server

// ...existing content...

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
}## ISO Storage

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

Ensure that all necessary environment variables are set appropriately before running the server.

// ...existing content...