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

Ensure that all necessary environment variables are set appropriately before running the server.

// ...existing content...