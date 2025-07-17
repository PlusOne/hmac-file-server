# Build Guide - HMAC File Server with Network Resilience

## ✅ Quick Build (Working)

### 1. Standard Build with Network Resilience
```bash
# Build with all features (including network resilience)
./buildgo.sh
```

**Output:** 
```
[BUILD] Building HMAC File Server v3.2 with Network Resilience...
[INFO] Found network resilience: upload_session.go
[INFO] Found network resilience: network_resilience.go
[INFO] Found network resilience: chunked_upload_handler.go
[INFO] Found network resilience: integration.go
[BUILD] Build successful! Binary created: ./hmac-file-server
[INFO] Binary size: 16M
```

### 2. Manual Build (Alternative)
```bash
# Build manually with all network resilience features
go build -o hmac-file-server \
  cmd/server/main.go \
  cmd/server/helpers.go \
  cmd/server/config_validator.go \
  cmd/server/config_test_scenarios.go \
  cmd/server/upload_session.go \
  cmd/server/network_resilience.go \
  cmd/server/chunked_upload_handler.go \
  cmd/server/integration.go
```

## Build Requirements

### Prerequisites
- **Go 1.24+** (as specified in go.mod)
- **OpenSSL** (optional, for HMAC testing)
- **Redis** (optional, for session persistence)

### Dependencies
All dependencies are handled by Go modules:
```bash
# Download dependencies
go mod download

# Verify dependencies
go mod verify

# View dependency tree
go mod graph
```

## Build Options

### Development Build
```bash
# Build with debug information
go build -gcflags="all=-N -l" -o hmac-file-server-debug cmd/server/*.go

# Or use the build script in debug mode
DEBUG=1 ./buildgo.sh
```

### Production Build
```bash
# Optimized production build
go build -ldflags="-s -w" -o hmac-file-server cmd/server/*.go

# With version information
VERSION="3.2.1"
go build -ldflags="-s -w -X main.version=$VERSION" -o hmac-file-server cmd/server/*.go
```

### Cross-Platform Build
```bash
# Linux AMD64
GOOS=linux GOARCH=amd64 go build -o hmac-file-server-linux-amd64 cmd/server/*.go

# Linux ARM64 (for ARM servers/Raspberry Pi)
GOOS=linux GOARCH=arm64 go build -o hmac-file-server-linux-arm64 cmd/server/*.go

# Windows
GOOS=windows GOARCH=amd64 go build -o hmac-file-server.exe cmd/server/*.go

# macOS
GOOS=darwin GOARCH=amd64 go build -o hmac-file-server-macos cmd/server/*.go
```

## Configuration for Build

### Enable Network Resilience Features
Create or update your `config.toml`:
```toml
[server]
bind_ip = "0.0.0.0"
listenport = "8080"
networkevents = true              # Enable network monitoring

[uploads]
chunkeduploadsenabled = true      # Enable chunked uploads
resumableuploadsenabled = true    # Enable resumable uploads
chunksize = "5MB"                 # Optimal for mobile
sessiontimeout = "24h"            # Session persistence
maxretries = 5                    # Retry attempts

[timeouts]
readtimeout = "300s"              # 5 minutes
writetimeout = "300s"             # 5 minutes  
idletimeout = "600s"              # 10 minutes

[security]
secret = "your-super-secret-hmac-key-minimum-32-characters"
```

## Testing the Build

### 1. Basic Functionality Test
```bash
# Test binary works
./hmac-file-server --help

# Test with config file
./hmac-file-server --config config.toml
```

### 2. Test Network Resilience Features
```bash
# Start server with chunked uploads enabled
./hmac-file-server --config config.toml

# In another terminal, test chunked upload endpoint
curl -X POST \
  -H "X-Filename: test.txt" \
  -H "X-Total-Size: 1024" \
  -H "X-Signature: $(echo -n '/upload/chunked' | openssl dgst -sha256 -hmac 'your-secret' | cut -d' ' -f2)" \
  http://localhost:8080/upload/chunked
```

### 3. Run Go Tests
```bash
# Run existing tests
go test ./test/...

# Run with verbose output
go test -v ./test/...

# Run specific tests
go test -run TestUpload ./test/
```

## Docker Build (Alternative)

### Using Existing Docker Setup
```bash
# Build Docker image
./builddocker.sh

# Or manually
docker build -t hmac-file-server .
```

### Run with Docker
```bash
# Start with docker-compose
cd dockerenv
docker-compose up -d

# Or run directly
docker run -d \
  -p 8080:8080 \
  -p 9090:9090 \
  -v $(pwd)/config:/etc/hmac-file-server \
  -v $(pwd)/data:/var/lib/hmac-file-server \
  hmac-file-server
```

## Troubleshooting

### Build Issues

#### Missing Dependencies
```bash
# Clean module cache and re-download
go clean -modcache
go mod download
```

#### Go Version Issues
```bash
# Check Go version
go version

# Update Go if needed (Ubuntu/Debian)
sudo snap install go --classic

# Or download from https://golang.org/dl/
```

#### Network Resilience Files Missing
```bash
# Check if files exist
ls -la cmd/server/upload_session.go
ls -la cmd/server/network_resilience.go
ls -la cmd/server/chunked_upload_handler.go
ls -la cmd/server/integration.go

# If missing, the build will work but without network resilience features
# Core functionality remains unchanged
```

### Runtime Issues

#### Port Already in Use
```bash
# Check what's using port 8080
sudo netstat -tlnp | grep :8080

# Kill process if needed
sudo kill $(sudo lsof -t -i:8080)
```

#### Permission Issues
```bash
# Make binary executable
chmod +x hmac-file-server

# For system service installation
sudo chown root:root hmac-file-server
sudo chmod 755 hmac-file-server
```

#### Config File Issues
```bash
# Validate config syntax
./hmac-file-server --config config.toml --validate

# Use example config as starting point
cp config-example-xmpp.toml config.toml
```

## Build Performance

### Faster Builds
```bash
# Use build cache
export GOCACHE=$(go env GOCACHE)

# Parallel builds
go build -p 4 cmd/server/*.go

# Skip tests during development
go build -a cmd/server/*.go
```

### Smaller Binaries
```bash
# Strip debug info and symbol table
go build -ldflags="-s -w" cmd/server/*.go

# Use UPX compression (if installed)
upx --best hmac-file-server
```

## Deployment

### System Service
```bash
# Copy binary to system location
sudo cp hmac-file-server /usr/local/bin/

# Create systemd service
sudo cp hmac-file-server.service /etc/systemd/system/
sudo systemctl enable hmac-file-server
sudo systemctl start hmac-file-server
```

### Reverse Proxy Setup
```bash
# Nginx configuration
sudo cp nginx-hmac-file-server.conf /etc/nginx/sites-available/
sudo ln -s /etc/nginx/sites-available/hmac-file-server.conf /etc/nginx/sites-enabled/
sudo nginx -t && sudo systemctl reload nginx
```

This build process ensures that:
- ✅ **Backward Compatibility**: Works with or without network resilience files
- ✅ **Feature Detection**: Automatically includes available network resilience features  
- ✅ **Zero Downtime**: Existing deployments continue working unchanged
- ✅ **Mobile Optimized**: New features specifically address network switching issues
