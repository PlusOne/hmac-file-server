#!/usr/bin/env bash

ORIG_FILE="main.go"
BACKUP_FILE="main.go.backup"

if [ ! -f "$ORIG_FILE" ]; then
    echo "$ORIG_FILE not found! Please run this script from your project directory."
    exit 1
fi

# Create a backup of the original main.go
cp "$ORIG_FILE" "$BACKUP_FILE"

# Create directory structure
mkdir -p cmd/hmac-server
mkdir -p internal/config
mkdir -p internal/logging
mkdir -p internal/metrics
mkdir -p internal/storage
mkdir -p internal/uploads
mkdir -p internal/downloads
mkdir -p internal/security
mkdir -p internal/scanning
mkdir -p internal/network
mkdir -p internal/redis
mkdir -p internal/iso
mkdir -p internal/router

# Extract main() to cmd/hmac-server/main.go
awk '/func main\(\)/,/^}/' main.go > cmd/hmac-server/main.go

# Configuration related
awk '/func readConfig\(/,/^}/' main.go > internal/config/config.go
awk '/func setDefaults\(/,/^}/' main.go >> internal/config/config.go
awk '/func validateConfig\(/,/^}/' main.go >> internal/config/config.go

# Logging related
awk '/func setupLogging\(/,/^}/' main.go > internal/logging/logging.go
awk '/func logSystemInfo\(/,/^}/' main.go >> internal/logging/logging.go

# Metrics related
awk '/func initMetrics\(/,/^}/' main.go > internal/metrics/metrics.go
awk '/func updateSystemMetrics\(/,/^}/' main.go >> internal/metrics/metrics.go

# Storage related
awk '/func checkStorageSpace\(/,/^}/' main.go > internal/storage/storage.go
awk '/func runFileCleaner\(/,/^}/' main.go >> internal/storage/storage.go
awk '/func DeduplicateFiles\(/,/^}/' main.go >> internal/storage/storage.go
awk '/func computeFileHash\(/,/^}/' main.go >> internal/storage/storage.go
awk '/func checkFreeSpaceWithRetry\(/,/^}/' main.go >> internal/storage/storage.go

# Uploads related
awk '/func handleUpload\(/,/^}/' main.go > internal/uploads/upload.go
awk '/func handleMultipartUpload\(/,/^}/' main.go >> internal/uploads/upload.go
awk '/func isExtensionAllowed\(/,/^}/' main.go >> internal/uploads/upload.go
awk '/func processUpload\(/,/^}/' main.go >> internal/uploads/upload.go
awk '/func uploadWorker\(/,/^}/' main.go >> internal/uploads/upload.go
awk '/func initializeUploadWorkerPool\(/,/^}/' main.go >> internal/uploads/upload.go
awk '/func createFile\(/,/^}/' main.go >> internal/uploads/upload.go
awk '/func handleChunkedUpload\(/,/^}/' main.go >> internal/uploads/upload.go

# Downloads related
awk '/func handleDownload\(/,/^}/' main.go > internal/downloads/download.go
awk '/func handleResumableDownload\(/,/^}/' main.go >> internal/downloads/download.go

# Security related
# If you later add distinct security functions (like verifyHmac), you can extract them here.
# Currently, security checks (HMAC) seem integrated within handleUpload.

# Scanning/ClamAV related
awk '/func initClamAV\(/,/^}/' main.go > internal/scanning/clamav.go
awk '/func scanFileWithClamAV\(/,/^}/' main.go >> internal/scanning/clamav.go
awk '/func scanWorker\(/,/^}/' main.go >> internal/scanning/clamav.go
awk '/func initializeScanWorkerPool\(/,/^}/' main.go >> internal/scanning/clamav.go

# Network related
awk '/func monitorNetwork\(/,/^}/' main.go > internal/network/network.go
awk '/func handleNetworkEvents\(/,/^}/' main.go >> internal/network/network.go
awk '/func getCurrentIPAddress\(/,/^}/' main.go >> internal/network/network.go

# Redis related
awk '/func initRedis\(/,/^}/' main.go > internal/redis/redis.go
awk '/func MonitorRedisHealth\(/,/^}/' main.go >> internal/redis/redis.go

# ISO related
awk '/func verifyAndCreateISOContainer\(/,/^}/' main.go > internal/iso/iso.go
awk '/func CreateISOContainer\(/,/^}/' main.go >> internal/iso/iso.go
awk '/func MountISOContainer\(/,/^}/' main.go >> internal/iso/iso.go
awk '/func UnmountISOContainer\(/,/^}/' main.go >> internal/iso/iso.go
awk '/func handleISOContainer\(/,/^}/' main.go >> internal/iso/iso.go
awk '/func verifyISOFile\(/,/^}/' main.go >> internal/iso/iso.go
awk '/func handleCorruptedISOFile\(/,/^}/' main.go >> internal/iso/iso.go

# Router related
awk '/func setupRouter\(/,/^}/' main.go > internal/router/router.go
awk '/func loggingMiddleware\(/,/^}/' main.go >> internal/router/router.go
awk '/func recoveryMiddleware\(/,/^}/' main.go >> internal/router/router.go
awk '/func corsMiddleware\(/,/^}/' main.go >> internal/router/router.go
awk '/func handleRequest\(/,/^}/' main.go >> internal/router/router.go

# Insert package declarations
echo "package main" | cat - cmd/hmac-server/main.go > cmd/hmac-server/tmp.go && mv cmd/hmac-server/tmp.go cmd/hmac-server/main.go

for dir in config logging metrics storage uploads downloads security scanning network redis iso router; do
    f="internal/$dir/${dir}.go"
    if [ -f "$f" ]; then
        echo "package $dir" | cat - "$f" > "$f.tmp" && mv "$f.tmp" "$f"
    fi
done

# Function to clear imports and add a placeholder import block
clear_imports_and_add_placeholder() {
    local f="$1"
    # Remove existing import blocks
    sed -i '/^import (\|^import(/,/^)/d' "$f"
    # Add a placeholder import block after the package line
    sed -i '1!b; /package .*/a \
import ( \
        // TODO: Add required imports here \
)' "$f"
}

# Clear imports from all internal files and add placeholder
for dir in config logging metrics storage uploads downloads security scanning network redis iso router; do
    f="internal/$dir/${dir}.go"
    if [ -f "$f" ]; then
        clear_imports_and_add_placeholder "$f"
    fi
done

# Clear imports from main.go and add placeholder
clear_imports_and_add_placeholder "cmd/hmac-server/main.go"

echo "Splitting completed. A backup of main.go is stored in $BACKUP_FILE."
echo ""
echo "NEXT STEPS:"
echo "1. Check each new file and verify the package name is correct."
echo "2. Add or remove imports as needed. The script inserted a placeholder import block in each file."
echo "3. Export functions if needed (e.g., if main.go calls a function in internal/config/config.go,"
echo "   ensure that the function is capitalized there and called as config.FunctionName() in main.go)."
echo "4. Run 'go build ./cmd/hmac-server' to test compilation and fix any errors."
echo "5. Remove placeholder comments and unused code after verifying everything works."
