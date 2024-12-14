#!/usr/bin/env bash

# This script helps restructure the monolithic main.go file into multiple Go files
# based on functionality. It assumes that you have the main.go file in cmd/server/main.go
# and you want to break it down into smaller, logically separated files for better maintainability.
#
# Before running this script:
# 1. Make sure you're in the root directory of the project (where go.mod resides).
# 2. Ensure that cmd/server/main.go exists.
# 3. Run: ./reorg.sh
#
# The script will:
# - Create a 'server' package directory: internal/server (or use pkg/server if you prefer)
# - Move main.go into cmd/server/main.go (if not already there)
# - Extract different related functions and types into separate files under internal/server
#
# Adjust filenames and sections as needed based on the codebase and preferences.

set -e

# Create internal directory if it doesn't exist
mkdir -p internal/server

# Backup the original main.go
cp cmd/server/main.go cmd/server/main.go.bak

# Extract configuration-related code to config.go
awk '
/^type ServerConfig struct/,/^}/ {print > "internal/server/config.go"; next}
/^type TimeoutConfig struct/,/^}/ {print >> "internal/server/config.go"; next}
/^type SecurityConfig struct/,/^}/ {print >> "internal/server/config.go"; next}
/^type VersioningConfig struct/,/^}/ {print >> "internal/server/config.go"; next}
/^type UploadsConfig struct/,/^}/ {print >> "internal/server/config.go"; next}
/^type ClamAVConfig struct/,/^}/ {print >> "internal/server/config.go"; next}
/^type RedisConfig struct/,/^}/ {print >> "internal/server/config.go"; next}
/^type WorkersConfig struct/,/^}/ {print >> "internal/server/config.go"; next}
/^type FileConfig struct/,/^}/ {print >> "internal/server/config.go"; next}
/^type ISOConfig struct/,/^}/ {print >> "internal/server/config.go"; next}
/^type Config struct/,/^}/ {print >> "internal/server/config.go"; next}
/^func readConfig/,/}/ {print >> "internal/server/config.go"; next}
/^func setDefaults/,/}/ {print >> "internal/server/config.go"; next}
/^func validateConfig/,/}/ {print >> "internal/server/config.go"; next}
{ next }' cmd/server/main.go

# Extract logging & system info to logging.go
awk '
/^func setupLogging/,/}/ {print > "internal/server/logging.go"; next}
/^func logSystemInfo/,/}/ {print >> "internal/server/logging.go"; next}
{ next }' cmd/server/main.go.bak

# Extract metrics initialization to metrics.go
awk '
/^var.*prometheus/,/^\)/ {print > "internal/server/metrics.go"}
/^func initMetrics/,/}/ {print >> "internal/server/metrics.go"; next}
/^func updateSystemMetrics/,/}/ {print >> "internal/server/metrics.go"; next}
{ next }' cmd/server/main.go.bak

# Extract file handling & versioning to files.go
awk '
/^type UploadTask struct/,/^}/ {print > "internal/server/files.go"; next}
/^type ScanTask struct/,/^}/ {print >> "internal/server/files.go"; next}
/^func fileExists/,/}/ {print >> "internal/server/files.go"; next}
/^func isExtensionAllowed/,/}/ {print >> "internal/server/files.go"; next}
/^func versionFile/,/}/ {print >> "internal/server/files.go"; next}
/^func cleanupOldVersions/,/}/ {print >> "internal/server/files.go"; next}
/^func processUpload/,/}/ {print >> "internal/server/files.go"; next}
/^func uploadWorker/,/}/ {print >> "internal/server/files.go"; next}
/^func initializeUploadWorkerPool/,/}/ {print >> "internal/server/files.go"; next}
/^func createFile/,/}/ {print >> "internal/server/files.go"; next}
/^func handleMultipartUpload/,/}/ {print >> "internal/server/files.go"; next}
/^func handleChunkedUpload/,/}/ {print >> "internal/server/files.go"; next}
/^func getFileInfo/,/}/ {print >> "internal/server/files.go"; next}
/^func handleRequest/,/}/ {print >> "internal/server/files.go"; next}
/^func handleUpload/,/}/ {print >> "internal/server/files.go"; next}
/^func handleDownload/,/}/ {print >> "internal/server/files.go"; next}
/^func handleResumableDownload/,/}/ {print >> "internal/server/files.go"; next}
/^func runFileCleaner/,/}/ {print >> "internal/server/files.go"; next}
{ next }' cmd/server/main.go.bak

# Extract scanning (ClamAV) to scanning.go
awk '
/^func scanFileWithClamAV/,/}/ {print > "internal/server/scanning.go"; next}
/^func initClamAV/,/}/ {print >> "internal/server/scanning.go"; next}
/^func scanWorker/,/}/ {print >> "internal/server/scanning.go"; next}
/^func initializeScanWorkerPool/,/}/ {print >> "internal/server/scanning.go"; next}
{ next }' cmd/server/main.go.bak

# Extract network & events to network.go
awk '
/^type NetworkEvent struct/,/^}/ {print > "internal/server/network.go"; next}
/^func monitorNetwork/,/}/ {print >> "internal/server/network.go"; next}
/^func handleNetworkEvents/,/}/ {print >> "internal/server/network.go"; next}
/^func getCurrentIPAddress/,/}/ {print >> "internal/server/network.go"; next}
{ next }' cmd/server/main.go.bak

# Extract redis related code to redis.go
awk '
/^var redisClient/,/^var redisConnected/ {print > "internal/server/redis.go"; next}
/^func initRedis/,/}/ {print >> "internal/server/redis.go"; next}
/^func MonitorRedisHealth/,/}/ {print >> "internal/server/redis.go"; next}
{ next }' cmd/server/main.go.bak

# Extract deduplication to dedup.go
awk '
/^func DeduplicateFiles/,/}/ {print > "internal/server/dedup.go"; next}
/^func computeFileHash/,/}/ {print >> "internal/server/dedup.go"; next}
/^func handleDeduplication/,/}/ {print >> "internal/server/dedup.go"; next}
{ next }' cmd/server/main.go.bak

# Extract ISO container handling to iso.go
awk '
/^func CreateISOContainer/,/}/ {print > "internal/server/iso.go"; next}
/^func MountISOContainer/,/}/ {print >> "internal/server/iso.go"; next}
/^func UnmountISOContainer/,/}/ {print >> "internal/server/iso.go"; next}
/^func handleISOContainer/,/}/ {print >> "internal/server/iso.go"; next}
/^func verifyAndCreateISOContainer/,/}/ {print >> "internal/server/iso.go"; next}
/^func verifyISOFile/,/}/ {print >> "internal/server/iso.go"; next}
/^func handleCorruptedISOFile/,/}/ {print >> "internal/server/iso.go"; next}
{ next }' cmd/server/main.go.bak

# Extract utility functions to utils.go
awk '
/^func sanitizeFilePath/,/}/ {print > "internal/server/utils.go"; next}
/^func checkStorageSpace/,/}/ {print >> "internal/server/utils.go"; next}
/^func computeSHA256/,/}/ {print >> "internal/server/utils.go"; next}
/^func checkFreeSpaceWithRetry/,/}/ {print >> "internal/server/utils.go"; next}
{ next }' cmd/server/main.go.bak

# Extract HTTP server setup & middleware to server.go
awk '
/^func setupRouter/,/}/ {print > "internal/server/server.go"; next}
/^func loggingMiddleware/,/}/ {print >> "internal/server/server.go"; next}
/^func recoveryMiddleware/,/}/ {print >> "internal/server/server.go"; next}
/^func corsMiddleware/,/}/ {print >> "internal/server/server.go"; next}
{ next }' cmd/server/main.go.bak

# Extract graceful shutdown & main function to main.go
awk '
/^func setupGracefulShutdown/,/}/ {print > "cmd/server/main.go.new"; next}
/^func main/,/}/ {print >> "cmd/server/main.go.new"; next}
{ next }' cmd/server/main.go.bak

# Replace the old main.go with the newly created main.go.new
mv cmd/server/main.go.new cmd/server/main.go

echo "Restructuring complete!"
echo "Check the internal/server directory for your separated files."
echo "Please review each file and adjust package names/imports as necessary."
