package fileops

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"time"
)

// parseSize converts a human-readable size string to bytes
func ParseSize(sizeStr string) (int64, error) {
	sizeStr = strings.TrimSpace(sizeStr)
	if len(sizeStr) < 2 {
		return 0, fmt.Errorf("invalid size: %s", sizeStr)
	}

	unit := strings.ToUpper(sizeStr[len(sizeStr)-2:])
	valueStr := sizeStr[:len(sizeStr)-2]
	value, err := strconv.Atoi(valueStr)
	if err != nil {
		return 0, fmt.Errorf("invalid size value: %s", valueStr)
	}

	switch unit {
	case "KB":
		return int64(value) * 1024, nil
	case "MB":
		return int64(value) * 1024 * 1024, nil
	case "GB":
		return int64(value) * 1024 * 1024 * 1024, nil
	case "TB":
		return int64(value) * 1024 * 1024 * 1024 * 1024, nil
	default:
		return 0, fmt.Errorf("unknown size unit: %s", unit)
	}
}

// parseTTL converts a human-readable TTL string to a time.Duration
func ParseTTL(ttlStr string) (time.Duration, error) {
	ttlStr = strings.TrimSpace(ttlStr)
	if len(ttlStr) < 2 {
		return 0, fmt.Errorf("invalid TTL: %s", ttlStr)
	}

	unit := ttlStr[len(ttlStr)-1:]
	valueStr := ttlStr[:len(ttlStr)-1]
	value, err := strconv.Atoi(valueStr)
	if err != nil {
		return 0, fmt.Errorf("invalid TTL value: %s", valueStr)
	}

	switch strings.ToUpper(unit) {
	case "D":
		return time.Duration(value) * 24 * time.Hour, nil
	case "M":
		return time.Duration(value) * 30 * 24 * time.Hour, nil
	case "Y":
		return time.Duration(value) * 365 * 24 * time.Hour, nil
	default:
		return 0, fmt.Errorf("unknown TTL unit: %s", unit)
	}
}

// sanitizeFilePath ensures that the file path is within the designated storage directory
func SanitizeFilePath(baseDir, filePath string) (string, error) {
	absBaseDir, err := filepath.Abs(baseDir)
	if err != nil {
		return "", fmt.Errorf("failed to resolve base directory: %w", err)
	}

	absFilePath, err := filepath.Abs(filepath.Join(absBaseDir, filePath))
	if err != nil {
		return "", fmt.Errorf("failed to resolve file path: %w", err)
	}

	if !strings.HasPrefix(absFilePath, absBaseDir) {
		return "", fmt.Errorf("invalid file path: %s", filePath)
	}

	return absFilePath, nil
}

// checkStorageSpace ensures that there is enough free space in the storage path
func CheckStorageSpace(storagePath string, minFreeBytes int64) error {
	var stat syscall.Statfs_t
	err := syscall.Statfs(storagePath, &stat)
	if err != nil {
		return fmt.Errorf("failed to get filesystem stats: %w", err)
	}

	availableBytes := stat.Bavail * uint64(stat.Bsize)
	if int64(availableBytes) < minFreeBytes {
		return fmt.Errorf("not enough free space: %d available, %d required", availableBytes, minFreeBytes)
	}

	return nil
}
