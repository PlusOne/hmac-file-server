// Package storage handles file system operations, deduplication, and space checking.
package storage

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"time"

	"github.com/sirupsen/logrus"
	"golang.org/x/sys/unix"

	"git.uuxo.net/uuxo/hmac-file-server/internal/config"
	"git.uuxo.net/uuxo/hmac-file-server/internal/metrics"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// CheckFreeSpaceWithRetry checks available disk space with retries.
func CheckFreeSpaceWithRetry(path string, minFreeBytes uint64, retries int, retryDelay time.Duration) error {
	var lastErr error
	for i := 0; i < retries; i++ {
		if i > 0 {
			time.Sleep(retryDelay)
		}
		err := CheckStorageSpace(path, minFreeBytes)
		if err == nil {
			return nil
		}
		lastErr = err
		log.Warnf("Storage space check attempt %d/%d failed: %v", i+1, retries, err)
	}
	return fmt.Errorf("storage space check failed after %d attempts: %v", retries, lastErr)
}

// CheckStorageSpace checks if there is enough free disk space.
func CheckStorageSpace(path string, minFreeBytes uint64) error {
	var stat unix.Statfs_t
	if err := unix.Statfs(path, &stat); err != nil {
		return fmt.Errorf("failed to check storage space: %w", err)
	}
	availableBytes := stat.Bavail * uint64(stat.Bsize)
	if availableBytes < minFreeBytes {
		return fmt.Errorf("insufficient storage space: %d bytes available, %d bytes required",
			availableBytes, minFreeBytes)
	}
	return nil
}

// ComputeSHA256 calculates the SHA-256 hash of a file.
func ComputeSHA256(filePath string) (string, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return "", fmt.Errorf("failed to open file for hashing: %w", err)
	}
	defer file.Close()

	hasher := sha256.New()
	if _, err := io.Copy(hasher, file); err != nil {
		return "", fmt.Errorf("failed to hash file: %w", err)
	}

	return hex.EncodeToString(hasher.Sum(nil)), nil
}

// HandleDeduplication checks for duplicate files and creates hard links.
func HandleDeduplication(filePath string, cfg *config.DeduplicationConfig) (bool, error) {
	if !cfg.Enabled {
		return false, nil
	}

	hash, err := ComputeSHA256(filePath)
	if err != nil {
		metrics.DeduplicationErrorsTotal.Inc()
		return false, err
	}

	dedupDir := cfg.Directory
	if dedupDir == "" {
		dedupDir = filepath.Dir(filePath)
	}

	dedupPath := filepath.Join(dedupDir, hash)

	if _, err := os.Stat(dedupPath); err == nil {
		// File with same hash exists - create hard link
		if err := os.Remove(filePath); err != nil {
			metrics.DeduplicationErrorsTotal.Inc()
			return false, fmt.Errorf("failed to remove duplicate file: %w", err)
		}
		if err := os.Link(dedupPath, filePath); err != nil {
			metrics.DeduplicationErrorsTotal.Inc()
			return false, fmt.Errorf("failed to create hard link: %w", err)
		}
		metrics.FilesDeduplicatedTotal.Inc()
		log.Infof("File deduplicated: %s (hash: %s)", filePath, hash)
		return true, nil
	}

	// Store the hash reference
	if err := os.MkdirAll(filepath.Dir(dedupPath), os.ModePerm); err != nil {
		return false, fmt.Errorf("failed to create dedup directory: %w", err)
	}
	if err := os.Link(filePath, dedupPath); err != nil {
		log.Warnf("Failed to create dedup reference link: %v", err)
	}

	return false, nil
}

// HandleFileCleanup removes files older than the specified TTL.
func HandleFileCleanup(directory string, ttl time.Duration) {
	err := filepath.Walk(directory, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && time.Since(info.ModTime()) > ttl {
			if removeErr := os.Remove(path); removeErr != nil {
				log.Warnf("Failed to remove expired file %s: %v", path, removeErr)
			} else {
				log.Infof("Removed expired file: %s", path)
			}
		}
		return nil
	})
	if err != nil {
		log.Errorf("Error during file cleanup: %v", err)
	}
}

// CreateAndMountISO creates an ISO container and mounts it.
func CreateAndMountISO(isoPath, size, mountpoint, charset string) error {
	cmd := exec.Command("dd", "if=/dev/zero", fmt.Sprintf("of=%s", isoPath), fmt.Sprintf("bs=%s", size), "count=1")
	if err := cmd.Run(); err != nil {
		metrics.IsoCreationErrorsTotal.Inc()
		return fmt.Errorf("failed to create ISO file: %w", err)
	}

	cmd = exec.Command("mkfs", "-t", "iso9660", "-input-charset", charset, isoPath)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to format ISO file: %w", err)
	}

	if err := os.MkdirAll(mountpoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	cmd = exec.Command("mount", "-o", "loop", isoPath, mountpoint)
	if err := cmd.Run(); err != nil {
		metrics.IsoMountErrorsTotal.Inc()
		return fmt.Errorf("failed to mount ISO file: %w", err)
	}

	metrics.IsoContainersCreatedTotal.Inc()
	metrics.IsoContainersMountedTotal.Inc()
	return nil
}

// PrecacheStoragePath is a stub for future pre-caching functionality.
func PrecacheStoragePath(_ string) {
	// Stub implementation
}
