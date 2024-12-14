// internal/storage/storage.go

package storage

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"syscall"
	"time"

	"your-project/internal/config"
	"your-project/internal/logging"

	"github.com/patrickmn/go-cache"
)

var (
	fileInfoCache *cache.Cache
	mu            sync.RWMutex
)

const (
	MinFreeBytes = 1 << 30 // 1 GB
)

// InitCache initialisiert den Datei-Info-Cache
func InitCache() {
	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)
}

// FileExists prüft, ob eine Datei existiert
func FileExists(filePath string) (bool, os.FileInfo) {
	if cachedInfo, found := fileInfoCache.Get(filePath); found {
		if info, ok := cachedInfo.(os.FileInfo); ok {
			return !info.IsDir(), info
		}
	}

	fileInfo, err := os.Stat(filePath)
	if os.IsNotExist(err) {
		return false, nil
	} else if err != nil {
		logging.Log.Error("Error checking file existence:", err)
		return false, nil
	}

	fileInfoCache.Set(filePath, fileInfo, cache.DefaultExpiration)
	return !fileInfo.IsDir(), fileInfo
}

// SanitizeFilePath sichert, dass der Dateipfad innerhalb des Storage-Verzeichnisses liegt
func SanitizeFilePath(baseDir, filePath string) (string, error) {
	// Resolve the absolute path
	absBaseDir, err := filepath.Abs(baseDir)
	if err != nil {
		return "", fmt.Errorf("failed to resolve base directory: %w", err)
	}

	absFilePath, err := filepath.Abs(filepath.Join(absBaseDir, filePath))
	if err != nil {
		return "", fmt.Errorf("failed to resolve file path: %w", err)
	}

	// Check if the resolved file path is within the base directory
	if !strings.HasPrefix(absFilePath, absBaseDir) {
		return "", fmt.Errorf("invalid file path: %s", filePath)
	}

	return absFilePath, nil
}

// CheckFreeSpace prüft, ob genügend freier Speicherplatz vorhanden ist
func CheckFreeSpace(storagePath string, minFreeBytes int64) error {
	var stat syscall.Statfs_t
	err := syscall.Statfs(storagePath, &stat)
	if err != nil {
		return fmt.Errorf("failed to get filesystem stats: %w", err)
	}

	// Calculate available bytes
	availableBytes := stat.Bavail * uint64(stat.Bsize)
	if int64(availableBytes) < minFreeBytes {
		return fmt.Errorf("not enough free space: %d bytes available, %d bytes required", availableBytes, minFreeBytes)
	}

	return nil
}

// CheckFreeSpaceWithRetry prüft den freien Speicherplatz mit Wiederholungen
func CheckFreeSpaceWithRetry(path string, retries int, delay time.Duration) error {
	for i := 0; i < retries; i++ {
		if err := CheckFreeSpace(path, config.Conf.Server.MinFreeBytes); err != nil {
			logging.Log.Warnf("Free space check failed (attempt %d/%d): %v", i+1, retries, err)
			time.Sleep(delay)
			continue
		}
		return nil
	}
	return fmt.Errorf("checkFreeSpace: insufficient free space after %d attempts", retries)
}

// RunFileCleaner startet einen periodischen Job zur Entfernung veralteter Dateien
func RunFileCleaner(ctx context.Context, storeDir string, ttl time.Duration) {
	ticker := time.NewTicker(1 * time.Hour)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			logging.Log.Info("Stopping file cleaner.")
			return
		case <-ticker.C:
			now := time.Now()
			err := filepath.Walk(storeDir, func(path string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if info.IsDir() {
					return nil
				}
				if now.Sub(info.ModTime()) > ttl {
					err := os.Remove(path)
					if err != nil {
						logging.Log.WithError(err).Errorf("Failed to remove expired file: %s", path)
					} else {
						logging.Log.Infof("Removed expired file: %s", path)
					}
				}
				return nil
			})
			if err != nil {
				logging.Log.WithError(err).Error("Error walking store directory for file cleaning")
			}
		}
	}
}

// SetupGracefulShutdown richtet die Graceful Shutdown Logik ein
func SetupGracefulShutdown(server *http.Server, cancel context.CancelFunc) {
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-quit
		logging.Log.Infof("Received signal %s. Initiating shutdown...", sig)

		// Create a deadline to wait for.
		ctxShutdown, shutdownCancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer shutdownCancel()

		// Attempt graceful shutdown
		if err := server.Shutdown(ctxShutdown); err != nil {
			logging.Log.Errorf("Server shutdown failed: %v", err)
		} else {
			logging.Log.Info("Server shutdown gracefully.")
		}

		// Signal other goroutines to stop
		cancel()

		logging.Log.Info("Shutdown process completed. Exiting application.")
		os.Exit(0)
	}()
}
