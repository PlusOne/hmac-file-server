package fileops

import (
	"context"
	"os"
	"path/filepath"
	"time"

	"github.com/sirupsen/logrus"
)

func RunFileCleaner(ctx context.Context, storeDir string, ttl time.Duration, log *logrus.Logger) {
	ticker := time.NewTicker(1 * time.Hour)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping file cleaner.")
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
						log.WithError(err).Errorf("Failed to remove expired file: %s", path)
					} else {
						log.Infof("Removed expired file: %s", path)
					}
				}
				return nil
			})
			if err != nil {
				log.WithError(err).Error("Error cleaning files")
			}
		}
	}
}
