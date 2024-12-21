package fileops

import (
	"fmt"
	"path/filepath"

	"github.com/renz/hmac-file-server/internal/config"
)

var conf *config.Config

func Init(configuration *config.Config) {
	conf = configuration
}

func SanitizeFilePath(filePath string) (string, error) {
	absBaseDir, err := filepath.Abs(conf.Server.StoragePath)
	if err != nil {
		return "", err
	}

	absFilePath, err := filepath.Abs(filepath.Join(absBaseDir, filePath))
	if err != nil {
		return "", err
	}

	if !filepath.HasPrefix(absFilePath, absBaseDir) {
		return "", fmt.Errorf("invalid file path")
	}

	return absFilePath, nil
}

// ...additional file operations...
