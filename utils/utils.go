package utils

import (
	"fmt"
	"path/filepath"
	"strings"
)

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

// Additional utility functions...
