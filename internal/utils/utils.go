package utils

import (
    "bufio"
    "fmt"
    "io"
    "os"
    "path/filepath"
    "strings"
)

// SanitizeFilePath ensures that the file path is within the designated storage directory
func SanitizeFilePath(baseDir, filePath string) (string, error) {
    absBaseDir, err := filepath.Abs(baseDir)
    if err != nil {
        return "", err
    }

    absFilePath, err := filepath.Abs(filepath.Join(absBaseDir, filePath))
    if err != nil {
        return "", err
    }

    if !strings.HasPrefix(absFilePath, absBaseDir) {
        return "", fmt.Errorf("invalid file path: %s", filePath)
    }

    return absFilePath, nil
}

// IsExtensionAllowed checks if the file extension is allowed
func IsExtensionAllowed(filePath string) bool {
    allowedExtensions := []string{".txt", ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".svg", ".webp"}
    ext := strings.ToLower(filepath.Ext(filePath))
    for _, allowedExt := range allowedExtensions {
        if ext == allowedExt {
            return true
        }
    }
    return false
}

// CreateFile creates a file from the provided reader
func CreateFile(filePath string, reader io.Reader) error {
    absDirectory := filepath.Dir(filePath)
    err := os.MkdirAll(absDirectory, os.ModePerm)
    if err != nil {
        return err
    }

    file, err := os.OpenFile(filePath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
    if err != nil {
        return err
    }
    defer file.Close()

    writer := bufio.NewWriter(file)
    _, err = io.Copy(writer, reader)
    if err != nil {
        return err
    }

    return writer.Flush()
}