package main

import (
    "os"
    "path/filepath"
    "errors"
    "time"
)

func fileExists(filePath string) bool {
    _, err := os.Stat(filePath)
    return !errors.Is(err, os.ErrNotExist)
}

func createFile(path string) (*os.File, error) {
    dir := filepath.Dir(path)
    if err := os.MkdirAll(dir, 0755); err != nil {
        return nil, err
    }
    return os.Create(path)
}

func versionFile(path string) string {
    timestamp := time.Now().Format("20060102-150405")
    ext := filepath.Ext(path)
    base := path[:len(path)-len(ext)]
    return base + "-" + timestamp + ext
}
