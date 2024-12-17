package main

import (
    "bufio"
    "context"
    "crypto/sha256"
    "encoding/hex"
    "fmt"
    "io"
    "os"
    "os/exec"
    "path/filepath"
    "strconv"
    "strings"
    "syscall"
    "time"
)

func parseSize(sizeStr string) (uint64, error) {
    var size uint64
    var unit string
    _, err := fmt.Sscanf(sizeStr, "%d%s", &size, &unit)
    if err != nil {
        return 0, fmt.Errorf("invalid size format: %s", sizeStr)
    }
    switch unit {
    case "KB":
        size *= 1 << 10
    case "MB":
        size *= 1 << 20
    case "GB":
        size *= 1 << 30
    case "TB":
        size *= 1 << 40
    }
    return size, nil
}

func parseDuration(durationStr string) time.Duration {
    duration, err := time.ParseDuration(durationStr)
    if err != nil {
        log.Fatalf("Invalid duration string '%s': %v", durationStr, err)
    }
    return duration
}

func checkFreeSpace(path string) error {
    var stat syscall.Statfs_t
    err := syscall.Statfs(path, &stat)
    if err != nil {
        return fmt.Errorf("failed to get filesystem stats: %w", err)
    }
    freeBytes := stat.Bavail * uint64(stat.Bsize)
    minFreeBytes, err := parseSize(conf.Server.MinFreeBytes)
    if err != nil {
        return fmt.Errorf("invalid MinFreeBytes: %w", err)
    }
    if freeBytes < minFreeBytes {
        return fmt.Errorf("not enough free space: need %d bytes, have %d bytes", minFreeBytes, freeBytes)
    }
    return nil
}

func checkFreeSpaceWithRetry(path string, retries int, delay time.Duration) error {
    for i := 0; i < retries; i++ {
        err := checkFreeSpace(path)
        if err == nil {
            return nil
        }
        log.Warnf("Free space check failed (%d/%d): %v", i+1, retries, err)
        time.Sleep(delay)
    }
    return fmt.Errorf("insufficient free space after %d retries", retries)
}

func sanitizeFilePath(baseDir, filePath string) (string, error) {
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

func createISOContainer(isoPath, size, charset string) error {
    cmd := exec.Command("dd", "if=/dev/zero", fmt.Sprintf("of=%s", isoPath), fmt.Sprintf("bs=%s", size), "count=1")
    err := cmd.Run()
    if err != nil {
        return fmt.Errorf("failed to create ISO file: %w", err)
    }
    cmd = exec.Command("mkfs.iso9660", "-input-charset", charset, isoPath)
    err = cmd.Run()
    if err != nil {
        return fmt.Errorf("failed to format ISO file: %w", err)
    }
    return nil
}

func MountISOContainer(isoPath, mountPoint string) error {
    cmd := exec.Command("mount", "-o", "loop", isoPath, mountPoint)
    err := cmd.Run()
    if err != nil {
        return fmt.Errorf("failed to mount ISO: %w", err)
    }
    return nil
}

func UnmountISOContainer(mountPoint string) error {
    cmd := exec.Command("umount", mountPoint)
    err := cmd.Run()
    if err != nil {
        return fmt.Errorf("failed to unmount ISO: %w", err)
    }
    return nil
}
