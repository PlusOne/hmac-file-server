//go:build !windows

package main

import (
	"fmt"
	"syscall"
)

func checkStorageSpacePlatform(storagePath string, minFreeBytes int64) error {
	var stat syscall.Statfs_t
	if err := syscall.Statfs(storagePath, &stat); err != nil {
		return err
	}
	availableBytes := stat.Bavail * uint64(stat.Bsize)
	if int64(availableBytes) < minFreeBytes {
		return fmt.Errorf("not enough space: available %d < required %d", availableBytes, minFreeBytes)
	}
	return nil
}
