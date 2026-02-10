//go:build windows

package main

import (
	"fmt"
	"unsafe"

	"golang.org/x/sys/windows"
)

func checkStorageSpacePlatform(storagePath string, minFreeBytes int64) error {
	kernel32 := windows.NewLazySystemDLL("kernel32.dll")
	getDiskFreeSpaceEx := kernel32.NewProc("GetDiskFreeSpaceExW")

	var freeBytesAvailable, totalBytes, totalFreeBytes uint64
	pathPtr, err := windows.UTF16PtrFromString(storagePath)
	if err != nil {
		return fmt.Errorf("invalid path: %w", err)
	}

	ret, _, err := getDiskFreeSpaceEx.Call(
		uintptr(unsafe.Pointer(pathPtr)),
		uintptr(unsafe.Pointer(&freeBytesAvailable)),
		uintptr(unsafe.Pointer(&totalBytes)),
		uintptr(unsafe.Pointer(&totalFreeBytes)),
	)
	if ret == 0 {
		return fmt.Errorf("GetDiskFreeSpaceExW failed: %w", err)
	}

	if int64(freeBytesAvailable) < minFreeBytes {
		return fmt.Errorf("not enough space: available %d < required %d", freeBytesAvailable, minFreeBytes)
	}
	return nil
}
