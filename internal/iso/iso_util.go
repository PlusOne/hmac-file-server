package iso

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/sirupsen/logrus"
)

// VerifyAndCreateISOContainer checks if the ISO container exists and recreates it if corrupted.
func VerifyAndCreateISOContainer(storagePath, isoMountPoint, isoSize, charset string, log *logrus.Logger) error {
	isoPath := filepath.Join(isoMountPoint, "container.iso")

	// Check if ISO file exists
	if _, err := os.Stat(isoPath); os.IsNotExist(err) {
		log.Infof("ISO container does not exist, creating at %s", isoPath)
		files := []string{storagePath}

		err := CreateISOContainer(files, isoPath, isoSize, charset, log)
		if err != nil {
			return fmt.Errorf("failed to create ISO: %w", err)
		}
		log.Infof("ISO container created at %s", isoPath)
	}

	err := verifyISOFile(isoPath)
	if err != nil {
		files := []string{storagePath}
		err = handleCorruptedISOFile(isoPath, files, isoSize, charset, log)
		if err != nil {
			return fmt.Errorf("failed to handle corrupted ISO: %w", err)
		}
	}

	if err := os.MkdirAll(isoMountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	err = MountISOContainer(isoPath, isoMountPoint, log)
	if err != nil {
		return fmt.Errorf("failed to mount ISO: %w", err)
	}
	log.Infof("ISO container mounted at %s", isoMountPoint)

	return nil
}

// verifyISOFile checks the integrity of the ISO file.
func verifyISOFile(isoPath string) error {
	cmd := exec.Command("isoinfo", "-i", isoPath, "-d")
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("failed to verify ISO: %w, output: %s", err, string(output))
	}
	return nil
}

// handleCorruptedISOFile attempts to recreate the ISO file if it's found to be corrupted.
func handleCorruptedISOFile(isoPath string, files []string, size string, charset string, log *logrus.Logger) error {
	log.Warnf("ISO file %s corrupted. Recreating...", isoPath)
	err := CreateISOContainer(files, isoPath, size, charset, log)
	if err != nil {
		return fmt.Errorf("failed to recreate ISO: %w", err)
	}
	return nil
}
