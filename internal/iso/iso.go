// internal/iso/iso.go

package iso

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"your-project/internal/config"
	"your-project/internal/logging"
)

func CreateISOContainer(files []string, isoPath string, size string, charset string) error {
	args := []string{"-o", isoPath, "-V", "ISO_CONTAINER", "-J", "-R", "-input-charset", charset}
	args = append(args, files...)
	cmd := exec.Command("genisoimage", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err := cmd.Run()
	if err != nil {
		logging.Log.Errorf("genisoimage failed: %v", err)
		return err
	}
	return nil
}

func MountISOContainer(isoPath string, mountPoint string) error {
	// Ensure the mount point directory exists
	if err := os.MkdirAll(mountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point directory: %w", err)
	}

	var output []byte
	var err error
	for i := 0; i < 3; i++ {
		cmd := exec.Command("mount", "-o", "loop,ro", isoPath, mountPoint)
		output, err = cmd.CombinedOutput()
		if err == nil {
			logging.Log.Infof("ISO container mounted successfully at %s", mountPoint)
			return nil
		}
		logging.Log.Warnf("Failed to mount ISO container (attempt %d/3): %v, output: %s", i+1, err, string(output))
		time.Sleep(2 * time.Second)
	}
	return fmt.Errorf("failed to mount ISO container: %w, output: %s", err, string(output))
}

func UnmountISOContainer(mountPoint string) error {
	cmd := exec.Command("umount", mountPoint)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err := cmd.Run()
	if err != nil {
		logging.Log.Errorf("Failed to unmount ISO container: %v", err)
		return err
	}
	return nil
}

func HandleISOContainer(absFilename string) error {
	isoPath := filepath.Join(config.Conf.ISO.MountPoint, "container.iso")

	// Create ISO container
	err := CreateISOContainer([]string{absFilename}, isoPath, config.Conf.ISO.Size, config.Conf.ISO.Charset)
	if err != nil {
		return fmt.Errorf("failed to create ISO container: %w", err)
	}

	// Ensure the mount point directory exists
	if err := os.MkdirAll(config.Conf.ISO.MountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point directory: %w", err)
	}

	// Mount ISO container
	err = MountISOContainer(isoPath, config.Conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}

	// Unmount ISO container (example)
	err = UnmountISOContainer(config.Conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to unmount ISO container: %w", err)
	}

	return nil
}

func VerifyAndCreateISOContainer() error {
	isoPath := filepath.Join(config.Conf.ISO.MountPoint, "container.iso")

	// Check if ISO file exists
	if exists, _ := storage.FileExists(isoPath); !exists {
		logging.Log.Infof("ISO container does not exist. Creating new ISO container at %s", isoPath)

		// Example files to include in the ISO container
		files := []string{config.Conf.Server.StoragePath}

		// Create ISO container
		err := CreateISOContainer(files, isoPath, config.Conf.ISO.Size, config.Conf.ISO.Charset)
		if err != nil {
			return fmt.Errorf("failed to create ISO container: %w", err)
		}
		logging.Log.Infof("ISO container created successfully at %s", isoPath)
	}

	// Verify ISO file consistency
	err := VerifyISOFile(isoPath)
	if err != nil {
		// Handle corrupted ISO file
		files := []string{config.Conf.Server.StoragePath}
		err = HandleCorruptedISOFile(isoPath, files, config.Conf.ISO.Size, config.Conf.ISO.Charset)
		if err != nil {
			return fmt.Errorf("failed to handle corrupted ISO file: %w", err)
		}
	}

	// Ensure the mount point directory exists
	if err := os.MkdirAll(config.Conf.ISO.MountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point directory: %w", err)
	}

	// Mount ISO container
	err = MountISOContainer(isoPath, config.Conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}
	logging.Log.Infof("ISO container mounted successfully at %s", config.Conf.ISO.MountPoint)

	return nil
}

func VerifyISOFile(isoPath string) error {
	cmd := exec.Command("isoinfo", "-i", isoPath, "-d")
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("failed to verify ISO file: %w, output: %s", err, string(output))
	}
	return nil
}

func HandleCorruptedISOFile(isoPath string, files []string, size string, charset string) error {
	logging.Log.Warnf("ISO file %s is corrupted. Recreating it.", isoPath)
	err := CreateISOContainer(files, isoPath, size, charset)
	if err != nil {
		return fmt.Errorf("failed to recreate ISO container: %w", err)
	}
	return nil
}
