
package iso

import (
	"fmt"
	"os/exec"
	"path/filepath"

	"pkg/config"
	"pkg/logging"
)

func VerifyAndCreateISOContainer() error {
	isoPath := filepath.Join(config.Conf.ISO.MountPoint, "container.iso")

	err := verifyISOFile(isoPath)
	if err != nil {
		logging.Errorf("ISO verification failed: %v", err)
		err = handleCorruptedISOFile(isoPath, []string{""}, config.Conf.ISO.Size, config.Conf.ISO.Charset)
		if err != nil {
			return fmt.Errorf("failed to handle corrupted ISO file: %w", err)
		}
	}

	err = MountISOContainer(isoPath, config.Conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}
	logging.Infof("ISO container mounted at %s", config.Conf.ISO.MountPoint)

	return nil
}

func verifyISOFile(isoPath string) error {
	cmd := exec.Command("isoinfo", "-i", isoPath, "-d")
	output, err := cmd.CombinedOutput()
	if err != nil {
		logging.Errorf("ISO verification error: %v, output: %s", err, string(output))
		return err
	}
	return nil
}

func CreateISOContainer(files []string, isoPath string, size string, charset string) error {
	args := []string{"-o", isoPath, "-V", "ISO_CONTAINER", "-J", "-R", "-input-charset", charset}
	args = append(args, files...)
	cmd := exec.Command("genisoimage", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func MountISOContainer(isoPath string, mountPoint string) error {
	cmd := exec.Command("mount", "-o", "loop", isoPath, mountPoint)
	output, err := cmd.CombinedOutput()
	if err != nil {
		logging.Errorf("Failed to mount ISO: %v, output: %s", err, string(output))
		return err
	}
	return nil
}

func UnmountISOContainer(mountPoint string) error {
	cmd := exec.Command("umount", mountPoint)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func handleCorruptedISOFile(isoPath string, files []string, size string, charset string) error {
	logging.Warnf("ISO file %s corrupted. Recreating...", isoPath)
	err := CreateISOContainer(files, isoPath, size, charset)
	if err != nil {
		return fmt.Errorf("failed to recreate ISO container: %w", err)
	}
	return nil
}