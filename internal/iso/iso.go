package iso

import (
	"fmt"
	"os"
	"os/exec"
	"time"

	"github.com/sirupsen/logrus"
)

// CreateISOContainer creates an ISO container with the specified size and charset.
func CreateISOContainer(files []string, isoPath string, size string, charset string, log *logrus.Logger) error {
	args := []string{"-o", isoPath, "-V", "ISO_CONTAINER", "-J", "-R", "-input-charset", charset}
	args = append(args, files...)
	cmd := exec.Command("genisoimage", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to run genisoimage: %w", err)
	}
	log.Infof("ISO container created at %s", isoPath)
	return nil
}

// MountISOContainer mounts the ISO container at the given mount point.
func MountISOContainer(isoPath string, mountPoint string, log *logrus.Logger) error {
	if err := os.MkdirAll(mountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	var output []byte
	var err error
	for i := 0; i < 3; i++ {
		cmd := exec.Command("mount", "-o", "loop,ro", isoPath, mountPoint)
		output, err = cmd.CombinedOutput()
		if err == nil {
			log.Infof("ISO container mounted at %s", mountPoint)
			return nil
		}
		log.Warnf("Mount attempt %d failed: %v, output: %s", i+1, err, string(output))
		time.Sleep(2 * time.Second)
	}
	return fmt.Errorf("failed to mount ISO: %w, output: %s", err, string(output))
}

// UnmountISOContainer unmounts the ISO container from the specified mount point.
func UnmountISOContainer(mountPoint string, log *logrus.Logger) error {
	cmd := exec.Command("umount", mountPoint)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to unmount ISO: %w", err)
	}
	log.Infof("ISO container unmounted from %s", mountPoint)
	return nil
}

// HandleISOContainer creates, mounts, and unmounts an ISO container for the given file.
func HandleISOContainer(absFilename string, isoPath, mountPoint, size, charset string, log *logrus.Logger) error {
	if err := CreateISOContainer([]string{absFilename}, isoPath, size, charset, log); err != nil {
		return fmt.Errorf("failed to create ISO container: %w", err)
	}

	if err := os.MkdirAll(mountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point directory: %w", err)
	}

	if err := MountISOContainer(isoPath, mountPoint, log); err != nil {
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}

	if err := UnmountISOContainer(mountPoint, log); err != nil {
		return fmt.Errorf("failed to unmount ISO container: %w", err)
	}

	return nil
}
