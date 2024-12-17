package iso

import (
	"os/exec"
	"path/filepath"

	"github.com/sirupsen/logrus"
	"github.com/renz/hmac-file-server/pkg/config"
)

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
	if err := cmd.Run(); err != nil {
		logrus.Errorf("Failed to mount ISO: %v", err)
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

func HandleISOContainer(absFilename string, conf config.ISOConfig) error {
	isoPath := filepath.Join(conf.MountPoint, "container.iso")

	err := CreateISOContainer([]string{absFilename}, isoPath, conf.Size, conf.Charset)
	if err != nil {
		return err
	}

	err = MountISOContainer(isoPath, conf.MountPoint)
	if err != nil {
		return err
	}

	err = UnmountISOContainer(conf.MountPoint)
	if err != nil {
		return err
	}

	return nil
}

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

func handleCorruptedISOFile(isoPath string, files []string, size string, charset string) error {
	logging.Warnf("ISO file %s corrupted. Recreating...", isoPath)
	err := CreateISOContainer(files, isoPath, size, charset)
	if err != nil {
		return fmt.Errorf("failed to recreate ISO container: %w", err)
	}
	return nil
}
