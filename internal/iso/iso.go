package iso

import (
	"fmt"
	"os/exec"

	"github.com/renz/hmac-file-server/internal/config"
	"github.com/sirupsen/logrus"
)

func Setup(conf *config.Config) error {
	err := verifyISOFile(conf.ISO.MountPoint + "/container.iso")
	if err != nil {
		err = CreateISOContainer([]string{}, conf.ISO.MountPoint+"/container.iso", conf.ISO.Size, conf.ISO.Charset)
		if err != nil {
			return fmt.Errorf("failed to create ISO container: %w", err)
		}
	}

	err = MountISOContainer(conf.ISO.MountPoint+"/container.iso", conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}

	logrus.Infof("ISO container mounted at %s", conf.ISO.MountPoint)
	return nil
}

func CreateISOContainer(files []string, isoPath string, size string, charset string) error {
	args := []string{"-o", isoPath, "-V", "ISO_CONTAINER", "-J", "-R", "-input-charset", charset}
	args = append(args, files...)
	cmd := exec.Command("genisoimage", args...)
	cmd.Stdout = nil
	cmd.Stderr = nil
	return cmd.Run()
}

func MountISOContainer(isoPath string, mountPoint string) error {
	cmd := exec.Command("mount", "-o", "loop", isoPath, mountPoint)
	return cmd.Run()
}

func verifyISOFile(isoPath string) error {
	cmd := exec.Command("isoinfo", "-i", isoPath, "-d")
	return cmd.Run()
}
