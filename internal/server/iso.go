func CreateISOContainer(files []string, isoPath string, size string, charset string) error {
	args := []string{"-o", isoPath, "-V", "ISO_CONTAINER", "-J", "-R", "-input-charset", charset}
func MountISOContainer(isoPath string, mountPoint string) error {
	// Ensure the mount point directory exists
	if err := os.MkdirAll(mountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point directory: %w", err)
	}
func UnmountISOContainer(mountPoint string) error {
	cmd := exec.Command("umount", mountPoint)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
func handleISOContainer(absFilename string) error {
	isoPath := filepath.Join(conf.ISO.MountPoint, "container.iso")

	// Create ISO container
	err := CreateISOContainer([]string{absFilename}, isoPath, conf.ISO.Size, conf.ISO.Charset)
func verifyAndCreateISOContainer() error {
	isoPath := filepath.Join(conf.ISO.MountPoint, "container.iso")

	// Check if ISO file exists
	if exists, _ := fileExists(isoPath); !exists {
		log.Infof("ISO container does not exist. Creating new ISO container at %s", isoPath)

		// Example files to include in the ISO container
		files := []string{conf.Server.StoragePath}
func verifyISOFile(isoPath string) error {
	cmd := exec.Command("isoinfo", "-i", isoPath, "-d")
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("failed to verify ISO file: %w, output: %s", err, string(output))
	}
func handleCorruptedISOFile(isoPath string, files []string, size string, charset string) error {
	log.Warnf("ISO file %s is corrupted. Recreating it.", isoPath)
	err := CreateISOContainer(files, isoPath, size, charset)
	if err != nil {
		return fmt.Errorf("failed to recreate ISO container: %w", err)
	}
