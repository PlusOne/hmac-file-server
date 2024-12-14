func sanitizeFilePath(baseDir, filePath string) (string, error) {
	// Resolve the absolute path
	absBaseDir, err := filepath.Abs(baseDir)
	if err != nil {
		return "", fmt.Errorf("failed to resolve base directory: %w", err)
	}
func checkStorageSpace(storagePath string, minFreeBytes int64) error {
	var stat syscall.Statfs_t
	err := syscall.Statfs(storagePath, &stat)
	if err != nil {
		return fmt.Errorf("failed to get filesystem stats: %w", err)
	}
func computeSHA256(ctx context.Context, filePath string) (string, error) {
	if filePath == "" {
		return "", fmt.Errorf("computeSHA256: filePath cannot be empty")
	}
func checkFreeSpaceWithRetry(path string, retries int, delay time.Duration) error {
	for i := 0; i < retries; i++ {
		if err := checkStorageSpace(path, MinFreeBytes); err != nil {
			log.Warnf("Free space check failed (attempt %d/%d): %v", i+1, retries, err)
			time.Sleep(delay)
			continue
		}
