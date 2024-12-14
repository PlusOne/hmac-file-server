func checkStorageSpace(storagePath string, minFreeBytes int64) error {
	var stat syscall.Statfs_t
	err := syscall.Statfs(storagePath, &stat)
	if err != nil {
		return fmt.Errorf("failed to get filesystem stats: %w", err)
	}

	// Calculate available bytes
	availableBytes := stat.Bavail * uint64(stat.Bsize)
	if int64(availableBytes) < minFreeBytes {
		return fmt.Errorf("not enough free space: %d bytes available, %d bytes required", availableBytes, minFreeBytes)
	}

	return nil
}
func runFileCleaner(ctx context.Context, storeDir string, ttl time.Duration) {
	ticker := time.NewTicker(1 * time.Hour)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Info("Stopping file cleaner.")
			return
		case <-ticker.C:
			now := time.Now()
			err := filepath.Walk(storeDir, func(path string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if info.IsDir() {
					return nil
				}
				if now.Sub(info.ModTime()) > ttl {
					err := os.Remove(path)
					if err != nil {
						log.WithError(err).Errorf("Failed to remove expired file: %s", path)
					} else {
						log.Infof("Removed expired file: %s", path)
					}
				}
				return nil
			})
			if err != nil {
				log.WithError(err).Error("Error walking store directory for file cleaning")
			}
		}
	}
}
func DeduplicateFiles(storeDir string) error {
	hashMap := make(map[string]string) // map[hash]filepath
	var mu sync.Mutex
	var wg sync.WaitGroup
	fileChan := make(chan string, 100)

	// Worker to process files
	numWorkers := 10
	for i := 0; i < numWorkers; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for filePath := range fileChan {
				hash, err := computeFileHash(filePath)
				if err != nil {
					logrus.WithError(err).Errorf("Failed to compute hash for %s", filePath)
					continue
				}

				mu.Lock()
				original, exists := hashMap[hash]
				if !exists {
					hashMap[hash] = filePath
					mu.Unlock()
					continue
				}
				mu.Unlock()

				// Duplicate found
				err = os.Remove(filePath)
				if err != nil {
					logrus.WithError(err).Errorf("Failed to remove duplicate file %s", filePath)
					continue
				}

				// Create hard link to the original file
				err = os.Link(original, filePath)
				if err != nil {
					logrus.WithError(err).Errorf("Failed to create hard link from %s to %s", original, filePath)
					continue
				}

				logrus.Infof("Removed duplicate %s and linked to %s", filePath, original)
			}
		}()
	}

	// Walk through the store directory
	err := filepath.Walk(storeDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			logrus.WithError(err).Errorf("Error accessing path %s", path)
			return nil
		}
		if !info.Mode().IsRegular() {
			return nil
		}
		fileChan <- path
		return nil
	})
	if err != nil {
		return fmt.Errorf("error walking the path %s: %w", storeDir, err)
	}

	close(fileChan)
	wg.Wait()
	return nil
}
func computeFileHash(filePath string) (string, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return "", fmt.Errorf("unable to open file %s: %w", filePath, err)
	}
	defer file.Close()

	hasher := sha256.New()
	if _, err := io.Copy(hasher, file); err != nil {
		return "", fmt.Errorf("error hashing file %s: %w", filePath, err)
	}

	return hex.EncodeToString(hasher.Sum(nil)), nil
}
func checkFreeSpaceWithRetry(path string, retries int, delay time.Duration) error {
	for i := 0; i < retries; i++ {
		if err := checkStorageSpace(path, MinFreeBytes); err != nil {
			log.Warnf("Free space check failed (attempt %d/%d): %v", i+1, retries, err)
			time.Sleep(delay)
			continue
		}
		return nil
	}
	return fmt.Errorf("checkFreeSpace: insufficient free space after %d attempts", retries)
}
