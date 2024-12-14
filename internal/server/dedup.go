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
func computeFileHash(filePath string) (string, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return "", fmt.Errorf("unable to open file %s: %w", filePath, err)
	}
func handleDeduplication(ctx context.Context, absFilename string) error {
	// Compute checksum of the uploaded file
	checksum, err := computeSHA256(ctx, absFilename)
	if err != nil {
		log.Errorf("Failed to compute SHA256 for %s: %v", absFilename, err)
		return fmt.Errorf("checksum computation failed: %w", err)
	}
