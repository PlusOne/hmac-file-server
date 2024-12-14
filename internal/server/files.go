type UploadTask struct {
	AbsFilename string
	Request     *http.Request
	Result      chan error
}
type ScanTask struct {
	AbsFilename string
	Result      chan error
}
func fileExists(filePath string) (bool, int64) {
	if cachedInfo, found := fileInfoCache.Get(filePath); found {
		if info, ok := cachedInfo.(os.FileInfo); ok {
			return !info.IsDir(), info.Size()
		}
func isExtensionAllowed(filename string) bool {
	if len(conf.Uploads.AllowedExtensions) == 0 {
		return true // No restrictions if the list is empty
	}
func versionFile(absFilename string) error {
	versionDir := absFilename + "_versions"

	err := os.MkdirAll(versionDir, os.ModePerm)
	if err != nil {
		return fmt.Errorf("failed to create version directory: %v", err)
	}
func cleanupOldVersions(versionDir string) error {
	files, err := os.ReadDir(versionDir)
	if err != nil {
		return fmt.Errorf("failed to list version files: %v", err)
	}
func processUpload(task UploadTask) error {
	absFilename := task.AbsFilename
	tempFilename := absFilename + ".tmp"
	r := task.Request

	log.Infof("Processing upload for file: %s", absFilename)
	startTime := time.Now()

	// Handle uploads and write to a temporary file
	if conf.Uploads.ChunkedUploadsEnabled {
		log.Debugf("Chunked uploads enabled. Handling chunked upload for %s", tempFilename)
		err := handleChunkedUpload(tempFilename, r)
		if err != nil {
			uploadDuration.Observe(time.Since(startTime).Seconds())
			log.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Failed to handle chunked upload")
func uploadWorker(ctx context.Context, workerID int) {
    log.Infof("Upload worker %d started.", workerID)
    defer log.Infof("Upload worker %d stopped.", workerID)
    for {
        select {
        case <-ctx.Done():
            return
        case task, ok := <-uploadQueue:
            if !ok {
                return
            }
func initializeUploadWorkerPool(ctx context.Context) {
    for i := 0; i < conf.Workers.NumWorkers; i++ {
        go uploadWorker(ctx, i)
    }
func handleRequest(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost && strings.Contains(r.Header.Get("Content-Type"), "multipart/form-data") {
		absFilename, err := sanitizeFilePath(conf.Server.StoragePath, strings.TrimPrefix(r.URL.Path, "/"))
		if err != nil {
			log.WithError(err).Error("Invalid file path")
			http.Error(w, "Invalid file path", http.StatusBadRequest)
			return
		}
func handleUpload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string, a url.Values) {
	// Log the storage path being used
	log.Infof("Using storage path: %s", conf.Server.StoragePath)

	// Determine protocol version based on query parameters
	var protocolVersion string
	if a.Get("v2") != "" {
		protocolVersion = "v2"
	} else if a.Get("token") != "" {
func handleDownload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string) {
	fileInfo, err := getFileInfo(absFilename)
	if err != nil {
		log.WithError(err).Error("Failed to get file information")
		http.Error(w, "Not Found", http.StatusNotFound)
		downloadErrorsTotal.Inc()
		return
	} else if fileInfo.IsDir() {
func createFile(tempFilename string, r *http.Request) error {
    absDirectory := filepath.Dir(tempFilename)
    err := os.MkdirAll(absDirectory, os.ModePerm)
    if err != nil {
        return fmt.Errorf("failed to create directory: %v", err)
    }
func handleResumableDownload(absFilename string, w http.ResponseWriter, r *http.Request, fileSize int64) {
	rangeHeader := r.Header.Get("Range")
	if rangeHeader == "" {
		// If no Range header, serve the full file
		startTime := time.Now()
		http.ServeFile(w, r, absFilename)
		downloadDuration.Observe(time.Since(startTime).Seconds())
		downloadSizeBytes.Observe(float64(fileSize))
		downloadsTotal.Inc()
		return
	}
func handleChunkedUpload(tempFilename string, r *http.Request) error {
	log.WithField("file", tempFilename).Info("Handling chunked upload to temporary file")

	// Ensure the directory exists
	absDirectory := filepath.Dir(tempFilename)
	err := os.MkdirAll(absDirectory, os.ModePerm)
	if err != nil {
		return fmt.Errorf("failed to create directory: %v", err)
	}
func getFileInfo(absFilename string) (os.FileInfo, error) {
	if cachedInfo, found := fileInfoCache.Get(absFilename); found {
		if info, ok := cachedInfo.(os.FileInfo); ok {
			return info, nil
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
func handleMultipartUpload(w http.ResponseWriter, r *http.Request, absFilename string) error {
	err := r.ParseMultipartForm(32 << 20) // 32MB is the default used by FormFile
	if err != nil {
		log.WithError(err).Error("Failed to parse multipart form")
		http.Error(w, "Failed to parse multipart form", http.StatusBadRequest)
		return err
	}
