package file

import (
	"bufio"
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"mime"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"time"

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"github.com/patrickmn/go-cache"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
	"pkg/metrics"
	"pkg/logging"
)

func handleMultipartUpload(w http.ResponseWriter, r *http.Request, absFilename string) error {
	err := r.ParseMultipartForm(32 << 20)
	if err != nil {
		logging.WithError(err).Error("Failed to parse multipart form")
		http.Error(w, "Failed to parse multipart form", http.StatusBadRequest)
		return err
	}

	file, handler, err := r.FormFile("file")
	if err != nil {
		logging.WithError(err).Error("Failed to retrieve file from form data")
		http.Error(w, "Failed to retrieve file from form data", http.StatusBadRequest)
		return err
	}
	defer file.Close()

	if !isExtensionAllowed(handler.Filename) {
		logging.WithFields(logrus.Fields{"filename": handler.Filename, "extension": filepath.Ext(handler.Filename)}).Warn("Attempted upload with disallowed file extension")
		http.Error(w, "Disallowed file extension. Allowed: "+strings.Join(viper.GetStringSlice("uploads.allowed_extensions"), ", "), http.StatusForbidden)
		metrics.UploadErrorsTotal.Inc()
		return fmt.Errorf("disallowed file extension")
	}

	tempFilename := absFilename + ".tmp"
	tempFile, err := os.OpenFile(tempFilename, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		logging.WithError(err).Error("Failed to create temporary file")
		http.Error(w, "Failed to create temporary file", http.StatusInternalServerError)
		return err
	}
	defer tempFile.Close()

	_, err = io.Copy(tempFile, file)
	if err != nil {
		logging.WithError(err).Error("Failed to copy uploaded file to temporary file")
		http.Error(w, "Failed to copy uploaded file", http.StatusInternalServerError)
		return err
	}

	if clamClient != nil {
		err := scanFile(tempFilename, clamClient)
		if err != nil {
			logging.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Warn("ClamAV detected a virus")
			os.Remove(tempFilename)
			metrics.UploadErrorsTotal.Inc()
			return err
		}
	}

	if viper.GetBool("versioning.enable_versioning") {
		existing, _ := fileExists(absFilename)
		if existing {
			err := versionFile(absFilename)
			if err != nil {
		}
				os.Remove(tempFilename)
	} else {
		logging.Warn("ClamAV is not available or file extension not in scan list. Proceeding without virus scan.")
	}


	err = os.Rename(tempFilename, absFilename)
	if err != nil {
		os.Remove(tempFilename)
		return fmt.Errorf("failed to move file to final destination: %w", err)
	}

	logging.WithField("file", absFilename).Info("File uploaded and scanned successfully")
				return err
			}
			logging.Infof("File versioned successfully: %s", absFilename)

	}
	logging.WithField("file", absFilename).Info("Scanning file with ClamAV")

	scanResultChan, err := clamClient.ScanFile(absFilename)
	if err != nil {
		logging.WithError(err).Error("Failed to initiate ClamAV scan")
		return fmt.Errorf("failed to initiate ClamAV scan: %w", err)
	}

	scanResult := <-scanResultChan
	if scanResult == nil {
		logging.Error("Failed to receive scan result from ClamAV")
		return fmt.Errorf("failed to receive scan result")
	}

	switch scanResult.Status {
		logging.Warnf("Callback-URL provided (%s) but not needed. Ignoring.", callbackURL)
		logging.WithField("file", absFilename).Info("ClamAV scan passed")
		return nil
	case clamd.RES_FOUND:
		logging.WithFields(logrus.Fields{"file": absFilename, "description": scanResult.Description}).Warn("ClamAV detected a virus")
		return fmt.Errorf("virus detected: %s", scanResult.Description)
	default:
		logging.WithFields(logrus.Fields{"file": absFilename, "status": scanResult.Status, "description": scanResult.Description}).Warn("ClamAV scan returned unexpected status")
		return fmt.Errorf("unexpected ClamAV status: %s", scanResult.Description)
	}
}

func processUpload(task UploadTask) error {
	logging.Infof("Started processing upload for file: %s", task.AbsFilename)
	semaphore <- struct{}{}
	defer func() { <-semaphore }()

	absFilename := task.AbsFilename
	tempFilename := absFilename + ".tmp"
	r := task.Request

	logging.Infof("Processing upload for file: %s", absFilename)
	startTime := time.Now()

	if viper.GetBool("uploads.chunked_uploads_enabled") {
		chunkSize, err := parseSize(viper.GetString("uploads.chunk_size"))
		if err != nil {
			logging.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Error("Error parsing chunk size")
			metrics.UploadDuration.Observe(time.Since(startTime).Seconds())
			return err
		}
		err = handleChunkedUpload(tempFilename, r, int(chunkSize))
		if err != nil {
			metrics.UploadDuration.Observe(time.Since(startTime).Seconds())
			logging.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Error("Failed to handle chunked upload")
			return err
		}
	} else {
		err := createFile(tempFilename, r)
		if err != nil {
			logging.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Error("Error creating file")
			metrics.UploadDuration.Observe(time.Since(startTime).Seconds())
			return err
		}
	}

	if clamClient != nil && shouldScanFile(absFilename) {
		err := scanFile(tempFilename, clamClient)
		if err != nil {
			logging.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Warn("ClamAV detected a virus or scan failed")
			os.Remove(tempFilename)
			metrics.UploadErrorsTotal.Inc()
			return err
		}
		logging.Infof("ClamAV scan passed for file: %s", tempFilename)
	} else {
		logging.Warn("ClamAV is not available or file extension not in scan list. Proceeding without virus scan.")
	}

	if viper.GetBool("versioning.enable_versioning") {
		existing, _ := fileExists(absFilename)
		if existing {
			logging.Infof("File %s exists. Initiating versioning.", absFilename)
			err := versionFile(absFilename)
			if err != nil {
				logging.WithFields(logrus.Fields{"file": absFilename, "error": err}).Error("Error versioning file")
				os.Remove(tempFilename)
				return err
			}
			logging.Infof("File versioned successfully: %s", absFilename)
		}
	}

	err := os.Rename(tempFilename, absFilename)
	defer func() {
		if err != nil {
			os.Remove(tempFilename)
		}
	}()
	if err != nil {
		os.Remove(tempFilename)
		return fmt.Errorf("failed to move file to final destination: %w", err)
	}
	logging.Infof("File moved to final destination: %s", absFilename)

	callbackURL := r.Header.Get("Callback-URL")
	if callbackURL != "" {
		logging.Warnf("Callback-URL provided (%s) but not needed. Ignoring.", callbackURL)
	}

	if viper.GetBool("server.deduplication_enabled") {
		err = handleDeduplication(context.Background(), absFilename)
		if err != nil {
			logging.WithError(err).Error("Deduplication failed")
			metrics.UploadErrorsTotal.Inc()
			return err
		}
		logging.Infof("Deduplication handled successfully for file: %s", absFilename)
	}

	if viper.GetBool("iso.enabled") {
		err = handleISOContainer(absFilename)
		if err != nil {
			logging.WithError(err).Error("ISO container handling failed")
			metrics.UploadErrorsTotal.Inc()
			return err
		}
		logging.Infof("ISO container handled successfully for file: %s", absFilename)
	}

	logging.WithFields(logrus.Fields{"file": absFilename}).Info("File uploaded and processed successfully")

	metrics.UploadDuration.Observe(time.Since(startTime).Seconds())
	metrics.UploadsTotal.Inc()
	logging.Infof("Finished processing upload for file: %s", task.AbsFilename)
	return nil
}

func handleDownload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string) {
	fileInfo, err := getFileInfo(absFilename)
	if err != nil {
		logging.WithError(err).Error("Failed to get file information")
		http.Error(w, "Not Found", http.StatusNotFound)
		metrics.DownloadErrorsTotal.Inc()
		return
	} else if fileInfo.IsDir() {
		logging.Warn("Directory listing forbidden")
		http.Error(w, "Forbidden", http.StatusForbidden)
		metrics.DownloadErrorsTotal.Inc()
		return
	}

	contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))

	logging.WithFields(logrus.Fields{"temp_file": tempFilename, "total_bytes": totalBytes}).Info("Chunked upload completed successfully")
	metrics.ObserveUploadSizeBytes(float64(totalBytes))
	return nil
}

func fileExists(filePath string) (bool, int64) {
	if cachedInfo, found := fileInfoCache.Get(filePath); found {
		if info, ok := cachedInfo.(os.FileInfo); ok {
			return !info.IsDir(), info.Size()
		}
	}

	fileInfo, err := os.Stat(filePath)
	if os.IsNotExist(err) {
		return false, 0
	} else if err != nil {
		logging.Error("Error checking file existence:", err)
		return false, 0
	}

	fileInfoCache.Set(filePath, fileInfo, cache.DefaultExpiration)
	return !fileInfo.IsDir(), fileInfo.Size()
}

func versionFile(absFilename string) error {
	versionDir := absFilename + "_versions"

	err := os.MkdirAll(versionDir, os.ModePerm)
	if err != nil {
		return fmt.Errorf("failed to create version directory: %v", err)
	}

	timestamp := time.Now().Format("20060102-150405")
	versionedFilename := filepath.Join(versionDir, filepath.Base(absFilename)+"."+timestamp)

	err = os.Rename(absFilename, versionedFilename)
	if err != nil {
		return fmt.Errorf("failed to version the file: %v", err)
	}

	logging.WithFields(logrus.Fields{
		"original":     absFilename,
		"versioned_as": versionedFilename,
	}).Info("Versioned old file")
	return cleanupOldVersions(versionDir)
}

func cleanupOldVersions(versionDir string) error {
	files, err := os.ReadDir(versionDir)
	if err != nil {
		return fmt.Errorf("failed to list version files: %v", err)
	}

	if config.Conf.Versioning.MaxVersions > 0 && len(files) > config.Conf.Versioning.MaxVersions {
		excessFiles := len(files) - config.Conf.Versioning.MaxVersions
		for i := 0; i < excessFiles; i++ {
			err := os.Remove(filepath.Join(versionDir, files[i].Name()))
			if err != nil {
				return fmt.Errorf("failed to remove old version: %v", err)
			}
			logging.WithField("file", files[i].Name()).Info("Removed old version")
		}
	}

	return nil
}

func handleDeduplication(ctx context.Context, absFilename string) error {
	checksum, err := computeSHA256(ctx, absFilename)
	if err != nil {
		logging.Errorf("Failed to compute SHA256 for %s: %v", absFilename, err)
		return fmt.Errorf("checksum computation failed: %w", err)
	}
	logging.Debugf("Computed checksum for %s: %s", absFilename, checksum)

	existingPath, err := redisClient.Get(ctx, checksum).Result()
	if err != nil {
		if err == redis.Nil {
			err = redisClient.Set(ctx, checksum, absFilename, 0).Err()
			if err != nil {
				logging.Errorf("Redis error setting checksum %s: %v", checksum, err)
				return fmt.Errorf("redis error: %w", err)
			}
			logging.Infof("Stored new checksum %s for file %s", checksum, absFilename)
			return nil
		}
		logging.Errorf("Redis error fetching checksum %s: %v", checksum, err)
		return fmt.Errorf("redis error: %w", err)
	}

	if existingPath != absFilename {
		if _, err := os.Stat(existingPath); os.IsNotExist(err) {
			logging.Errorf("Existing file for checksum %s not found at %s", checksum, existingPath)
			return fmt.Errorf("existing file not found: %w", err)
		}

		err = os.Link(existingPath, absFilename)
		if err != nil {
			logging.Errorf("Failed linking %s to %s: %v", existingPath, absFilename, err)
			return fmt.Errorf("failed link: %w", err)
		}
		logging.Infof("Created hard link for duplicate file %s -> %s", absFilename, existingPath)
	} else {
		logging.Infof("File %s already exists with same checksum", absFilename)
	}

	return nil
}

func computeSHA256(ctx context.Context, filePath string) (string, error) {
	if filePath == "" {
		return "", fmt.Errorf("computeSHA256: filePath cannot be empty")
	}

	file, err := os.Open(filePath)
	if err != nil {
		logging.Errorf("Failed to open file for checksum: %v", err)
		return "", fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	hasher := sha256.New()
	reader := bufio.NewReader(file)

	buffer := make([]byte, 4096)
	for {
		select {
		case <-ctx.Done():
			return "", fmt.Errorf("operation cancelled")
		default:
			n, err := reader.Read(buffer)
			if n > 0 {
				if _, wErr := hasher.Write(buffer[:n]); wErr != nil {
					return "", fmt.Errorf("hasher write error: %w", wErr)
				}
			}
			if err != nil {
				if err == io.EOF {
					sum := hex.EncodeToString(hasher.Sum(nil))
					logging.Debugf("Checksum computed: %s", sum)
					return sum, nil
				}
				return "", fmt.Errorf("read error: %w", err)
			}
		}
	}
}

func handleISOContainer(absFilename string) error {
	isoPath := filepath.Join(config.Conf.ISO.MountPoint, "container.iso")

	err := CreateISOContainer([]string{absFilename}, isoPath, config.Conf.ISO.Size, config.Conf.ISO.Charset)
	if err != nil {
		return fmt.Errorf("failed to create ISO container: %w", err)
	}

	if err := os.MkdirAll(config.Conf.ISO.MountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	err = MountISOContainer(isoPath, config.Conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to mount ISO container: %w", err)
	}

	err = UnmountISOContainer(config.Conf.ISO.MountPoint)
	if err != nil {
		return fmt.Errorf("failed to unmount ISO: %w", err)
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
	if err := os.MkdirAll(mountPoint, os.ModePerm); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	var output []byte
	var err error
	for i := 0; i < 3; i++ {
		cmd := exec.Command("mount", "-o", "loop,ro", isoPath, mountPoint)
		output, err = cmd.CombinedOutput()
		if err == nil {
			logging.Infof("ISO container mounted at %s", mountPoint)
			return nil
		}
		logging.Warnf("Mount attempt %d failed: %v, output: %s", i+1, err, string(output))
		time.Sleep(2 * time.Second)
	}
	return fmt.Errorf("failed to mount ISO: %w, output: %s", err, string(output))
}

func UnmountISOContainer(mountPoint string) error {
	cmd := exec.Command("umount", mountPoint)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
	if contentType == "" {

		metrics.DownloadSizeBytes.Observe(float64(fileInfo.Size()))
		metrics.DownloadsTotal.Inc()
		logging.Infof("File downloaded successfully: %s", absFilename)
		return
	}
}
	if r.Method == http.MethodHead {
		metrics.DownloadDuration.Observe(time.Since(startTime).Seconds())
		w.Header().Set("Content-Length", strconv.FormatInt(fileInfo.Size(), 10))
		metrics.DownloadsTotal.Inc()
		return
		logging.Infof("Initiating download for file: %s", absFilename)
		http.ServeFile(w, r, absFilename)
	} else {
		startTime := time.Now()
		handleResumableDownload(absFilename, w, r, fileInfo.Size())
		return
	}
		contentType = "application/octet-stream"
	}
	w.Header().Set("Content-Type", contentType)

	if viper.GetBool("uploads.resumable_uploads_enabled") {
		err = handleDeduplication(context.Background(), absFilename)
		if err != nil {
			logging.WithError(err).Error("Deduplication failed")
		logging.Infof("Deduplication handled successfully for file: %s", absFilename)
	}

	if config.Conf.ISO.Enabled {
	logging.Infof("Finished processing upload for file: %s", task.AbsFilename)
	return nil

			return fmt.Errorf("failed to write to file: %v", err)
		}
		totalBytes += int64(n)
	if err != nil {
		return fmt.Errorf("failed to flush writer: %v", err)
	}
	}

	err = writer.Flush()
	file, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return err
	}

			return fmt.Errorf("failed to read request body: %v", err)
		}
		if n == 0 {

		_, err = writer.Write(buffer[:n])
		if err != nil {
			break
		}

	targetFile, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		if err != nil && err != io.EOF {
	defer targetFile.Close()

	writer := bufio.NewWriterSize(targetFile, chunkSize)
	buffer := make([]byte, chunkSize)

	totalBytes := int64(0)
	for {
		n, err := r.Body.Read(buffer)
		return fmt.Errorf("failed to open file: %v", err)
	}
	bufWriter := bufio.NewWriter(file)
		return err
	}
	if err != nil {
		return fmt.Errorf("failed to create directory: %v", err)
	}
	logging.WithField("file", tempFilename).Info("Handling chunked upload to temporary file")

	absDirectory := filepath.Dir(tempFilename)
	err := os.MkdirAll(absDirectory, os.ModePerm)
		return fmt.Errorf("unexpected ClamAV status: %s", scanResult.Description)
	}
}

func handleChunkedUpload(tempFilename string, r *http.Request, chunkSize int) error {

	return nil
		if strings.ToLower(scanExt) == ext {
			return true
		}
	}
	return false
	logging.WithField("file", filePath).Info("Scanning file with ClamAV")

	if scanResult == nil {
		logging.WithFields(logrus.Fields{"file": filePath, "status": scanResult.Status, "description": scanResult.Description}).Warn("ClamAV scan returned unexpected status")
		return fmt.Errorf("failed to receive scan result")
		logging.WithField("file", filePath).Info("ClamAV scan passed")
		logging.WithFields(logrus.Fields{"file": filePath, "description": scanResult.Description}).Warn("ClamAV detected a virus")
		return fmt.Errorf("virus detected: %s", scanResult.Description)
	default:
		return nil
	case clamd.RES_FOUND:
	}

	switch scanResult.Status {
	case clamd.RES_OK:
		logging.Error("Failed to receive scan result from ClamAV")
	scanResultChan, err := clamClient.ScanFile(filePath)
	if err != nil {
		logging.WithError(err).Error("Failed to initiate ClamAV scan")
		return fmt.Errorf("failed to initiate ClamAV scan: %w", err)
	}

	scanResult := <-scanResultChan

func scanFileWithClamAV(filePath string) error {
}
}

func shouldScanFile(filename string) bool {
	ext := strings.ToLower(filepath.Ext(filename))
	for _, scanExt := range config.Conf.ClamAV.ScanFileExtensions {

	_, err = io.CopyBuffer(bufWriter, r.Body, *bufPtr)
	if err != nil {
	defer bufWriter.Flush()
	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)
	defer file.Close()
	err := os.MkdirAll(filepath.Dir(tempFilename), 0755)
	if err != nil {
		return err
	}
}

func createFile(tempFilename string, r *http.Request) error {
		err = handleISOContainer(absFilename)
			logging.WithError(err).Error("ISO container handling failed")

	metrics.ObserveUploadDuration(time.Since(startTime).Seconds())
	metrics.IncUploadsTotal()

	logging.WithFields(logrus.Fields{"file": absFilename}).Info("File uploaded and processed successfully")
			metrics.IncUploadErrorsTotal()
			return err
		}
		logging.Infof("ISO container handled successfully for file: %s", absFilename)
	}
		if err != nil {
			metrics.IncUploadErrorsTotal()
			return err
		}
	}

	if config.Conf.Server.DeduplicationEnabled {
	case clamd.RES_OK:
			os.Remove(tempFilename)
		}
	logging.Infof("File moved to final destination: %s", absFilename)
	if callbackURL != "" {

	callbackURL := r.Header.Get("Callback-URL")
	}()
	if err != nil {
		return fmt.Errorf("failed to move file to final destination: %w", err)
	}
		os.Remove(tempFilename)

	err := os.Rename(tempFilename, absFilename)
	defer func() {
		if err != nil {
func scanFile(absFilename string, clamClient *clamd.Clamd) error {
		}
	metrics.UploadsTotal.Inc()
	return nil
}
				logging.WithFields(logrus.Fields{"file": absFilename, "error": err}).Error("Error versioning file")
				os.Remove(tempFilename)
			err := versionFile(absFilename)
			if err != nil {
	if config.Conf.Versioning.EnableVersioning {
		existing, _ := fileExists(absFilename)
		if existing {
			logging.Infof("File %s exists. Initiating versioning.", absFilename)
				return err
			}
		}
	}
		logging.Infof("ClamAV scan passed for file: %s", tempFilename)
				logging.WithFields(logrus.Fields{"file": absFilename, "error": err}).Error("Error versioning file")
	"os"
			return &buf
		},
	}
	semaphore = make(chan struct{}, 10)
	semaphore <- struct{}{}
	defer func() { <-semaphore }()

	tempFilename := absFilename + ".tmp"
	r := task.Request

			logging.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Error("Error parsing chunk size")
			metrics.ObserveUploadDuration(time.Since(startTime).Seconds())
			return err
		err = handleChunkedUpload(tempFilename, r, int(chunkSize))
			logging.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Error("Error creating file")
			metrics.ObserveUploadDuration(time.Since(startTime).Seconds())
			logging.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Warn("ClamAV detected a virus or scan failed")
			os.Remove(tempFilename)
			metrics.IncUploadErrorsTotal()
			return err
			return err
		}
	}

	if clamClient != nil && shouldScanFile(absFilename) {
		err := scanFileWithClamAV(tempFilename)
		if err != nil {
		if err != nil {
			logging.WithFields(logrus.Fields{"file": tempFilename, "error": err}).Error("Failed to handle chunked upload")
		err := createFile(tempFilename, r)
		if err != nil {
			return err
		}
	} else {
			metrics.ObserveUploadDuration(time.Since(startTime).Seconds())
		}
		chunkSize, err := parseSize(config.Conf.Uploads.ChunkSize)
		if err != nil {
	logging.Infof("Processing upload for file: %s", absFilename)
	startTime := time.Now()

	if config.Conf.Uploads.ChunkedUploadsEnabled {
	absFilename := task.AbsFilename
)

func ProcessUpload(task UploadTask) error {
	logging.Infof("Started processing upload for file: %s", task.AbsFilename)
	"github.com/patrickmn/go-cache"
	"pkg/logging"
	"pkg/metrics"
)

var (
	clamClient     *clamd.Clamd
	redisClient    *redis.Client
	fileInfoCache  *cache.Cache
	bufferPool     = sync.Pool{
		New: func() interface{} {
			buf := make([]byte, 32*1024)
	"github.com/sirupsen/logrus"
	"pkg/config"

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"
package file

import (
	"bufio"
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"mime"
		err := handleMultipartUpload(w, r, absFilename)
		if err != nil {
			http.Error(w, "Failed to handle multipart upload", http.StatusInternalServerError)
			return
		}
	}

	// ...existing request handling logic...
}

// ...other file-related functions...
	"net/http"
package file

func HandleRequest(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost && strings.Contains(r.Header.Get("Content-Type"), "multipart/form-data") {
		// Handle multipart upload

import (
	"net/http"
	"strings"
	"pkg/logging"
)

		}
func CreateFile(tempFilename string, r *http.Request, bufferPool *sync.Pool) error {
	err := os.MkdirAll(filepath.Dir(tempFilename), 0755)
	if err != nil {
		return err
	}

	file, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return err
	}
	defer file.Close()

	bufWriter := bufio.NewWriter(file)
	defer bufWriter.Flush()
	bufPtr := bufferPool.Get().(*[]byte)
	defer bufferPool.Put(bufPtr)

	_, err = io.CopyBuffer(bufWriter, r.Body, *bufPtr)
	if err != nil {
		return err
	}

	return nil
}

func IsExtensionAllowed(filename string, allowedExtensions []string) bool {
	if len(allowedExtensions) == 0 {
		return true
	}
	ext := strings.ToLower(filepath.Ext(filename))
	for _, allowedExt := range allowedExtensions {
		if strings.ToLower(allowedExt) == ext {
			return true
		}
	}
	return false
}

// ...other file-related functions...
	}

	// ...existing request handling logic...
}

// ...other file-related functions...
	"net/http"
package file

func HandleRequest(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost && strings.Contains(r.Header.Get("Content-Type"), "multipart/form-data") {
		// Handle multipart upload

import (
	"net/http"
	"strings"
	"pkg/logging"
)