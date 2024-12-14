// internal/uploads/upload.go

package uploads

import (
	"context"
	"net/http"
	"path/filepath"
	"strings"

	"github.com/PlusOne/hmac-file-server/internal/config"
	"github.com/PlusOne/hmac-file-server/internal/logging"
)

type UploadTask struct {
	AbsFilename string
	Request     *http.Request
	Result      chan error
}

var (
	uploadQueue chan UploadTask
)

// InitUploadQueue initialisiert die Upload-Warteschlange
func InitUploadQueue() {
	uploadQueue = make(chan UploadTask, config.Conf.Workers.UploadQueueSize)
	logging.Log.Infof("Upload queue initialized with size: %d", config.Conf.Workers.UploadQueueSize)
}

// HandleUpload verarbeitet den Upload-Antrag
func HandleUpload(w http.ResponseWriter, r *http.Request, absFilename string) {
	// Validierung und Verarbeitung des Uploads
	err := handleMultipartUpload(w, r, absFilename)
	if err != nil {
		logging.Log.WithError(err).Error("Failed to handle multipart upload")
		http.Error(w, "Failed to handle multipart upload", http.StatusInternalServerError)
		return
	}
	w.WriteHeader(http.StatusCreated)
}

// InitializeUploadWorkerPool startet die Upload-Worker
func InitializeUploadWorkerPool(ctx context.Context) {
	for i := 0; i < config.Conf.Workers.NumWorkers; i++ {
		go uploadWorker(ctx, i)
	}
	logging.Log.Infof("Initialized %d upload workers", config.Conf.Workers.NumWorkers)
}

func uploadWorker(ctx context.Context, workerID int) {
	logging.Log.Infof("Upload worker %d started.", workerID)
	defer logging.Log.Infof("Upload worker %d stopped.", workerID)
	for {
		select {
		case <-ctx.Done():
			return
		case task, ok := <-uploadQueue:
			if !ok {
				return
			}
			err := processUpload(task)
			task.Result <- err
		}
	}
}

func processUpload(task UploadTask) error {
	// Implementiere die Logik für den Upload-Prozess
	// Dies könnte das Speichern der Datei, das Scannen mit ClamAV, etc. beinhalten
	return nil
}

func isExtensionAllowed(filename string) bool {
	if len(config.Conf.Uploads.AllowedExtensions) == 0 {
		return true // No restrictions if the list is empty
	}
	ext := strings.ToLower(filepath.Ext(filename))
	for _, allowedExt := range config.Conf.Uploads.AllowedExtensions {
		if strings.ToLower(allowedExt) == ext {
			return true
		}
	}
	return false
}

func handleMultipartUpload(w http.ResponseWriter, r *http.Request, absFilename string) error {
	// Hier wird die Upload-Logik implementiert
	// Zum Beispiel das Parsen der Multipart-Form, Validierung der Dateiendung, Speichern der Datei, etc.
	return nil
}
