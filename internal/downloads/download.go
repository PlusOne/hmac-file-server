// internal/downloads/download.go

package downloads

import (
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"time"

	"github.com/PlusOne/hmac-file-server/internal/config"
	"github.com/PlusOne/hmac-file-server/internal/logging"
)

func HandleDownload(w http.ResponseWriter, r *http.Request, absFilename string) {
	fileInfo, err := os.Stat(absFilename)
	if err != nil {
		logging.Log.WithError(err).Error("Failed to get file information")
		http.Error(w, "Not Found", http.StatusNotFound)
		return
	} else if fileInfo.IsDir() {
		logging.Log.Warn("Directory listing forbidden")
		http.Error(w, "Forbidden", http.StatusForbidden)
		return
	}

	contentType := getContentType(absFilename)
	w.Header().Set("Content-Type", contentType)

	// Handle resumable downloads
	if config.Conf.Uploads.ResumableUploadsEnabled {
		handleResumableDownload(w, r, absFilename, fileInfo.Size())
		return
	}

	if r.Method == http.MethodHead {
		w.Header().Set("Content-Length", strconv.FormatInt(fileInfo.Size(), 10))
		return
	} else {
		// Measure download duration
		startTime := time.Now()
		logging.Log.Infof("Initiating download for file: %s", absFilename)
		http.ServeFile(w, r, absFilename)
		// Update metrics here if notwendig
		logging.Log.Infof("File downloaded successfully: %s", absFilename)
		return
	}
}

func getContentType(filename string) string {
	ext := strings.ToLower(filepath.Ext(filename))
	switch ext {
	case ".txt":
		return "text/plain"
	case ".pdf":
		return "application/pdf"
	case ".png":
		return "image/png"
	case ".jpg", ".jpeg":
		return "image/jpeg"
	case ".gif":
		return "image/gif"
	// Füge weitere Typen hinzu nach Bedarf
	default:
		return "application/octet-stream"
	}
}

func handleResumableDownload(w http.ResponseWriter, r *http.Request, absFilename string, fileSize int64) {
	// Implementiere die Logik für resumable Downloads
}
