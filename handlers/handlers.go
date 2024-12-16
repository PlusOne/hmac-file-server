package handlers

import (
	"net/http"
	"strconv"

	"github.com/PlusOne/hmac-file-server/config"
	"github.com/PlusOne/hmac-file-server/workers"
)

// UploadTask represents a task for uploading files
type UploadTask struct {
	FilePath string
	UserID   int
	// Add other relevant fields
}

type ScanTask struct {
	// Add relevant fields here
}


var uploadQueue chan workers.UploadTask

func HandleRequest(w http.ResponseWriter, r *http.Request) {
	// Implementation from main.go's handleRequest
	// Utilize config.Conf, metrics, workers
	// Example:
	switch r.Method {
	case http.MethodPost:
		HandleUpload(w, r)
	case http.MethodGet:
		HandleDownload(w, r)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

func atoi(s string) int {
	i, _ := strconv.Atoi(s)
	return i
}

func HandleUpload(w http.ResponseWriter, r *http.Request) {
	// Implementation from main.go's handleUpload
	// Example:
	uploadTask := workers.UploadTask{
		FilePath: r.FormValue("filePath"),
		UserID:   atoi(r.FormValue("userID")),
	}
	uploadQueue <- uploadTask
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("Upload successful"))
}

func HandleDownload(w http.ResponseWriter, r *http.Request) {
	// Implementation from main.go's handleDownload
	// Example:
	filePath := r.URL.Query().Get("filePath")
	if filePath == "" {
		http.Error(w, "filePath is required", http.StatusBadRequest)
		return
	}
	// Add logic to handle file download using filePath
	// ...
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("Download successful: " + filePath))
}

func InitHandlers(uploadQueue chan workers.UploadTask, scanQueue chan workers.ScanTask, conf *config.Config) {
	// Initialize handlers with access to the queues and configuration
	http.HandleFunc("/upload", HandleUpload)
	http.HandleFunc("/download", HandleDownload)
	http.HandleFunc("/", HandleRequest)
}
