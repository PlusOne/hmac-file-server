// internal/handlers/upload.go
package handlers

import (
    "net/http"
    "strings"

    "github.com/renz/hmac-file-server/internal/utils"
    "github.com/renz/hmac-file-server/internal/metrics"
)

// HandleUpload handles file upload requests
func HandleUpload(w http.ResponseWriter, r *http.Request) {
    // Extract the file path from the URL
    filePath := strings.TrimPrefix(r.URL.Path, "/upload/")
    absFilePath, err := utils.SanitizeFilePath("/uploads", filePath)
    if err != nil {
        http.Error(w, "Invalid file path", http.StatusBadRequest)
        return
    }

    // Validate file extension
    if !utils.IsExtensionAllowed(absFilePath) {
        http.Error(w, "File extension not allowed", http.StatusForbidden)
        return
    }

    // Create the file
    err = utils.CreateFile(absFilePath, r.Body)
    if err != nil {
        http.Error(w, "Failed to create file", http.StatusInternalServerError)
        return
    }

    // Increment the upload counter
    metrics.UploadsTotal.Inc()

    w.WriteHeader(http.StatusCreated)
    w.Write([]byte("Upload successful"))
}