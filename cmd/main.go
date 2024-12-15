package handlers

import (
    "fmt"
    "io"
    "net/http"
    "os"
    "path/filepath"
    "time"

    "github.com/PlusOne/hmac-file-server/workers"
    "github.com/PlusOne/hmac-file-server/config"
    "github.com/sirupsen/logrus"
)

var (
    uploadQueue chan tasks.UploadTask
    scanQueue   chan tasks.ScanTask
    conf        *Config
)

// InitHandlers initialisiert die Handler mit den notwendigen Queues und Konfiguration
func InitHandlers(uq chan tasks.UploadTask, sq chan tasks.ScanTask, cfg *Config) {
    uploadQueue = uq
    scanQueue = sq
    conf = cfg
}

// UploadHandler behandelt Datei-Uploads
func UploadHandler(w http.ResponseWriter, r *http.Request) {
    // Überprüfe die HTTP-Methode
    if r.Method != http.MethodPost {
        http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
        return
    }

    // Parse das Multipart-Form
    err := r.ParseMultipartForm(10 << 20) // Limit auf 10 MB
    if err != nil {
        http.Error(w, "Unable to parse form", http.StatusBadRequest)
        return
    }

    // Hole die Datei
    file, header, err := r.FormFile("file")
    if err != nil {
        http.Error(w, "Invalid file", http.StatusBadRequest)
        return
    }
    defer file.Close()

    // Speichere die Datei im Speicherpfad
    filePath := filepath.Join(conf.Server.StoragePath, header.Filename)
    out, err := os.Create(filePath)
    if err != nil {
        http.Error(w, "Unable to create the file", http.StatusInternalServerError)
        return
    }
    defer out.Close()

    fileSize, err := io.Copy(out, file)
    if err != nil {
        http.Error(w, "Unable to save the file", http.StatusInternalServerError)
        return
    }

    logrus.Infof("File uploaded: %s (%d bytes)", header.Filename, fileSize)

    // Erstelle eine UploadTask
    uploadTask := tasks.UploadTask{
        FileName:   header.Filename,
        FileSize:   fileSize,
        UploadedAt: time.Now(),
    }

    // Sende die UploadTask zur Upload-Queue
    uploadQueue <- uploadTask

    // Optional: Sende eine ScanTask zur Scan-Queue, wenn ClamAV aktiviert ist
    if conf.ClamAV.ClamAVEnabled {
        scanTask := tasks.ScanTask{
            FileName:   filePath,
            FileSize:   fileSize,
            UploadedAt: time.Now(),
        }
        scanQueue <- scanTask
    }

    // Antworte dem Client
    w.WriteHeader(http.StatusOK)
    fmt.Fprintf(w, "File uploaded successfully: %s", header.Filename)
}
