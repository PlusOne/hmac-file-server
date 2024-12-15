package handlers

import (
    "io"
    "net/http"
    "os"
    "strconv"
    "strings"

    "github.com/sirupsen/logrus"
)

func DownloadHandler(w http.ResponseWriter, r *http.Request) {
    filePath := r.URL.Query().Get("file")
    if filePath == "" {
        http.Error(w, "File parameter is required", http.StatusBadRequest)
        return
    }

    file, err := os.Open(filePath)
    if err != nil {
        http.Error(w, "File not found", http.StatusNotFound)
        return
    }
    defer file.Close()

    fileInfo, err := file.Stat()
    if (err != nil) {
        http.Error(w, "Could not get file info", http.StatusInternalServerError)
        return
    }

    fileSize := fileInfo.Size()
    w.Header().Set("Content-Disposition", "attachment; filename="+fileInfo.Name())
    w.Header().Set("Accept-Ranges", "bytes")

    rangeHeader := r.Header.Get("Range")
    if rangeHeader == "" {
        http.ServeContent(w, r, fileInfo.Name(), fileInfo.ModTime(), file)
        return
    }

    rangeParts := strings.Split(rangeHeader, "=")
    if len(rangeParts) != 2 || rangeParts[0] != "bytes" {
        http.Error(w, "Invalid range header", http.StatusRequestedRangeNotSatisfiable)
        return
    }

    rangeSpec := strings.Split(rangeParts[1], "-")
    if len(rangeSpec) != 2 {
        http.Error(w, "Invalid range header", http.StatusRequestedRangeNotSatisfiable)
        return
    }

    start, err := strconv.ParseInt(rangeSpec[0], 10, 64)
    if err != nil {
        http.Error(w, "Invalid range start", http.StatusRequestedRangeNotSatisfiable)
        return
    }

    var end int64
    if rangeSpec[1] == "" {
        end = fileSize - 1
    } else {
        end, err = strconv.ParseInt(rangeSpec[1], 10, 64)
        if err != nil {
            http.Error(w, "Invalid range end", http.StatusRequestedRangeNotSatisfiable)
            return
        }
    }

    if start > end || start < 0 || end >= fileSize {
        http.Error(w, "Invalid range", http.StatusRequestedRangeNotSatisfiable)
        return
    }

    w.Header().Set("Content-Range", "bytes "+strconv.FormatInt(start, 10)+"-"+strconv.FormatInt(end, 10)+"/"+strconv.FormatInt(fileSize, 10))
    w.Header().Set("Content-Length", strconv.FormatInt(end-start+1, 10))
    w.WriteHeader(http.StatusPartialContent)

    _, err = file.Seek(start, 0)
    if err != nil {
        http.Error(w, "Could not seek file", http.StatusInternalServerError)
        return
    }

    _, err = io.CopyN(w, file, end-start+1)
    if err != nil {
        logrus.Errorf("Error serving file: %v", err)
    }
}