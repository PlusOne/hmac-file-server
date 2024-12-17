package main

import (
    "github.com/gin-gonic/gin"
    "net/http"
    "mime"
    "os"
    "strconv"
    "strings"
    "context"
    "path/filepath"
    "io"
)

func setupRouter() *gin.Engine {
    r := gin.Default()
    r.POST("/upload", handleUpload)
    r.GET("/download/:filename", handleDownload)
    return r
}

func handleUpload(c *gin.Context) {
    file, handler, err := c.Request.FormFile("file")
    if err != nil {
        c.JSON(http.StatusBadRequest, gin.H{"error": "Failed to retrieve file"})
        return
    }
    defer file.Close()

    absFilename := sanitizeFilePath(conf.Server.StoragePath, handler.Filename)
    tempFilename := absFilename + ".tmp"

    // Temporäre Datei erstellen
    outFile, err := os.Create(tempFilename)
    if err != nil {
        c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to create temp file"})
        return
    }
    defer outFile.Close()

    _, err = io.Copy(outFile, file)
    if err != nil {
        c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to save file"})
        return
    }

    // ClamAV-Scan durchführen
    if conf.ClamAV.ClamAVEnabled {
        err = scanFileWithClamAV(tempFilename)
        if err != nil {
            os.Remove(tempFilename)
            c.JSON(http.StatusBadRequest, gin.H{"error": "File failed ClamAV scan"})
            return
        }
    }

    // Datei-Versionierung
    if conf.Versioning.EnableVersioning {
        versionFile(absFilename)
    }

    // Datei an finalen Speicherort verschieben
    err = os.Rename(tempFilename, absFilename)
    if err != nil {
        c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to finalize file upload"})
        return
    }

    c.JSON(http.StatusCreated, gin.H{"status": "File uploaded successfully", "path": absFilename})
}

func handleDownload(c *gin.Context) {
    filename := c.Param("filename")
    absFilename, _ := sanitizeFilePath(conf.Server.StoragePath, filename)

    fileInfo, err := os.Stat(absFilename)
    if os.IsNotExist(err) {
        c.JSON(http.StatusNotFound, gin.H{"error": "File not found"})
        return
    }

    c.Header("Content-Type", mime.TypeByExtension(filepath.Ext(absFilename)))
    c.Header("Content-Length", strconv.FormatInt(fileInfo.Size(), 10))

    // Resumable Download
    if c.Request.Header.Get("Range") != "" {
        handleResumableDownload(absFilename, c.Writer, c.Request, fileInfo.Size())
        return
    }

    // Normales Herunterladen
    c.File(absFilename)
}
