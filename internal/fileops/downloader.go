package fileops

import (
	"fmt"
	"net/http"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/sirupsen/logrus"
)

func HandleResumableDownload(absFilename string, w http.ResponseWriter, r *http.Request, fileSize int64, log *logrus.Logger) {
	startTime := time.Now()

	rangeHeader := r.Header.Get("Range")
	if rangeHeader == "" {
		http.ServeFile(w, r, absFilename)
		// observe metrics here if needed
		log.Infof("File fully downloaded: %s, duration: %v", absFilename, time.Since(startTime))
		return
	}

	ranges := strings.Split(strings.TrimPrefix(rangeHeader, "bytes="), "-")
	if len(ranges) != 2 {
		http.Error(w, "Invalid Range", http.StatusRequestedRangeNotSatisfiable)
		return
	}

	start, err := strconv.ParseInt(ranges[0], 10, 64)
	if err != nil || start < 0 {
		http.Error(w, "Invalid Range", http.StatusRequestedRangeNotSatisfiable)
		return
	}

	end := fileSize - 1
	if ranges[1] != "" {
		end, err = strconv.ParseInt(ranges[1], 10, 64)
		if err != nil || end < start || end >= fileSize {
			http.Error(w, "Invalid Range", http.StatusRequestedRangeNotSatisfiable)
			return
		}
	}

	w.Header().Set("Content-Range", fmt.Sprintf("bytes %d-%d/%d", start, end, fileSize))
	w.Header().Set("Content-Length", strconv.FormatInt(end-start+1, 10))
	w.Header().Set("Accept-Ranges", "bytes")
	w.WriteHeader(http.StatusPartialContent)

	file, err := os.Open(absFilename)
	if err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}
	defer file.Close()

	_, err = file.Seek(start, 0)
	if err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}

	buffer := make([]byte, 32*1024)
	remaining := end - start + 1
	for remaining > 0 {
		if int64(len(buffer)) > remaining {
			buffer = buffer[:remaining]
		}
		n, readErr := file.Read(buffer)
		if n > 0 {
			if _, writeErr := w.Write(buffer[:n]); writeErr != nil {
				log.WithError(writeErr).Errorf("Failed to write to response (Range: %d-%d)", start, end)
				return
			}
			remaining -= int64(n)
		}
		if readErr != nil {
			break
		}
	}
	// observe metrics
	log.Infof("Partial file downloaded: %s (Range: %d-%d), duration: %v", absFilename, start, end, time.Since(startTime))
}
