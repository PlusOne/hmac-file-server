package fileops

import (
	"bufio"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"

	"github.com/sirupsen/logrus"
)

func HandleChunkedUpload(tempFilename string, r *http.Request, chunkSize int, log *logrus.Logger) error {
	log.WithField("file", tempFilename).Info("Handling chunked upload")

	absDirectory := filepath.Dir(tempFilename)
	err := os.MkdirAll(absDirectory, os.ModePerm)
	if err != nil {
		return fmt.Errorf("failed to create directory: %v", err)
	}

	targetFile, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return fmt.Errorf("failed to open file: %v", err)
	}
	defer targetFile.Close()

	writer := bufio.NewWriterSize(targetFile, chunkSize)
	buffer := make([]byte, chunkSize)

	totalBytes := int64(0)
	for {
		n, err := r.Body.Read(buffer)
		if err != nil && err != io.EOF {
			return fmt.Errorf("failed to read request body: %v", err)
		}
		if n == 0 {
			break
		}

		_, err = writer.Write(buffer[:n])
		if err != nil {
			return fmt.Errorf("failed to write to file: %v", err)
		}
		totalBytes += int64(n)
	}

	err = writer.Flush()
	if err != nil {
		return fmt.Errorf("failed to flush writer: %v", err)
	}

	log.WithFields(logrus.Fields{"temp_file": tempFilename, "total_bytes": totalBytes}).Info("Chunked upload completed successfully")
	return nil
}
