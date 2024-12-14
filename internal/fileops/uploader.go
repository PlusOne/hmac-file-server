package fileops

import (
	"bufio"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"sync"

	"github.com/sirupsen/logrus"
)

// createFile creates a file from the request body
func CreateFile(tempFilename string, r *http.Request, bufferPool *sync.Pool, log *logrus.Logger) error {
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
