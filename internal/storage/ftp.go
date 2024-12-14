package storage

import (
	"fmt"
	"io"
	"path/filepath"
	"time"

	"github.com/jlaffaye/ftp"
)

type FTPStorage struct {
	client   *ftp.ServerConn
	basePath string
}

func NewFTPStorage(server, username, password, basePath string) (*FTPStorage, error) {
	client, err := ftp.Dial(server, ftp.DialWithTimeout(5*time.Second))
	if err != nil {
		return nil, fmt.Errorf("failed to connect to FTP server: %w", err)
	}

	err = client.Login(username, password)
	if err != nil {
		return nil, fmt.Errorf("failed to login to FTP server: %w", err)
	}

	return &FTPStorage{client: client, basePath: basePath}, nil
}

func (f *FTPStorage) Upload(objectName string, reader io.Reader, size int64) error {
	path := filepath.Join(f.basePath, objectName)
	return f.client.Stor(path, reader)
}

func (f *FTPStorage) Download(objectName string) (io.Reader, error) {
	path := filepath.Join(f.basePath, objectName)
	reader, err := f.client.Retr(path)
	if err != nil {
		return nil, fmt.Errorf("failed to retrieve file: %w", err)
	}
	return reader, nil
}

func (f *FTPStorage) Delete(objectName string) error {
	path := filepath.Join(f.basePath, objectName)
	return f.client.Delete(path)
}
