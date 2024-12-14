package storage

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
)

type LocalStorage struct {
	basePath string
}

func NewLocalStorage(basePath string) *LocalStorage {
	return &LocalStorage{basePath: basePath}
}

func (l *LocalStorage) Upload(objectName string, reader io.Reader, size int64) error {
	fullPath := filepath.Join(l.basePath, objectName)
	file, err := os.Create(fullPath)
	if err != nil {
		return fmt.Errorf("error creating file: %w", err)
	}
	defer file.Close()
	_, err = io.Copy(file, reader)
	return err
}

func (l *LocalStorage) Download(objectName string) (io.Reader, error) {
	fullPath := filepath.Join(l.basePath, objectName)
	file, err := os.Open(fullPath)
	if err != nil {
		return nil, fmt.Errorf("error opening file: %w", err)
	}
	return file, nil
}

func (l *LocalStorage) Delete(objectName string) error {
	fullPath := filepath.Join(l.basePath, objectName)
	err := os.Remove(fullPath)
	if err != nil {
		return fmt.Errorf("error deleting file: %w", err)
	}
	return nil
}
