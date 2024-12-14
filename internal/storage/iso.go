package storage

import (
	"fmt"
	"io"
)

type ISOStorage struct {
	mountPoint string
	size       string
	charset    string
}

func NewISOStorage(mountPoint, size, charset string) *ISOStorage {
	return &ISOStorage{
		mountPoint: mountPoint,
		size:       size,
		charset:    charset,
	}
}

func (iso *ISOStorage) Upload(objectName string, reader io.Reader, size int64) error {
	// ISO-Speicherung implementieren
	return fmt.Errorf("ISO upload not implemented")
}

func (iso *ISOStorage) Download(objectName string) (io.Reader, error) {
	// ISO-Download implementieren
	return nil, fmt.Errorf("ISO download not implemented")
}

func (iso *ISOStorage) Delete(objectName string) error {
	// ISO-Löschen implementieren
	return fmt.Errorf("ISO delete not implemented")
}
