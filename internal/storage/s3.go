package storage

import (
	"context"
	"fmt"
	"io"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
)

type S3Storage struct {
	client   *minio.Client
	bucket   string
	location string
}

func NewS3Storage(endpoint, accessKey, secretKey, bucket, location string) (*S3Storage, error) {
	client, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKey, secretKey, ""),
		Secure: true,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to create S3 client: %w", err)
	}

	return &S3Storage{client: client, bucket: bucket, location: location}, nil
}

func (s *S3Storage) Upload(objectName string, reader io.Reader, size int64) error {
	_, err := s.client.PutObject(context.Background(), s.bucket, objectName, reader, size, minio.PutObjectOptions{})
	return err
}

func (s *S3Storage) Download(objectName string) (io.Reader, error) {
	reader, err := s.client.GetObject(context.Background(), s.bucket, objectName, minio.GetObjectOptions{})
	if err != nil {
		return nil, fmt.Errorf("failed to download file: %w", err)
	}
	return reader, nil
}

func (s *S3Storage) Delete(objectName string) error {
	return s.client.RemoveObject(context.Background(), s.bucket, objectName, minio.RemoveObjectOptions{})
}
