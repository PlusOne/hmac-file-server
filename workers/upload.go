package workers

import (
    "context"

    "github.com/sirupsen/logrus"
)

type FileUploadTask struct {
    FilePath string
    UserID   int
    // Add other relevant fields
}

func FileUploadWorker(ctx context.Context, uploadQueue chan FileUploadTask) {
    for {
        select {
        case <-ctx.Done():
            logrus.Info("UploadWorker shutting down...")
            return
        case task := <-uploadQueue:
            // Process the upload task
            logrus.Infof("Processing upload task: %+v", task)
            // Add your upload processing logic here
        }
    }
}