package workers

import (
    "context"

    "github.com/sirupsen/logrus"
)

type ScanTask struct {
    // Define the fields for your scan task
}

func ScanWorker(ctx context.Context, scanQueue chan ScanTask) {
    for {
        select {
        case <-ctx.Done():
            logrus.Info("ScanWorker shutting down...")
            return
        case task := <-scanQueue:
            // Process the scan task
            logrus.Infof("Processing scan task: %+v", task)
            // Add your scan processing logic here
        }
    }
}