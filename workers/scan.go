package workers

import (
    "context"

    "github.com/sirupsen/logrus"
)

type ScanTaskV2 struct {
    AbsFilename string
    Result      chan error
}

func ScanWorkerV2(ctx context.Context, scanQueue chan ScanTaskV2) {
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