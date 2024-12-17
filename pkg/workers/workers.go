package workers

import (
	"context"
	"sync"

	"github.com/renz/hmac-file-server/pkg/config"
	"github.com/sirupsen/logrus"
)

func InitializeWorkerSettings(server *config.ServerConfig, workersCfg *config.WorkersConfig, clamavCfg *config.ClamAVConfig) {
	if server.AutoAdjustWorkers {
		numWorkers, queueSize := autoAdjustWorkers()
		workersCfg.NumWorkers = numWorkers
		workersCfg.UploadQueueSize = queueSize
		clamavCfg.NumScanWorkers = max(numWorkers/2, 1)

		logrus.Infof("AutoAdjustWorkers enabled: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
			workersCfg.NumWorkers, workersCfg.UploadQueueSize, clamavCfg.NumScanWorkers)
	} else {
		logrus.Infof("Manual configuration in effect: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
			workersCfg.NumWorkers, workersCfg.UploadQueueSize, clamavCfg.NumScanWorkers)
	}
}

func InitializeUploadWorkerPool(ctx context.Context, w *config.WorkersConfig) {
	for i := 0; i < w.NumWorkers; i++ {
		go uploadWorker(ctx, i)
		logrus.Infof("Upload worker %d started.", i)
	}
	logrus.Infof("Initialized %d upload workers", w.NumWorkers)
}

func InitializeScanWorkerPool(ctx context.Context, numScanWorkers int) {
	for i := 0; i < numScanWorkers; i++ {
		go scanWorker(ctx, i)
	}
	logrus.Infof("Initialized %d scan workers", numScanWorkers)
}

func uploadWorker(ctx context.Context, workerID int) {
	logrus.Infof("Upload worker %d started.", workerID)
	defer logrus.Infof("Upload worker %d stopped.", workerID)
	for {
		select {
		case <-ctx.Done():
			return
		case task, ok := <-uploadQueue:
			if !ok {
				return
			}
			// Process upload task
			err := processUpload(task)
			if err != nil {
				logrus.Errorf("Upload worker %d error: %v", workerID, err)
				metrics.IncUploadErrors()
			} else {
				metrics.IncUploadsTotal()
			}
		}
	}
}

func scanWorker(ctx context.Context, workerID int, clamClient *Clamd.Client) {
	logrus.Infof("Scan worker %d started.", workerID)
	defer logrus.Infof("Scan worker %d stopped.", workerID)
	for {
		select {
		case <-ctx.Done():
			return
		case task, ok := <-scanQueue:
			if !ok {
				return
			}
			// Process scan task
			err := scanFile(task.AbsFilename, clamClient)
			if err != nil {
				logrus.Errorf("Scan worker %d error: %v", workerID, err)
			}
		}
	}
}

// ...additional worker-related functions...
