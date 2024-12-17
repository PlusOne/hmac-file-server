
package workers

import (
	"context"
	"pkg/logging"
	"pkg/metrics"
)

func InitializeWorkerSettings(server *config.ServerConfig, workers *config.WorkersConfig, clamav *config.ClamAVConfig) {
	if server.AutoAdjustWorkers {
		numWorkers, queueSize := autoAdjustWorkers()
		workers.NumWorkers = numWorkers
		workers.UploadQueueSize = queueSize
		clamav.NumScanWorkers = max(numWorkers/2, 1)

		logging.Infof("AutoAdjustWorkers enabled: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
			workers.NumWorkers, workers.UploadQueueSize, clamav.NumScanWorkers)
	} else {
		logging.Infof("Manual configuration in effect: NumWorkers=%d, UploadQueueSize=%d, NumScanWorkers=%d",
			workers.NumWorkers, workers.UploadQueueSize, clamav.NumScanWorkers)
	}
}

func InitializeUploadWorkerPool(ctx context.Context, w *config.WorkersConfig) {
	for i := 0; i < w.NumWorkers; i++ {
		go uploadWorker(ctx, i)
		logging.Infof("Upload worker %d started.", i)
	}
	logging.Infof("Initialized %d upload workers", w.NumWorkers)
}

func InitializeScanWorkerPool(ctx context.Context, clamClient *Clamd.Client) {
	for i := 0; i < clamav.NumScanWorkers; i++ {
		go scanWorker(ctx, i, clamClient)
	}
	logging.Infof("Initialized %d scan workers", clamav.NumScanWorkers)
}

func uploadWorker(ctx context.Context, workerID int) {
	logging.Infof("Upload worker %d started.", workerID)
	defer logging.Infof("Upload worker %d stopped.", workerID)
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
				logging.Errorf("Upload worker %d error: %v", workerID, err)
				metrics.IncUploadErrors()
			} else {
				metrics.IncUploadsTotal()
			}
		}
	}
}

func scanWorker(ctx context.Context, workerID int, clamClient *Clamd.Client) {
	logging.Infof("Scan worker %d started.", workerID)
	defer logging.Infof("Scan worker %d stopped.", workerID)
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
				logging.Errorf("Scan worker %d error: %v", workerID, err)
			}
		}
	}
}

// ...additional worker-related functions...