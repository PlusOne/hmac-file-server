package workers

import (
	"context"
	"hmac-file-server/config"
	"hmac-file-server/handlers"
)

var (
	UploadQueue chan handlers.UploadTask
	ScanQueue   chan handlers.ScanTask
)

func InitializeWorkers(ctx context.Context) {
	UploadQueue = make(chan handlers.UploadTask, config.Conf.Workers.UploadQueueSize)
	ScanQueue = make(chan handlers.ScanTask, config.Conf.Workers.UploadQueueSize)

	for i := 0; i < config.Conf.Workers.NumWorkers; i++ {
		go uploadWorker(ctx, i)
	}

	if config.Conf.ClamAV.ClamAVEnabled {
		for i := 0; i < config.Conf.ClamAV.NumScanWorkers; i++ {
			go scanWorker(ctx, i)
		}
	}
}

func uploadWorker(ctx context.Context, id int) {
	// Implementation from main.go's uploadWorker
}

func scanWorker(ctx context.Context, id int) {
	// Implementation from main.go's scanWorker
}
