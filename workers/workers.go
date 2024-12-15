package workers

import (
	"context"

	"github.com/PlusOne/hmac-file-server/config"
	"github.com/PlusOne/hmac-file-server/handlers"
)

type WorkersConfig struct {
	NumWorkers      int
	UploadQueueSize int
	ScanQueueSize   int
}

var (
	UploadQueue chan handlers.UploadTask
	ScanQueue   chan handlers.ScanTask
)

func InitializeWorkers(ctx context.Context) {
	UploadQueue = make(chan handlers.UploadTask, config.Conf.Workers.UploadQueueSize)
	ScanQueue = make(chan handlers.ScanTask, config.Conf.Workers.ScanQueueSize)

	for i := 0; i < config.Conf.Workers.NumWorkers; i++ {
		go uploadWorker(ctx, i)
	}

	if config.Conf.ClamAV.ClamAVEnabled {
		for i := 0; i < config.Conf.Workers.NumWorkers; i++ {
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
