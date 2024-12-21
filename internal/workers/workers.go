package workers

import (
	"github.com/go-redis/redis/v8"
	"github.com/renz/hmac-file-server/internal/config"
)

type UploadWorker struct {
	ID int
}

func (uw *UploadWorker) Start() {
	// Implement the upload worker logic
}

type ScanWorker struct {
	ID int
}

func (sw *ScanWorker) Start() {
	// Implement the scan worker logic
}

type WorkerPool struct {
	UploadWorkers []UploadWorker
	ScanWorkers   []ScanWorker
	// ...other fields...
}

func NewWorkerPool(conf *config.Config, redisClient *redis.Client) *WorkerPool {
	pool := &WorkerPool{
		UploadWorkers: make([]UploadWorker, conf.Workers.NumWorkers),
		ScanWorkers:   make([]ScanWorker, conf.ClamAV.NumScanWorkers),
	}
	// Initialize workers
	return pool
}

func (wp *WorkerPool) Start() {
	for i := range wp.UploadWorkers {
		wp.UploadWorkers[i] = UploadWorker{ID: i}
		go wp.UploadWorkers[i].Start()
	}

	for i := range wp.ScanWorkers {
		wp.ScanWorkers[i] = ScanWorker{ID: i}
		go wp.ScanWorkers[i].Start()
	}
}
