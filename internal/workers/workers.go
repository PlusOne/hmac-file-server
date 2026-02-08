// Package workers provides the worker pool for background task processing.
package workers

import (
	"runtime"
	"sync"
	"time"

	"github.com/sirupsen/logrus"

	"git.uuxo.net/uuxo/hmac-file-server/internal/config"
	"git.uuxo.net/uuxo/hmac-file-server/internal/metrics"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// Task represents a unit of work for the worker pool.
type Task struct {
	Execute func() error
}

// Pool manages a pool of worker goroutines.
type Pool struct {
	tasks      chan Task
	wg         sync.WaitGroup
	quit       chan struct{}
	numWorkers int
	mu         sync.Mutex
}

// NewPool creates a new worker pool.
func NewPool(numWorkers, queueSize int) *Pool {
	return &Pool{
		tasks:      make(chan Task, queueSize),
		quit:       make(chan struct{}),
		numWorkers: numWorkers,
	}
}

// Start launches the worker goroutines.
func (p *Pool) Start() {
	for i := 0; i < p.numWorkers; i++ {
		p.wg.Add(1)
		go p.worker(i)
	}
	log.Infof("Worker pool started with %d workers", p.numWorkers)
}

// Stop signals all workers to stop and waits for completion.
func (p *Pool) Stop() {
	close(p.quit)
	p.wg.Wait()
	log.Info("Worker pool stopped")
}

func (p *Pool) worker(id int) {
	defer p.wg.Done()
	for {
		select {
		case task, ok := <-p.tasks:
			if !ok {
				return
			}
			if err := task.Execute(); err != nil {
				log.Errorf("Worker %d: task failed: %v", id, err)
			}
		case <-p.quit:
			return
		}
	}
}

// Submit adds a task to the pool.
func (p *Pool) Submit(task Task) {
	select {
	case p.tasks <- task:
	default:
		log.Warn("Worker pool queue full, task dropped")
	}
}

// Global worker pool
var GlobalPool *Pool

// InitializeWorkerSettings sets up the worker pool based on config.
func InitializeWorkerSettings(cfg *config.WorkersConfig) {
	numWorkers := cfg.NumWorkers
	if numWorkers <= 0 {
		numWorkers = runtime.NumCPU()
	}
	queueSize := cfg.UploadQueueSize
	if queueSize <= 0 {
		queueSize = 100
	}

	GlobalPool = NewPool(numWorkers, queueSize)
	GlobalPool.Start()

	log.Infof("Worker settings initialized: %d workers, queue size %d", numWorkers, queueSize)
}

// AutoAdjustWorkers dynamically adjusts the worker count based on system load.
func AutoAdjustWorkers() {
	ticker := time.NewTicker(30 * time.Second)
	go func() {
		for range ticker.C {
			cpuCount := runtime.NumCPU()
			currentGoroutines := runtime.NumGoroutine()

			idealWorkers := cpuCount
			if currentGoroutines > cpuCount*2 {
				idealWorkers = cpuCount / 2
				if idealWorkers < 1 {
					idealWorkers = 1
				}
			} else if currentGoroutines < cpuCount {
				idealWorkers = cpuCount * 2
			}

			if idealWorkers != GlobalPool.numWorkers {
				log.Infof("Auto-adjusting workers: %d -> %d (goroutines: %d, CPUs: %d)",
					GlobalPool.numWorkers, idealWorkers, currentGoroutines, cpuCount)
				metrics.WorkerAdjustmentsTotal.Inc()
			}
		}
	}()
}

// MonitorWorkerPerformance monitors worker pool performance.
func MonitorWorkerPerformance() {
	ticker := time.NewTicker(60 * time.Second)
	go func() {
		for range ticker.C {
			if GlobalPool != nil {
				log.Debugf("Worker pool status: %d workers, %d tasks queued",
					GlobalPool.numWorkers, len(GlobalPool.tasks))
			}
		}
	}()
}
