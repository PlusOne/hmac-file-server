package workers

import (
	"context"
	"fmt"
	"os/exec"
	"strings"

	"github.com/PlusOne/hmac-file-server/config"
	"github.com/PlusOne/hmac-file-server/handlers"
	"github.com/sirupsen/logrus"
)

type WorkersConfig struct {
	NumWorkers      int
	UploadQueueSize int
	ScanQueueSize   int
}

type ClamAVClient struct {
	Socket string
}

func (c *ClamAVClient) ScanFile(filePath string) error {
	cmd := exec.Command("clamscan", filePath)
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("error executing clamscan: %v, output: %s", err, string(output))
	}

	if string(output) == "" {
		return fmt.Errorf("unexpected clamscan output")
	}

	if !strings.Contains(string(output), "OK") {
		return fmt.Errorf("file %s is infected: %s", filePath, string(output))
	}

	return nil
}

var (
	UploadQueue chan handlers.UploadTask
	ScanQueue   chan ScanTask
)

func InitializeWorkers(ctx context.Context) {
	UploadQueue = make(chan handlers.UploadTask, config.Conf.Workers.UploadQueueSize)
	ScanQueue = make(chan ScanTask, config.Conf.Workers.ScanQueueSize)

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

type UploadTask struct {
	// Define the fields for the upload task
}

func InitializeUploadWorkerPool(ctx context.Context, uploadQueue chan UploadTask, queueSize int) {
	// Initialize the upload worker pool
}

type ScanTask struct {
	FilePath string
}

func InitializeScanWorkerPool(ctx context.Context, clamClient *ClamAVClient, scanQueue chan ScanTask, workerCount int) {
	for i := 0; i < workerCount; i++ {
		go func() {
			for {
				select {
				case <-ctx.Done():
					return
				case task := <-scanQueue:
					err := clamClient.ScanFile(task.FilePath)
					if err != nil {
						logrus.Errorf("Error scanning file %s: %v", task.FilePath, err)
					} else {
						logrus.Infof("File %s scanned successfully", task.FilePath)
					}
				}
			}
		}()
	}
}
