package main

import (
    "context"
    "sync"
)

func initializeUploadWorkerPool(ctx context.Context, w *WorkersConfig) {
    for i := 0; i < w.NumWorkers; i++ {
        go uploadWorker(ctx, i)
        log.Infof("Upload worker %d started.", i)
    }
    log.Infof("Initialized %d upload workers", w.NumWorkers)
}

func uploadWorker(ctx context.Context, workerID int) {
    log.Infof("Upload worker %d started.", workerID)
    defer log.Infof("Upload worker %d stopped.", workerID)
    for {
        select {
        case <-ctx.Done():
            return
        case task, ok := <-uploadQueue:
            if !ok {
                return
            }
            log.Infof("Worker %d processing file: %s", workerID, task.AbsFilename)
            err := processUpload(task)
            if err != nil {
                log.Errorf("Worker %d failed to process file %s: %v", workerID, task.AbsFilename, err)
            } else {
                log.Infof("Worker %d successfully processed file: %s", workerID, task.AbsFilename)
            }
            task.Result <- err
        }
    }
}

func initializeScanWorkerPool(ctx context.Context) {
    for i := 0; i < conf.ClamAV.NumScanWorkers; i++ {
        go scanWorker(ctx, i)
    }
    log.Infof("Initialized %d scan workers", conf.ClamAV.NumScanWorkers)
}

func scanWorker(ctx context.Context, workerID int) {
    log.WithField("worker_id", workerID).Info("Scan worker started")
    for {
        select {
        case <-ctx.Done():
            log.WithField("worker_id", workerID).Info("Scan worker stopping")
            return
        case task, ok := <-scanQueue:
            if !ok {
                log.WithField("worker_id", workerID).Info("Scan queue closed")
                return
            }
            log.WithFields(logrus.Fields{"worker_id": workerID, "file": task.AbsFilename}).Info("Processing scan task")
            err := scanFileWithClamAV(task.AbsFilename)
            if err != nil {
                log.WithFields(logrus.Fields{"worker_id": workerID, "file": task.AbsFilename, "error": err}).Error("Failed to scan file")
            } else {
                log.WithFields(logrus.Fields{"worker_id": workerID, "file": task.AbsFilename}).Info("Successfully scanned file")
            }
            task.Result <- err
            close(task.Result)
        }
    }
}
