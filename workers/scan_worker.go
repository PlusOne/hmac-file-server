package workers

import (
    "context"
    "fmt"
    "os/exec"
    "strings"

    "github.com/sirupsen/logrus"
)

// ClamAVClient repräsentiert einen Client zur Interaktion mit ClamAV
type ClamAVClient struct {
    socket string
    // Weitere Felder wie Verbindung, etc.
}

// ScanFile scannt die angegebene Datei mit ClamAV
func (c *ClamAVClient) ScanFile(filePath string) error {
    // Beispielhafte Implementierung. Ersetze dies mit echter ClamAV-Interaktion.
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

// ScanWorker verarbeitet ScanTasks aus der scanQueue
func ScanWorker(ctx context.Context, id int, client *ClamAVClient, scanQueue <-chan ScanTask) {
    logrus.Infof("Scan Worker %d started", id)
    for {
        select {
        case <-ctx.Done():
            logrus.Infof("Scan Worker %d shutting down", id)
            return
        case task := <-scanQueue:
            logrus.Infof("Scan Worker %d scanning file: %s", id, task.FileName)
            err := client.ScanFile(task.FileName)
            if err != nil {
                logrus.Errorf("Scan Worker %d: Error scanning file %s: %v", id, task.FileName, err)
                // Optional: Handle infizierte Dateien (z.B. Quarantäne, Benachrichtigung)
            } else {
                logrus.Infof("Scan Worker %d: File %s is clean", id, task.FileName)
            }
        }
    }
}

// InitializeScanWorkerPool initialisiert den Scan-Worker-Pool
func InitializeScanWorkerPool(ctx context.Context, client *ClamAVClient, scanQueue <-chan ScanTask, poolSize int) {
    for i := 0; i < poolSize; i++ {
        go ScanWorker(ctx, i+1, client, scanQueue)
    }
    logrus.Infof("%d Scan Workers initialized", poolSize)
}
