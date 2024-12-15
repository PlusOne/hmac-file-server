package handlers

import (
    "io"
    "net/http"
    "os"
    "path/filepath"
    "time"

    "github.com/sirupsen/logrus"
    "github.com/PlusOne/hmac-file-server/config"
    "github.com/PlusOne/hmac-file-server/workers"
    "github.com/shirou/gopsutil/cpu"
    "github.com/dutchcoders/go-clamd"
)

func corsMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        w.Header().Set("Access-Control-Allow-Origin", "*")
        w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
        w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
        if r.Method == "OPTIONS" {
            w.WriteHeader(http.StatusOK)
            return
        }
        next.ServeHTTP(w, r)
    })
}

func UploadHandlerV2(w http.ResponseWriter, r *http.Request) {
    // Load configuration
    conf, err := config.LoadConfig("./config.toml")
    if err != nil {
        logrus.Fatalf("Error loading configuration: %v", err)
    }

    // Check configuration values
    if conf.Server.ListenPort == "8080" {
        logrus.Info("ListenPort is set to 8080.")
    } else {
        logrus.Infof("ListenPort is set to %s.", conf.Server.ListenPort)
    }

    // Gather CPU information
    cpuInfo, err := cpu.Info()
    if err != nil {
        logrus.Fatalf("Error gathering CPU information: %v", err)
    }

    // Log CPU information in a cumulative manner
    for _, info := range cpuInfo {
        logrus.Infof("CPU Model: %s, Cores: %d, Mhz: %f", info.ModelName, info.Cores, info.Mhz)
    }

    // Start a goroutine to log system metrics every 10 seconds if LogLimiter is enabled
    if conf.Server.LogLimiter {
        go func() {
            ticker := time.NewTicker(10 * time.Second)
            defer ticker.Stop()
            for {
                select {
                case <-ticker.C:
                    logrus.Info("Updating system metrics...")
                    // Add your system metrics update logic here
                }
            }
        }()
    }

    // Implement the upload handler logic here
    file, header, err := r.FormFile("file")
    if err != nil {
        http.Error(w, "Failed to get file from request", http.StatusBadRequest)
        return
    }
    defer file.Close()

    filePath := filepath.Join(conf.Server.StoragePath, header.Filename)
    outFile, err := os.Create(filePath)
    if err != nil {
        http.Error(w, "Failed to create file", http.StatusInternalServerError)
        return
    }
    defer outFile.Close()

    _, err = io.Copy(outFile, file)
    if err != nil {
        http.Error(w, "Failed to save file", http.StatusInternalServerError)
        return
    }

    // Check if ClamAV is enabled and scan the file
    if conf.ClamAV.ClamAVEnabled {
        clamClient := clamd.NewClamd("unix:" + conf.ClamAV.ClamAVSocket)
        response, err := clamClient.ScanFile(filePath)
        if err != nil {
            logrus.Errorf("Error scanning file with ClamAV: %v", err)
            http.Error(w, "Failed to scan file", http.StatusInternalServerError)
            return
        }

        for result := range response {
            if result.Status == clamd.RES_FOUND {
                logrus.Warnf("ClamAV found a virus in the file: %s", result.Description)
                http.Error(w, "File contains a virus", http.StatusForbidden)
                return
            }
        }
    }

    w.WriteHeader(http.StatusCreated)
    w.Write([]byte("Upload successful"))
}

func InitHandlers(uploadQueue chan workers.UploadTask, scanQueue chan workers.ScanTask, conf *config.Config) {
    mux := http.NewServeMux()
    mux.HandleFunc("/upload", UploadHandlerV2)
    handler := corsMiddleware(mux)
    http.ListenAndServe(":"+conf.Server.ListenPort, handler)
}

type Config struct {
    Server struct {
        ListenPort           string
        StoragePath          string
        DeduplicationEnabled bool
        MinFreeBytes         string
        LogLimiter           bool
    }
    Uploads struct {
        ChunkedUploadsEnabled bool
        ChunkSize             string
        AllowedExtensions     []string
    }
    Redis struct {
        RedisEnabled             bool
        RedisDBIndex             int
        RedisAddr                string
        RedisPassword            string
        RedisHealthCheckInterval string
    }
    File struct {
        FileRevision int
    }
    ClamAV struct {
        ClamAVEnabled bool
        ClamAVSocket  string
    }
}

// Example function where you might want to check the ListenPort value
