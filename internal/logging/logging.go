// Package logging handles log setup including rotation and system info.
package logging

import (
	"fmt"
	"os"
	"runtime"

	"github.com/sirupsen/logrus"
	"gopkg.in/natefinch/lumberjack.v2"

	"git.uuxo.net/uuxo/hmac-file-server/internal/config"
)

// SetupLogging configures the global logger based on config.
func SetupLogging(cfg *config.Config, log *logrus.Logger) {
	switch cfg.Logging.Level {
	case "debug":
		log.SetLevel(logrus.DebugLevel)
	case "info":
		log.SetLevel(logrus.InfoLevel)
	case "warn":
		log.SetLevel(logrus.WarnLevel)
	case "error":
		log.SetLevel(logrus.ErrorLevel)
	default:
		log.SetLevel(logrus.InfoLevel)
	}

	if cfg.Logging.File != "" {
		maxSize := cfg.Logging.MaxSize
		if maxSize == 0 {
			maxSize = 100
		}
		maxBackups := cfg.Logging.MaxBackups
		if maxBackups == 0 {
			maxBackups = 3
		}
		maxAge := cfg.Logging.MaxAge
		if maxAge == 0 {
			maxAge = 28
		}

		lumberjackLogger := &lumberjack.Logger{
			Filename:   cfg.Logging.File,
			MaxSize:    maxSize,
			MaxBackups: maxBackups,
			MaxAge:     maxAge,
			Compress:   cfg.Logging.Compress,
		}
		log.SetOutput(lumberjackLogger)
	} else {
		log.SetOutput(os.Stdout)
	}

	log.Infof("Logging initialized at level: %s", cfg.Logging.Level)
}

// LogSystemInfo logs system information at startup.
func LogSystemInfo(log *logrus.Logger, version string) {
	hostname, _ := os.Hostname()
	log.Infof("=== System Information ===")
	log.Infof("Hostname: %s", hostname)
	log.Infof("OS: %s", runtime.GOOS)
	log.Infof("Architecture: %s", runtime.GOARCH)
	log.Infof("Go version: %s", runtime.Version())
	log.Infof("CPUs available: %d", runtime.NumCPU())
	log.Infof("Max Goroutines (GOMAXPROCS): %d", runtime.GOMAXPROCS(0))
	log.Infof("Version: %s", version)
	log.Infof("PID: %d", os.Getpid())
	log.Infof("==========================")
}

// WritePIDFile writes the current process ID to the specified pid file.
func WritePIDFile(pidPath string, log *logrus.Logger) error {
	pid := os.Getpid()
	pidStr := fmt.Sprintf("%d", pid)
	err := os.WriteFile(pidPath, []byte(pidStr), 0644)
	if err != nil {
		log.Errorf("Failed to write PID file: %v", err)
		return err
	}
	log.Infof("PID %d written to %s", pid, pidPath)
	return nil
}

// RemovePIDFile removes the PID file.
func RemovePIDFile(pidPath string, log *logrus.Logger) {
	err := os.Remove(pidPath)
	if err != nil {
		log.Errorf("Failed to remove PID file: %v", err)
	} else {
		log.Infof("PID file %s removed successfully", pidPath)
	}
}
