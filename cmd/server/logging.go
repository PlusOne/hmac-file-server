package main

import (
    "github.com/sirupsen/logrus"
    "os"
    "io"
)

func setupLogging() {
    log.SetFormatter(&logrus.TextFormatter{
        FullTimestamp: true,
    })

    if conf.Server.LogFile != "" {
        logFile, err := os.OpenFile(conf.Server.LogFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
        if err != nil {
            log.Fatalf("Failed to open log file: %v", err)
        }
        log.SetOutput(io.MultiWriter(os.Stdout, logFile))
    } else {
        log.SetOutput(os.Stdout)
    }

    level, err := logrus.ParseLevel(conf.Server.LogLevel)
    if err != nil {
        log.Warnf("Invalid log level: %s, defaulting to info", conf.Server.LogLevel)
        level = logrus.InfoLevel
    }
    log.SetLevel(level)
    logSystemInfo()
}

func logSystemInfo() {
    log.Info("========================================")
    log.Infof("       HMAC File Server - %s          ", versionString)
    log.Info("  Secure File Handling with HMAC Auth   ")
    log.Info("========================================")
    log.Infof("Log Level: %s", conf.Server.LogLevel)
    log.Infof("Storage Path: %s", conf.Server.StoragePath)
}
