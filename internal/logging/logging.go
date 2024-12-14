package logging

import (
	"io"
	"os"

	"github.com/sirupsen/logrus"
)

func SetupLogging(log *logrus.Logger, levelStr, logFile string) {
	level, err := logrus.ParseLevel(levelStr)
	if err != nil {
		log.Fatalf("Invalid log level: %s", levelStr)
	}
	log.SetLevel(level)

	if logFile != "" {
		f, err := os.OpenFile(logFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			log.Fatalf("Failed to open log file: %v", err)
		}
		log.SetOutput(io.MultiWriter(os.Stdout, f))
	} else {
		log.SetOutput(os.Stdout)
	}

	log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})
}
