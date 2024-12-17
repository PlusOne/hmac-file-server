
package logging

import (
	"io"
	"os"

	"github.com/sirupsen/logrus"
)

var Log = logrus.New()

func SetupLogging(level string, logFile string) {
	parsedLevel, err := logrus.ParseLevel(level)
	if err != nil {
		Log.Fatalf("Invalid log level: %s", level)
	}
	Log.SetLevel(parsedLevel)

	if logFile != "" {
		file, err := os.OpenFile(logFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			Log.Fatalf("Failed to open log file: %v", err)
		}
		Log.SetOutput(io.MultiWriter(os.Stdout, file))
	} else {
		Log.SetOutput(os.Stdout)
	}

	Log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})
}

func Fatalf(format string, args ...interface{}) {
	Log.Fatalf(format, args...)
}

func Infof(format string, args ...interface{}) {
	Log.Infof(format, args...)
}

// ...other logging utility functions...