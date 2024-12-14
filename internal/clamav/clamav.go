package clamav

import (
	"fmt"

	"github.com/dutchcoders/go-clamd"
	"github.com/sirupsen/logrus"
)

func InitClamAV(socket string, log *logrus.Logger) (*clamd.Clamd, error) {
	if socket == "" {
		log.Error("ClamAV socket path not configured.")
		return nil, fmt.Errorf("ClamAV socket path not configured")
	}

	clamClient := clamd.NewClamd("unix:" + socket)
	err := clamClient.Ping()
	if err != nil {
		log.Errorf("Failed to connect to ClamAV at %s: %v", socket, err)
		return nil, fmt.Errorf("failed to connect to ClamAV: %w", err)
	}

	log.Info("Connected to ClamAV successfully.")
	return clamClient, nil
}

func ScanFileWithClamAV(clamClient *clamd.Clamd, filePath string, log *logrus.Logger) error {
	log.WithField("file", filePath).Info("Scanning file with ClamAV")

	scanResultChan, err := clamClient.ScanFile(filePath)
	if err != nil {
		log.WithError(err).Error("Failed to initiate ClamAV scan")
		return fmt.Errorf("failed to initiate ClamAV scan: %w", err)
	}

	scanResult := <-scanResultChan
	if scanResult == nil {
		log.Error("Failed to receive scan result from ClamAV")
		return fmt.Errorf("failed to receive scan result")
	}

	switch scanResult.Status {
	case clamd.RES_OK:
		log.WithField("file", filePath).Info("ClamAV scan passed")
		return nil
	case clamd.RES_FOUND:
		log.WithFields(logrus.Fields{"file": filePath, "description": scanResult.Description}).Warn("ClamAV detected a virus")
		return fmt.Errorf("virus detected: %s", scanResult.Description)
	default:
		log.WithFields(logrus.Fields{"file": filePath, "status": scanResult.Status, "description": scanResult.Description}).Warn("ClamAV scan returned unexpected status")
		return fmt.Errorf("unexpected ClamAV status: %s", scanResult.Description)
	}
}
