// Package scanning handles ClamAV virus scanning integration.
package scanning

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"github.com/sirupsen/logrus"

	"git.uuxo.net/uuxo/hmac-file-server/internal/config"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// Global ClamAV client
var ClamClient *clamd.Clamd

// InitClamAV initializes the ClamAV connection.
func InitClamAV(cfg *config.ClamAVConfig) error {
	if !cfg.ClamAVEnabled {
		log.Info("ClamAV scanning disabled")
		return nil
	}

	ClamClient = clamd.NewClamd(cfg.ClamAVSocket)

	// Test connection
	err := ClamClient.Ping()
	if err != nil {
		return fmt.Errorf("ClamAV connection failed: %w", err)
	}

	log.Infof("ClamAV initialized: %s", cfg.ClamAVSocket)
	return nil
}

// ScanFileWithClamAV scans a file with ClamAV and returns true if clean.
func ScanFileWithClamAV(filename string) (bool, error) {
	if ClamClient == nil {
		return true, nil
	}

	response, err := ClamClient.ScanFile(filename)
	if err != nil {
		return false, fmt.Errorf("ClamAV scan error: %w", err)
	}

	for s := range response {
		if s.Status == clamd.RES_FOUND {
			log.Warnf("ClamAV: Virus found in %s: %s", filename, s.Description)
			return false, fmt.Errorf("virus found: %s", s.Description)
		}
		if s.Status == clamd.RES_ERROR {
			log.Errorf("ClamAV scan error for %s: %s", filename, s.Description)
			return false, fmt.Errorf("scan error: %s", s.Description)
		}
	}

	log.Debugf("ClamAV: File %s is clean", filename)
	return true, nil
}

// ProcessScan processes a scan task.
func ProcessScan(absFilename string, clamAddress string) error {
	log.Infof("Starting ClamAV scan for file: %s", absFilename)
	clam := clamd.NewClamd(clamAddress)

	response, err := clam.ScanFile(absFilename)
	if err != nil {
		log.WithFields(logrus.Fields{"file": absFilename, "error": err}).Error("ClamAV scan failed")
		return err
	}

	for s := range response {
		if s.Status == clamd.RES_FOUND {
			log.Warnf("ClamAV found threat in %s: %s", absFilename, s.Description)
			return fmt.Errorf("virus found: %s", s.Description)
		}
	}

	log.Infof("Finished processing scan for file: %s", absFilename)
	return nil
}

// Global Redis client
var RedisClient *redis.Client
var RedisConnected bool

// InitRedis initializes the Redis connection.
func InitRedis(cfg *config.RedisConfig) error {
	if !cfg.RedisEnabled {
		log.Info("Redis disabled")
		return nil
	}

	RedisClient = redis.NewClient(&redis.Options{
		Addr:     cfg.RedisAddr,
		Password: cfg.RedisPassword,
		DB:       cfg.RedisDBIndex,
	})

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	_, err := RedisClient.Ping(ctx).Result()
	if err != nil {
		if strings.Contains(err.Error(), "connection refused") {
			log.Warnf("Redis connection failed (non-critical): %v", err)
			RedisConnected = false
			return nil
		}
		return fmt.Errorf("Redis connection failed: %w", err)
	}

	RedisConnected = true
	log.Infof("Redis connected: %s", cfg.RedisAddr)
	return nil
}
