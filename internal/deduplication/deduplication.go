package deduplication

import (
	"bufio"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"os"

	"github.com/go-redis/redis/v8"
	"github.com/sirupsen/logrus"
)

func ComputeSHA256(ctx context.Context, filePath string, log *logrus.Logger) (string, error) {
	if filePath == "" {
		return "", fmt.Errorf("computeSHA256: filePath cannot be empty")
	}

	file, err := os.Open(filePath)
	if err != nil {
		log.Errorf("Failed to open file for checksum: %v", err)
		return "", fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	hasher := sha256.New()
	reader := bufio.NewReader(file)

	buffer := make([]byte, 4096)
	for {
		select {
		case <-ctx.Done():
			return "", fmt.Errorf("operation cancelled")
		default:
			n, err := reader.Read(buffer)
			if n > 0 {
				if _, wErr := hasher.Write(buffer[:n]); wErr != nil {
					return "", fmt.Errorf("hasher write error: %w", wErr)
				}
			}
			if err != nil {
				if err == io.EOF {
					sum := hex.EncodeToString(hasher.Sum(nil))
					log.Debugf("Checksum computed: %s", sum)
					return sum, nil
				}
				return "", fmt.Errorf("read error: %w", err)
			}
		}
	}
}

func HandleDeduplication(ctx context.Context, absFilename string, redisClient *redis.Client, log *logrus.Logger) error {
	checksum, err := ComputeSHA256(ctx, absFilename, log)
	if err != nil {
		log.Errorf("Failed to compute SHA256 for %s: %v", absFilename, err)
		return fmt.Errorf("checksum computation failed: %w", err)
	}

	existingPath, err := redisClient.Get(ctx, checksum).Result()
	if err != nil {
		if err == redis.Nil {
			err = redisClient.Set(ctx, checksum, absFilename, 0).Err()
			if err != nil {
				log.Errorf("Redis error setting checksum %s: %v", checksum, err)
				return fmt.Errorf("redis error: %w", err)
			}
			log.Infof("Stored new checksum %s for file %s", checksum, absFilename)
			return nil
		}
		log.Errorf("Redis error fetching checksum %s: %v", checksum, err)
		return fmt.Errorf("redis error: %w", err)
	}

	if existingPath != absFilename {
		if _, err := os.Stat(existingPath); os.IsNotExist(err) {
			log.Errorf("Existing file for checksum %s not found at %s", checksum, existingPath)
			return fmt.Errorf("existing file not found: %w", err)
		}

		err = os.Link(existingPath, absFilename)
		if err != nil {
			log.Errorf("Failed linking %s to %s: %v", existingPath, absFilename, err)
			return fmt.Errorf("failed link: %w", err)
		}
		log.Infof("Created hard link for duplicate file %s -> %s", absFilename, existingPath)
	} else {
		log.Infof("File %s already exists with same checksum", absFilename)
	}

	return nil
}
