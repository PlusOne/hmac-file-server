package config

import (
	"fmt"
	"time"
)

func ValidateConfig(conf *Config) error {
	if conf.Server.ListenPort == "" {
		return fmt.Errorf("ListenPort must be set")
	}
	if conf.Security.Secret == "" {
		return fmt.Errorf("secret must be set")
	}
	if conf.Server.StoragePath == "" {
		return fmt.Errorf("StoragePath must be set")
	}
	if conf.Server.FileTTL == "" {
		return fmt.Errorf("FileTTL must be set")
	}

	if _, err := time.ParseDuration(conf.Timeouts.ReadTimeout); err != nil {
		return fmt.Errorf("invalid ReadTimeout: %v", err)
	}
	if _, err := time.ParseDuration(conf.Timeouts.WriteTimeout); err != nil {
		return fmt.Errorf("invalid WriteTimeout: %v", err)
	}
	if _, err := time.ParseDuration(conf.Timeouts.IdleTimeout); err != nil {
		return fmt.Errorf("invalid IdleTimeout: %v", err)
	}

	if conf.Redis.RedisEnabled {
		if conf.Redis.RedisAddr == "" {
			return fmt.Errorf("RedisAddr must be set when Redis is enabled")
		}
	}

	if conf.ISO.Enabled {
		if conf.ISO.Size == "" {
			return fmt.Errorf("ISO size must be set")
		}
		if conf.ISO.MountPoint == "" {
			return fmt.Errorf("ISO mount point must be set")
		}
		if conf.ISO.Charset == "" {
			return fmt.Errorf("ISO charset must be set")
		}
	}

	return nil
}
