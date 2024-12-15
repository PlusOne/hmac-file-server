package config

import (
	"fmt"

	"github.com/spf13/viper"
)

// Configuration structures...

type Config struct {
	Server     ServerConfig     `mapstructure:"server"`
	Timeouts   TimeoutConfig    `mapstructure:"timeouts"`
	Security   SecurityConfig   `mapstructure:"security"`
	Versioning VersioningConfig `mapstructure:"versioning"`
	Uploads    UploadsConfig    `mapstructure:"uploads"`
	ClamAV     ClamAVConfig     `mapstructure:"clamav"`
	Redis      RedisConfig      `mapstructure:"redis"`
	Workers    WorkersConfig    `mapstructure:"workers"`
}

var Conf Config

func LoadConfig(configFile string) error {
	viper.SetConfigFile(configFile)
	viper.SetConfigType("toml")
	viper.AutomaticEnv()
	viper.SetEnvPrefix("HMAC")

	// Set defaults
	setDefaults()

	if err := viper.ReadInConfig(); err != nil {
		return fmt.Errorf("error reading config file: %w", err)
	}

	if err := viper.Unmarshal(&Conf); err != nil {
		return fmt.Errorf("unable to decode into struct: %w", err)
	}

	// Validate configuration
	if err := validateConfig(&Conf); err != nil {
		return fmt.Errorf("configuration validation failed: %w", err)
	}

	return nil
}

func setDefaults() {
	// Set default values as in main.go
	// ...
}

func validateConfig(conf *Config) error {
	// Validation logic as in main.go
	// ...
	return nil
}
