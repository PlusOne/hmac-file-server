// config_test_scenarios.go
package main

import (
	"fmt"
	"os"
	"path/filepath"
)

// ConfigTestScenario represents a test scenario for configuration validation
type ConfigTestScenario struct {
	Name             string
	Config           Config
	ShouldPass       bool
	ExpectedErrors   []string
	ExpectedWarnings []string
}

// GetConfigTestScenarios returns a set of test scenarios for configuration validation
func GetConfigTestScenarios() []ConfigTestScenario {
	baseValidConfig := Config{
		Server: ServerConfig{
			ListenAddress:  "8080",
			BindIP:         "0.0.0.0",
			StoragePath:    "/tmp/test-storage",
			MetricsEnabled: true,
			MetricsPort:    "9090",
			FileTTLEnabled: true,
			FileTTL:        "24h",
			MinFreeBytes:   "1GB",
			FileNaming:     "HMAC",
			ForceProtocol:  "auto",
			PIDFilePath:    "/tmp/test.pid",
		},
		Security: SecurityConfig{
			Secret:    "test-secret-key-32-characters",
			EnableJWT: false,
		},
		Logging: LoggingConfig{
			Level:      "info",
			File:       "/tmp/test.log",
			MaxSize:    100,
			MaxBackups: 3,
			MaxAge:     30,
		},
		Timeouts: TimeoutConfig{
			Read:  "30s",
			Write: "30s",
			Idle:  "60s",
		},
		Workers: WorkersConfig{
			NumWorkers:      4,
			UploadQueueSize: 50,
		},
		Uploads: UploadsConfig{
			AllowedExtensions: []string{".txt", ".pdf", ".jpg"},
			ChunkSize:         "10MB",
		},
		Downloads: DownloadsConfig{
			AllowedExtensions: []string{".txt", ".pdf", ".jpg"},
			ChunkSize:         "10MB",
		},
	}

	return []ConfigTestScenario{
		{
			Name:       "Valid Basic Configuration",
			Config:     baseValidConfig,
			ShouldPass: true,
		},
		{
			Name: "Missing Listen Address",
			Config: func() Config {
				c := baseValidConfig
				c.Server.ListenAddress = ""
				return c
			}(),
			ShouldPass:     false,
			ExpectedErrors: []string{"server.listen_address is required"},
		},
		{
			Name: "Invalid Port Number",
			Config: func() Config {
				c := baseValidConfig
				c.Server.ListenAddress = "99999"
				return c
			}(),
			ShouldPass:     false,
			ExpectedErrors: []string{"invalid port number"},
		},
		{
			Name: "Invalid IP Address",
			Config: func() Config {
				c := baseValidConfig
				c.Server.BindIP = "999.999.999.999"
				return c
			}(),
			ShouldPass:     false,
			ExpectedErrors: []string{"invalid IP address format"},
		},
		{
			Name: "Same Port for Server and Metrics",
			Config: func() Config {
				c := baseValidConfig
				c.Server.ListenAddress = "8080"
				c.Server.MetricsPort = "8080"
				return c
			}(),
			ShouldPass:     false,
			ExpectedErrors: []string{"metrics port cannot be the same as main listen port"},
		},
		{
			Name: "JWT Enabled Without Secret",
			Config: func() Config {
				c := baseValidConfig
				c.Security.EnableJWT = true
				c.Security.JWTSecret = ""
				return c
			}(),
			ShouldPass:     false,
			ExpectedErrors: []string{"JWT secret is required when JWT is enabled"},
		},
		{
			Name: "Short JWT Secret",
			Config: func() Config {
				c := baseValidConfig
				c.Security.EnableJWT = true
				c.Security.JWTSecret = "short"
				c.Security.JWTAlgorithm = "HS256"
				return c
			}(),
			ShouldPass:       true,
			ExpectedWarnings: []string{"JWT secret should be at least 32 characters"},
		},
		{
			Name: "Invalid Log Level",
			Config: func() Config {
				c := baseValidConfig
				c.Logging.Level = "invalid"
				return c
			}(),
			ShouldPass:     false,
			ExpectedErrors: []string{"invalid log level"},
		},
		{
			Name: "Invalid Timeout Format",
			Config: func() Config {
				c := baseValidConfig
				c.Timeouts.Read = "invalid"
				return c
			}(),
			ShouldPass:     false,
			ExpectedErrors: []string{"invalid read timeout format"},
		},
		{
			Name: "Negative Worker Count",
			Config: func() Config {
				c := baseValidConfig
				c.Workers.NumWorkers = -1
				return c
			}(),
			ShouldPass:     false,
			ExpectedErrors: []string{"number of workers must be positive"},
		},
		{
			Name: "Extensions Without Dots",
			Config: func() Config {
				c := baseValidConfig
				c.Uploads.AllowedExtensions = []string{"txt", "pdf"}
				return c
			}(),
			ShouldPass:     false,
			ExpectedErrors: []string{"file extensions must start with a dot"},
		},
		{
			Name: "High Worker Count Warning",
			Config: func() Config {
				c := baseValidConfig
				c.Workers.NumWorkers = 100
				return c
			}(),
			ShouldPass:       true,
			ExpectedWarnings: []string{"very high worker count may impact performance"},
		},
		{
			Name: "Deduplication Without Directory",
			Config: func() Config {
				c := baseValidConfig
				c.Deduplication.Enabled = true
				c.Deduplication.Directory = ""
				return c
			}(),
			ShouldPass:     false,
			ExpectedErrors: []string{"deduplication directory is required"},
		},
	}
}

// RunConfigTests runs all configuration test scenarios
func RunConfigTests() {
	scenarios := GetConfigTestScenarios()
	passed := 0
	failed := 0

	fmt.Println("🧪 Running Configuration Test Scenarios")
	fmt.Println("=======================================")
	fmt.Println()

	for i, scenario := range scenarios {
		fmt.Printf("Test %d: %s\n", i+1, scenario.Name)

		// Create temporary directories for testing
		tempDir := filepath.Join(os.TempDir(), fmt.Sprintf("hmac-test-%d", i))
		os.MkdirAll(tempDir, 0755)
		defer os.RemoveAll(tempDir)

		// Update paths in config to use temp directory
		scenario.Config.Server.StoragePath = filepath.Join(tempDir, "storage")
		scenario.Config.Logging.File = filepath.Join(tempDir, "test.log")
		scenario.Config.Server.PIDFilePath = filepath.Join(tempDir, "test.pid")
		if scenario.Config.Deduplication.Enabled {
			scenario.Config.Deduplication.Directory = filepath.Join(tempDir, "dedup")
		}

		result := ValidateConfigComprehensive(&scenario.Config)

		// Check if test passed as expected
		testPassed := true
		if scenario.ShouldPass && result.HasErrors() {
			fmt.Printf("  ❌ Expected to pass but failed with errors:\n")
			for _, err := range result.Errors {
				fmt.Printf("     • %s\n", err.Message)
			}
			testPassed = false
		} else if !scenario.ShouldPass && !result.HasErrors() {
			fmt.Printf("  ❌ Expected to fail but passed\n")
			testPassed = false
		} else if !scenario.ShouldPass && result.HasErrors() {
			// Check if expected errors are present
			expectedFound := true
			for _, expectedError := range scenario.ExpectedErrors {
				found := false
				for _, actualError := range result.Errors {
					if contains([]string{actualError.Message}, expectedError) ||
						contains([]string{actualError.Error()}, expectedError) {
						found = true
						break
					}
				}
				if !found {
					fmt.Printf("  ❌ Expected error not found: %s\n", expectedError)
					expectedFound = false
				}
			}
			if !expectedFound {
				testPassed = false
			}
		}

		// Check expected warnings
		if len(scenario.ExpectedWarnings) > 0 {
			for _, expectedWarning := range scenario.ExpectedWarnings {
				found := false
				for _, actualWarning := range result.Warnings {
					if contains([]string{actualWarning.Message}, expectedWarning) ||
						contains([]string{actualWarning.Error()}, expectedWarning) {
						found = true
						break
					}
				}
				if !found {
					fmt.Printf("  ⚠️  Expected warning not found: %s\n", expectedWarning)
				}
			}
		}

		if testPassed {
			fmt.Printf("  ✅ Passed\n")
			passed++
		} else {
			failed++
		}
		fmt.Println()
	}

	// Summary
	fmt.Printf("📊 Test Results: %d passed, %d failed\n", passed, failed)
	if failed > 0 {
		fmt.Printf("❌ Some tests failed. Please review the implementation.\n")
		os.Exit(1)
	} else {
		fmt.Printf("✅ All tests passed!\n")
	}
}
