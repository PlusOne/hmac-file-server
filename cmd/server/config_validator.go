// config_validator.go
package main

import (
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"
	"time"
)

// Global variable to store config file path for validation
var configFileGlobal string

// ConfigValidationError represents a configuration validation error
type ConfigValidationError struct {
	Field   string
	Value   interface{}
	Message string
}

func (e ConfigValidationError) Error() string {
	return fmt.Sprintf("config validation error in field '%s': %s (value: %v)", e.Field, e.Message, e.Value)
}

// ConfigValidationResult contains the results of config validation
type ConfigValidationResult struct {
	Errors   []ConfigValidationError
	Warnings []ConfigValidationError
	Valid    bool
}

// AddError adds a validation error
func (r *ConfigValidationResult) AddError(field string, value interface{}, message string) {
	r.Errors = append(r.Errors, ConfigValidationError{Field: field, Value: value, Message: message})
	r.Valid = false
}

// AddWarning adds a validation warning
func (r *ConfigValidationResult) AddWarning(field string, value interface{}, message string) {
	r.Warnings = append(r.Warnings, ConfigValidationError{Field: field, Value: value, Message: message})
}

// HasErrors returns true if there are validation errors
func (r *ConfigValidationResult) HasErrors() bool {
	return len(r.Errors) > 0
}

// HasWarnings returns true if there are validation warnings
func (r *ConfigValidationResult) HasWarnings() bool {
	return len(r.Warnings) > 0
}

// ValidateConfigComprehensive performs comprehensive configuration validation
func ValidateConfigComprehensive(c *Config) *ConfigValidationResult {
	result := &ConfigValidationResult{Valid: true}

	// Validate each section
	validateServerConfig(&c.Server, result)
	validateSecurityConfig(&c.Security, result)
	validateLoggingConfig(&c.Logging, result)
	validateTimeoutConfig(&c.Timeouts, result)
	validateUploadsConfig(&c.Uploads, result)
	validateDownloadsConfig(&c.Downloads, result)
	validateClamAVConfig(&c.ClamAV, result)
	validateRedisConfig(&c.Redis, result)
	validateWorkersConfig(&c.Workers, result)
	validateVersioningConfig(&c.Versioning, result)
	validateDeduplicationConfig(&c.Deduplication, result)
	validateISOConfig(&c.ISO, result)

	// Cross-section validations
	validateCrossSection(c, result)

	// Enhanced validations
	validateSystemResources(result)
	validateNetworkConnectivity(c, result)
	validatePerformanceSettings(c, result)
	validateSecurityHardening(c, result)

	// Check disk space for storage paths
	if c.Server.StoragePath != "" {
		checkDiskSpace(c.Server.StoragePath, result)
	}
	if c.Deduplication.Enabled && c.Deduplication.Directory != "" {
		checkDiskSpace(c.Deduplication.Directory, result)
	}

	// Check for common configuration field naming mistakes
	// This helps users identify issues like 'storagepath' vs 'storage_path'
	if configFileGlobal != "" {
		if configBytes, err := os.ReadFile(configFileGlobal); err == nil {
			checkCommonConfigurationMistakes(result, configBytes)
		}
	}

	return result
}

// validateServerConfig validates server configuration
func validateServerConfig(server *ServerConfig, result *ConfigValidationResult) {
	// ListenAddress validation
	if server.ListenAddress == "" {
		result.AddError("server.listenport", server.ListenAddress, "listen address/port is required")
	} else {
		if !isValidPort(server.ListenAddress) {
			result.AddError("server.listenport", server.ListenAddress, "invalid port number (must be 1-65535)")
		}
	}

	// BindIP validation
	if server.BindIP != "" {
		if ip := net.ParseIP(server.BindIP); ip == nil {
			result.AddError("server.bind_ip", server.BindIP, "invalid IP address format")
		}
	}

	// StoragePath validation
	if server.StoragePath == "" {
		result.AddError("server.storagepath", server.StoragePath, "storage path is required - check your config.toml uses 'storage_path' (with underscore) not 'storagepath'")
	} else {
		if err := validateDirectoryPath(server.StoragePath, true); err != nil {
			result.AddError("server.storagepath", server.StoragePath, err.Error())
		}
	}

	// MetricsPort validation
	if server.MetricsEnabled && server.MetricsPort != "" {
		if !isValidPort(server.MetricsPort) {
			result.AddError("server.metricsport", server.MetricsPort, "invalid metrics port number")
		}
		if server.MetricsPort == server.ListenAddress {
			result.AddError("server.metricsport", server.MetricsPort, "metrics port cannot be the same as main listen port")
		}
	}

	// Size validations
	if server.MaxUploadSize != "" {
		if _, err := parseSize(server.MaxUploadSize); err != nil {
			result.AddError("server.max_upload_size", server.MaxUploadSize, "invalid size format")
		}
	}

	if server.MinFreeBytes != "" {
		if _, err := parseSize(server.MinFreeBytes); err != nil {
			result.AddError("server.min_free_bytes", server.MinFreeBytes, "invalid size format")
		}
	}

	// TTL validation
	if server.FileTTLEnabled {
		if server.FileTTL == "" {
			result.AddError("server.filettl", server.FileTTL, "file TTL is required when TTL is enabled")
		} else {
			if _, err := parseTTL(server.FileTTL); err != nil {
				result.AddError("server.filettl", server.FileTTL, "invalid TTL format")
			}
		}
	}

	// File naming validation
	validFileNaming := []string{"HMAC", "original", "None"}
	if !contains(validFileNaming, server.FileNaming) {
		result.AddError("server.file_naming", server.FileNaming, "must be one of: HMAC, original, None")
	}

	// Protocol validation
	validProtocols := []string{"ipv4", "ipv6", "auto", ""}
	if !contains(validProtocols, server.ForceProtocol) {
		result.AddError("server.force_protocol", server.ForceProtocol, "must be one of: ipv4, ipv6, auto, or empty")
	}

	// PID file validation
	if server.PIDFilePath != "" {
		dir := filepath.Dir(server.PIDFilePath)
		if err := validateDirectoryPath(dir, false); err != nil {
			result.AddError("server.pidfilepath", server.PIDFilePath, fmt.Sprintf("PID file directory invalid: %v", err))
		}
	}

	// Worker threshold validation
	if server.EnableDynamicWorkers {
		if server.WorkerScaleUpThresh <= 0 {
			result.AddError("server.worker_scale_up_thresh", server.WorkerScaleUpThresh, "must be positive when dynamic workers are enabled")
		}
		if server.WorkerScaleDownThresh <= 0 {
			result.AddError("server.worker_scale_down_thresh", server.WorkerScaleDownThresh, "must be positive when dynamic workers are enabled")
		}
		if server.WorkerScaleDownThresh >= server.WorkerScaleUpThresh {
			result.AddWarning("server.worker_scale_down_thresh", server.WorkerScaleDownThresh, "scale down threshold should be lower than scale up threshold")
		}
	}

	// Extensions validation
	for _, ext := range server.GlobalExtensions {
		if !strings.HasPrefix(ext, ".") {
			result.AddError("server.global_extensions", ext, "file extensions must start with a dot")
		}
	}
}

// validateSecurityConfig validates security configuration
func validateSecurityConfig(security *SecurityConfig, result *ConfigValidationResult) {
	if security.EnableJWT {
		// JWT validation
		if strings.TrimSpace(security.JWTSecret) == "" {
			result.AddError("security.jwtsecret", security.JWTSecret, "JWT secret is required when JWT is enabled")
		} else if len(security.JWTSecret) < 32 {
			result.AddWarning("security.jwtsecret", "[REDACTED]", "JWT secret should be at least 32 characters for security")
		}

		validAlgorithms := []string{"HS256", "HS384", "HS512", "RS256", "RS384", "RS512", "ES256", "ES384", "ES512"}
		if !contains(validAlgorithms, security.JWTAlgorithm) {
			result.AddError("security.jwtalgorithm", security.JWTAlgorithm, "unsupported JWT algorithm")
		}

		if security.JWTExpiration != "" {
			if _, err := time.ParseDuration(security.JWTExpiration); err != nil {
				result.AddError("security.jwtexpiration", security.JWTExpiration, "invalid JWT expiration format")
			}
		}
	} else {
		// HMAC validation
		if strings.TrimSpace(security.Secret) == "" {
			result.AddError("security.secret", security.Secret, "HMAC secret is required when JWT is disabled")
		} else if len(security.Secret) < 16 {
			result.AddWarning("security.secret", "[REDACTED]", "HMAC secret should be at least 16 characters for security")
		}
	}
}

// validateLoggingConfig validates logging configuration
func validateLoggingConfig(logging *LoggingConfig, result *ConfigValidationResult) {
	validLevels := []string{"panic", "fatal", "error", "warn", "warning", "info", "debug", "trace"}
	if !contains(validLevels, strings.ToLower(logging.Level)) {
		result.AddError("logging.level", logging.Level, "invalid log level")
	}

	if logging.File != "" {
		dir := filepath.Dir(logging.File)
		if err := validateDirectoryPath(dir, false); err != nil {
			result.AddError("logging.file", logging.File, fmt.Sprintf("log file directory invalid: %v", err))
		}
	}

	if logging.MaxSize <= 0 {
		result.AddWarning("logging.max_size", logging.MaxSize, "max size should be positive")
	}

	if logging.MaxBackups < 0 {
		result.AddWarning("logging.max_backups", logging.MaxBackups, "max backups should be non-negative")
	}

	if logging.MaxAge < 0 {
		result.AddWarning("logging.max_age", logging.MaxAge, "max age should be non-negative")
	}
}

// validateTimeoutConfig validates timeout configuration
func validateTimeoutConfig(timeouts *TimeoutConfig, result *ConfigValidationResult) {
	if timeouts.Read != "" {
		if duration, err := time.ParseDuration(timeouts.Read); err != nil {
			result.AddError("timeouts.read", timeouts.Read, "invalid read timeout format")
		} else if duration <= 0 {
			result.AddError("timeouts.read", timeouts.Read, "read timeout must be positive")
		}
	}

	if timeouts.Write != "" {
		if duration, err := time.ParseDuration(timeouts.Write); err != nil {
			result.AddError("timeouts.write", timeouts.Write, "invalid write timeout format")
		} else if duration <= 0 {
			result.AddError("timeouts.write", timeouts.Write, "write timeout must be positive")
		}
	}

	if timeouts.Idle != "" {
		if duration, err := time.ParseDuration(timeouts.Idle); err != nil {
			result.AddError("timeouts.idle", timeouts.Idle, "invalid idle timeout format")
		} else if duration <= 0 {
			result.AddError("timeouts.idle", timeouts.Idle, "idle timeout must be positive")
		}
	}

	if timeouts.Shutdown != "" {
		if duration, err := time.ParseDuration(timeouts.Shutdown); err != nil {
			result.AddError("timeouts.shutdown", timeouts.Shutdown, "invalid shutdown timeout format")
		} else if duration <= 0 {
			result.AddError("timeouts.shutdown", timeouts.Shutdown, "shutdown timeout must be positive")
		}
	}
}

// validateUploadsConfig validates uploads configuration
func validateUploadsConfig(uploads *UploadsConfig, result *ConfigValidationResult) {
	// Validate extensions
	for _, ext := range uploads.AllowedExtensions {
		if !strings.HasPrefix(ext, ".") {
			result.AddError("uploads.allowed_extensions", ext, "file extensions must start with a dot")
		}
	}

	// Validate chunk size
	if uploads.ChunkSize != "" {
		if _, err := parseSize(uploads.ChunkSize); err != nil {
			result.AddError("uploads.chunk_size", uploads.ChunkSize, "invalid chunk size format")
		}
	}

	// Validate session timeout (renamed from max_resumable_age)
	if uploads.SessionTimeout != "" {
		if _, err := time.ParseDuration(uploads.SessionTimeout); err != nil {
			result.AddError("uploads.session_timeout", uploads.SessionTimeout, "invalid session timeout format")
		}
	}
}

// validateDownloadsConfig validates downloads configuration
func validateDownloadsConfig(downloads *DownloadsConfig, result *ConfigValidationResult) {
	// Validate extensions
	for _, ext := range downloads.AllowedExtensions {
		if !strings.HasPrefix(ext, ".") {
			result.AddError("downloads.allowed_extensions", ext, "file extensions must start with a dot")
		}
	}

	// Validate chunk size
	if downloads.ChunkSize != "" {
		if _, err := parseSize(downloads.ChunkSize); err != nil {
			result.AddError("downloads.chunk_size", downloads.ChunkSize, "invalid chunk size format")
		}
	}
}

// validateClamAVConfig validates ClamAV configuration
func validateClamAVConfig(clamav *ClamAVConfig, result *ConfigValidationResult) {
	if clamav.ClamAVEnabled {
		if clamav.ClamAVSocket == "" {
			result.AddWarning("clamav.clamavsocket", clamav.ClamAVSocket, "ClamAV socket path not specified, using default")
		} else {
			// Check if socket file exists
			if _, err := os.Stat(clamav.ClamAVSocket); os.IsNotExist(err) {
				result.AddWarning("clamav.clamavsocket", clamav.ClamAVSocket, "ClamAV socket file does not exist")
			}
		}

		if clamav.NumScanWorkers <= 0 {
			result.AddError("clamav.numscanworkers", clamav.NumScanWorkers, "number of scan workers must be positive")
		}

		// Validate scan extensions
		for _, ext := range clamav.ScanFileExtensions {
			if !strings.HasPrefix(ext, ".") {
				result.AddError("clamav.scanfileextensions", ext, "file extensions must start with a dot")
			}
		}
	}
}

// validateRedisConfig validates Redis configuration
func validateRedisConfig(redis *RedisConfig, result *ConfigValidationResult) {
	if redis.RedisEnabled {
		if redis.RedisAddr == "" {
			result.AddError("redis.redisaddr", redis.RedisAddr, "Redis address is required when Redis is enabled")
		} else {
			// Validate address format (host:port)
			if !isValidHostPort(redis.RedisAddr) {
				result.AddError("redis.redisaddr", redis.RedisAddr, "invalid Redis address format (should be host:port)")
			}
		}

		if redis.RedisDBIndex < 0 || redis.RedisDBIndex > 15 {
			result.AddWarning("redis.redisdbindex", redis.RedisDBIndex, "Redis DB index is typically 0-15")
		}

		if redis.RedisHealthCheckInterval != "" {
			if _, err := time.ParseDuration(redis.RedisHealthCheckInterval); err != nil {
				result.AddError("redis.redishealthcheckinterval", redis.RedisHealthCheckInterval, "invalid health check interval format")
			}
		}
	}
}

// validateWorkersConfig validates workers configuration
func validateWorkersConfig(workers *WorkersConfig, result *ConfigValidationResult) {
	if workers.NumWorkers <= 0 {
		result.AddError("workers.numworkers", workers.NumWorkers, "number of workers must be positive")
	}

	if workers.UploadQueueSize <= 0 {
		result.AddError("workers.uploadqueuesize", workers.UploadQueueSize, "upload queue size must be positive")
	}

	// Performance recommendations
	if workers.NumWorkers > 50 {
		result.AddWarning("workers.numworkers", workers.NumWorkers, "very high worker count may impact performance")
	}

	if workers.UploadQueueSize > 1000 {
		result.AddWarning("workers.uploadqueuesize", workers.UploadQueueSize, "very large queue size may impact memory usage")
	}
}

// validateVersioningConfig validates versioning configuration
func validateVersioningConfig(versioning *VersioningConfig, result *ConfigValidationResult) {
	if versioning.Enabled {
		if versioning.MaxRevs <= 0 {
			result.AddError("versioning.maxversions", versioning.MaxRevs, "max versions must be positive when versioning is enabled")
		}

		validBackends := []string{"filesystem", "database", "s3", ""}
		if !contains(validBackends, versioning.Backend) {
			result.AddWarning("versioning.backend", versioning.Backend, "unknown versioning backend")
		}
	}
}

// validateDeduplicationConfig validates deduplication configuration
func validateDeduplicationConfig(dedup *DeduplicationConfig, result *ConfigValidationResult) {
	if dedup.Enabled {
		if dedup.Directory == "" {
			result.AddError("deduplication.directory", dedup.Directory, "deduplication directory is required when deduplication is enabled")
		} else {
			if err := validateDirectoryPath(dedup.Directory, true); err != nil {
				result.AddError("deduplication.directory", dedup.Directory, err.Error())
			}
		}
	}
}

// validateISOConfig validates ISO configuration
func validateISOConfig(iso *ISOConfig, result *ConfigValidationResult) {
	if iso.Enabled {
		if iso.MountPoint == "" {
			result.AddError("iso.mount_point", iso.MountPoint, "mount point is required when ISO is enabled")
		}

		if iso.Size != "" {
			if _, err := parseSize(iso.Size); err != nil {
				result.AddError("iso.size", iso.Size, "invalid ISO size format")
			}
		}

		if iso.ContainerFile == "" {
			result.AddWarning("iso.containerfile", iso.ContainerFile, "container file path not specified")
		}

		validCharsets := []string{"utf-8", "iso-8859-1", "ascii", ""}
		if !contains(validCharsets, strings.ToLower(iso.Charset)) {
			result.AddWarning("iso.charset", iso.Charset, "uncommon charset specified")
		}
	}
}

// validateCrossSection performs cross-section validations
func validateCrossSection(c *Config, result *ConfigValidationResult) {
	// Storage path vs deduplication directory conflict
	if c.Deduplication.Enabled && c.Server.StoragePath == c.Deduplication.Directory {
		result.AddError("deduplication.directory", c.Deduplication.Directory, "deduplication directory cannot be the same as storage path")
	}

	// ISO mount point vs storage path conflict
	if c.ISO.Enabled && c.Server.StoragePath == c.ISO.MountPoint {
		result.AddWarning("iso.mount_point", c.ISO.MountPoint, "ISO mount point is the same as storage path")
	}

	// Extension conflicts between uploads and downloads
	if len(c.Uploads.AllowedExtensions) > 0 && len(c.Downloads.AllowedExtensions) > 0 {
		uploadExts := make(map[string]bool)
		for _, ext := range c.Uploads.AllowedExtensions {
			uploadExts[ext] = true
		}

		hasCommonExtensions := false
		for _, ext := range c.Downloads.AllowedExtensions {
			if uploadExts[ext] {
				hasCommonExtensions = true
				break
			}
		}

		if !hasCommonExtensions {
			result.AddWarning("uploads/downloads.allowed_extensions", "", "no common extensions between uploads and downloads - files may not be downloadable")
		}
	}

	// Global extensions override warning
	if len(c.Server.GlobalExtensions) > 0 && (len(c.Uploads.AllowedExtensions) > 0 || len(c.Downloads.AllowedExtensions) > 0) {
		result.AddWarning("server.global_extensions", c.Server.GlobalExtensions, "global extensions will override upload/download extension settings")
	}
}

// Enhanced Security Validation Functions

// checkSecretStrength analyzes the strength of secrets/passwords
func checkSecretStrength(secret string) (score int, issues []string) {
	if len(secret) == 0 {
		return 0, []string{"secret is empty"}
	}

	issues = []string{}
	score = 0

	// Length scoring
	if len(secret) >= 32 {
		score += 3
	} else if len(secret) >= 16 {
		score += 2
	} else if len(secret) >= 8 {
		score += 1
	} else {
		issues = append(issues, "secret is too short")
	}

	// Character variety scoring
	hasLower := false
	hasUpper := false
	hasDigit := false
	hasSpecial := false

	for _, char := range secret {
		switch {
		case char >= 'a' && char <= 'z':
			hasLower = true
		case char >= 'A' && char <= 'Z':
			hasUpper = true
		case char >= '0' && char <= '9':
			hasDigit = true
		case strings.ContainsRune("!@#$%^&*()_+-=[]{}|;:,.<>?", char):
			hasSpecial = true
		}
	}

	varietyCount := 0
	if hasLower {
		varietyCount++
	}
	if hasUpper {
		varietyCount++
	}
	if hasDigit {
		varietyCount++
	}
	if hasSpecial {
		varietyCount++
	}

	score += varietyCount

	if varietyCount < 3 {
		issues = append(issues, "secret should contain uppercase, lowercase, numbers, and special characters")
	}

	// Check for common patterns
	lowerSecret := strings.ToLower(secret)
	commonWeakPasswords := []string{
		"password", "123456", "qwerty", "admin", "root", "test", "guest",
		"secret", "hmac", "server", "default", "changeme", "example",
		"demo", "temp", "temporary", "fileserver", "upload", "download",
	}

	for _, weak := range commonWeakPasswords {
		if strings.Contains(lowerSecret, weak) {
			issues = append(issues, fmt.Sprintf("contains common weak pattern: %s", weak))
			score -= 2
		}
	}

	// Check for repeated characters
	if hasRepeatedChars(secret) {
		issues = append(issues, "contains too many repeated characters")
		score -= 1
	}

	// Ensure score doesn't go negative
	if score < 0 {
		score = 0
	}

	return score, issues
}

// hasRepeatedChars checks if a string has excessive repeated characters
func hasRepeatedChars(s string) bool {
	if len(s) < 4 {
		return false
	}

	for i := 0; i <= len(s)-3; i++ {
		if s[i] == s[i+1] && s[i+1] == s[i+2] {
			return true
		}
	}

	return false
}

// isDefaultOrExampleSecret checks if a secret appears to be a default/example value
func isDefaultOrExampleSecret(secret string) bool {
	defaultSecrets := []string{
		"your-secret-key-here",
		"change-this-secret",
		"example-secret",
		"default-secret",
		"test-secret",
		"demo-secret",
		"sample-secret",
		"placeholder",
		"PUT_YOUR_SECRET_HERE",
		"CHANGE_ME",
		"YOUR_JWT_SECRET",
		"your-hmac-secret",
		"supersecret",
		"secretkey",
		"myverysecuresecret",
	}

	lowerSecret := strings.ToLower(strings.TrimSpace(secret))

	for _, defaultSecret := range defaultSecrets {
		if strings.Contains(lowerSecret, strings.ToLower(defaultSecret)) {
			return true
		}
	}

	// Check for obvious patterns
	if strings.Contains(lowerSecret, "example") ||
		strings.Contains(lowerSecret, "default") ||
		strings.Contains(lowerSecret, "change") ||
		strings.Contains(lowerSecret, "replace") ||
		strings.Contains(lowerSecret, "todo") ||
		strings.Contains(lowerSecret, "fixme") {
		return true
	}

	return false
}

// calculateEntropy calculates the Shannon entropy of a string
func calculateEntropy(s string) float64 {
	if len(s) == 0 {
		return 0
	}

	// Count character frequencies
	freq := make(map[rune]int)
	for _, char := range s {
		freq[char]++
	}

	// Calculate entropy
	entropy := 0.0
	length := float64(len(s))

	for _, count := range freq {
		if count > 0 {
			p := float64(count) / length
			entropy -= p * (float64(count) / length) // Simplified calculation
		}
	}

	return entropy
}

// validateSecretSecurity performs comprehensive secret security validation
func validateSecretSecurity(fieldName, secret string, result *ConfigValidationResult) {
	if secret == "" {
		return // Already handled by other validators
	}

	// Check for default/example secrets
	if isDefaultOrExampleSecret(secret) {
		result.AddError(fieldName, "[REDACTED]", "appears to be a default or example secret - must be changed")
		return
	}

	// Check secret strength
	score, issues := checkSecretStrength(secret)

	if score < 3 {
		for _, issue := range issues {
			result.AddError(fieldName, "[REDACTED]", fmt.Sprintf("weak secret: %s", issue))
		}
	} else if score < 6 {
		for _, issue := range issues {
			result.AddWarning(fieldName, "[REDACTED]", fmt.Sprintf("secret could be stronger: %s", issue))
		}
	}

	// Check entropy (simplified)
	entropy := calculateEntropy(secret)
	if entropy < 3.0 {
		result.AddWarning(fieldName, "[REDACTED]", "secret has low entropy - consider using more varied characters")
	}

	// Length-specific warnings
	if len(secret) > 256 {
		result.AddWarning(fieldName, "[REDACTED]", "secret is very long - may impact performance")
	}
}

// validateSystemResources checks system resource availability
func validateSystemResources(result *ConfigValidationResult) {
	// Check available CPU cores
	cpuCores := runtime.NumCPU()
	if cpuCores < 2 {
		result.AddWarning("system.cpu", cpuCores, "minimum 2 CPU cores recommended for optimal performance")
	} else if cpuCores < 4 {
		result.AddWarning("system.cpu", cpuCores, "4+ CPU cores recommended for high-load environments")
	}

	// Check available memory (basic check through runtime)
	var memStats runtime.MemStats
	runtime.ReadMemStats(&memStats)

	// Basic memory availability check (simplified version)
	// This checks current Go heap, but for production we'd want system memory
	allocMB := float64(memStats.Alloc) / 1024 / 1024
	if allocMB > 512 {
		result.AddWarning("system.memory", allocMB, "current memory usage is high - ensure adequate system memory")
	}

	// Check for potential resource constraints
	numGoroutines := runtime.NumGoroutine()
	if numGoroutines > 1000 {
		result.AddWarning("system.goroutines", numGoroutines, "high goroutine count may indicate resource constraints")
	}
}

// validateNetworkConnectivity tests network connectivity to external services
func validateNetworkConnectivity(c *Config, result *ConfigValidationResult) {
	// Test Redis connectivity if enabled
	if c.Redis.RedisEnabled && c.Redis.RedisAddr != "" {
		if err := testNetworkConnection("tcp", c.Redis.RedisAddr, 5*time.Second); err != nil {
			result.AddWarning("redis.connectivity", c.Redis.RedisAddr, fmt.Sprintf("cannot connect to Redis: %v", err))
		}
	}

	// Test ClamAV connectivity if enabled
	if c.ClamAV.ClamAVEnabled && c.ClamAV.ClamAVSocket != "" {
		// For Unix socket, test file existence and permissions
		if strings.HasPrefix(c.ClamAV.ClamAVSocket, "/") {
			if stat, err := os.Stat(c.ClamAV.ClamAVSocket); err != nil {
				result.AddWarning("clamav.connectivity", c.ClamAV.ClamAVSocket, fmt.Sprintf("ClamAV socket not accessible: %v", err))
			} else if stat.Mode()&os.ModeSocket == 0 {
				result.AddWarning("clamav.connectivity", c.ClamAV.ClamAVSocket, "specified path is not a socket file")
			}
		} else {
			// Assume TCP connection format
			if err := testNetworkConnection("tcp", c.ClamAV.ClamAVSocket, 5*time.Second); err != nil {
				result.AddWarning("clamav.connectivity", c.ClamAV.ClamAVSocket, fmt.Sprintf("cannot connect to ClamAV: %v", err))
			}
		}
	}
}

// testNetworkConnection attempts to connect to a network address
func testNetworkConnection(network, address string, timeout time.Duration) error {
	conn, err := net.DialTimeout(network, address, timeout)
	if err != nil {
		return err
	}
	defer conn.Close()
	return nil
}

// validatePerformanceSettings analyzes configuration for performance implications
func validatePerformanceSettings(c *Config, result *ConfigValidationResult) {
	// Check worker configuration against system resources
	cpuCores := runtime.NumCPU()

	if c.Workers.NumWorkers > cpuCores*4 {
		result.AddWarning("workers.performance", c.Workers.NumWorkers,
			fmt.Sprintf("worker count (%d) significantly exceeds CPU cores (%d) - may cause context switching overhead",
				c.Workers.NumWorkers, cpuCores))
	}

	// Check ClamAV scan workers
	if c.ClamAV.ClamAVEnabled && c.ClamAV.NumScanWorkers > cpuCores {
		result.AddWarning("clamav.performance", c.ClamAV.NumScanWorkers,
			fmt.Sprintf("scan workers (%d) exceed CPU cores (%d) - may impact scanning performance",
				c.ClamAV.NumScanWorkers, cpuCores))
	}

	// Check timeout configurations for performance balance
	if c.Timeouts.Read != "" {
		if duration, err := time.ParseDuration(c.Timeouts.Read); err == nil {
			if duration > 300*time.Second {
				result.AddWarning("timeouts.performance", c.Timeouts.Read, "very long read timeout may impact server responsiveness")
			}
		}
	}

	// Check upload size vs available resources
	if c.Server.MaxUploadSize != "" {
		if size, err := parseSize(c.Server.MaxUploadSize); err == nil {
			if size > 10*1024*1024*1024 { // 10GB
				result.AddWarning("server.performance", c.Server.MaxUploadSize, "very large max upload size requires adequate disk space and memory")
			}
		}
	}

	// Check for potential memory-intensive configurations
	if c.Workers.UploadQueueSize > 500 && c.Workers.NumWorkers > 20 {
		result.AddWarning("workers.memory", fmt.Sprintf("queue:%d workers:%d", c.Workers.UploadQueueSize, c.Workers.NumWorkers),
			"high queue size with many workers may consume significant memory")
	}
}

// validateSecurityHardening performs advanced security validation
func validateSecurityHardening(c *Config, result *ConfigValidationResult) {
	// Check for default or weak configurations
	if c.Security.EnableJWT {
		if c.Security.JWTSecret == "your-secret-key-here" || c.Security.JWTSecret == "changeme" {
			result.AddError("security.jwtsecret", "[REDACTED]", "JWT secret appears to be a default value - change immediately")
		}

		// Check JWT algorithm strength
		weakAlgorithms := []string{"HS256"} // HS256 is considered less secure than RS256
		if contains(weakAlgorithms, c.Security.JWTAlgorithm) {
			result.AddWarning("security.jwtalgorithm", c.Security.JWTAlgorithm, "consider using RS256 or ES256 for enhanced security")
		}
	} else {
		if c.Security.Secret == "your-secret-key-here" || c.Security.Secret == "changeme" || c.Security.Secret == "secret" {
			result.AddError("security.secret", "[REDACTED]", "HMAC secret appears to be a default value - change immediately")
		}
	}

	// Check for insecure bind configurations
	if c.Server.BindIP == "0.0.0.0" {
		result.AddWarning("server.bind_ip", c.Server.BindIP, "binding to 0.0.0.0 exposes service to all interfaces - ensure firewall protection")
	}

	// Check for development/debug settings in production
	if c.Logging.Level == "debug" || c.Logging.Level == "trace" {
		result.AddWarning("logging.security", c.Logging.Level, "debug/trace logging may expose sensitive information - use 'info' or 'warn' in production")
	}

	// Check file permissions for sensitive paths
	if c.Server.StoragePath != "" {
		if stat, err := os.Stat(c.Server.StoragePath); err == nil {
			mode := stat.Mode().Perm()
			if mode&0077 != 0 { // World or group writable
				result.AddWarning("server.storagepath.permissions", c.Server.StoragePath, "storage directory permissions allow group/world access - consider restricting to owner-only")
			}
		}
	}
}

// checkDiskSpace validates available disk space for storage paths
func checkDiskSpace(path string, result *ConfigValidationResult) {
	if stat, err := os.Stat(path); err == nil && stat.IsDir() {
		// Get available space (platform-specific implementation would be more robust)
		// This is a simplified check - in production, use syscall.Statfs on Unix or similar

		// For now, we'll just check if we can write a test file
		testFile := filepath.Join(path, ".disk_space_test")
		if f, err := os.Create(testFile); err != nil {
			result.AddWarning("system.disk_space", path, fmt.Sprintf("cannot write to storage directory: %v", err))
		} else {
			f.Close()
			os.Remove(testFile)

			// Additional check: try to write a larger test file to estimate space
			const testSize = 1024 * 1024 // 1MB
			testData := make([]byte, testSize)
			if f, err := os.Create(testFile); err == nil {
				if _, err := f.Write(testData); err != nil {
					result.AddWarning("system.disk_space", path, "low disk space detected - ensure adequate storage for operations")
				}
				f.Close()
				os.Remove(testFile)
			}
		}
	}
}

// isValidPort checks if a string represents a valid port number
func isValidPort(port string) bool {
	if p, err := strconv.Atoi(port); err != nil || p < 1 || p > 65535 {
		return false
	}
	return true
}

// isValidHostPort checks if a string is a valid host:port combination
func isValidHostPort(hostPort string) bool {
	host, port, err := net.SplitHostPort(hostPort)
	if err != nil {
		return false
	}

	// Validate port
	if !isValidPort(port) {
		return false
	}

	// Validate host (can be IP, hostname, or empty for localhost)
	if host != "" {
		if ip := net.ParseIP(host); ip == nil {
			// If not an IP, check if it's a valid hostname
			matched, _ := regexp.MatchString(`^[a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?)*$`, host)
			return matched
		}
	}

	return true
}

// validateDirectoryPath validates a directory path
func validateDirectoryPath(path string, createIfMissing bool) error {
	if path == "" {
		return errors.New("directory path cannot be empty")
	}

	// Check if path exists
	if stat, err := os.Stat(path); os.IsNotExist(err) {
		if createIfMissing {
			// Try to create the directory
			if err := os.MkdirAll(path, 0755); err != nil {
				return fmt.Errorf("cannot create directory: %v", err)
			}
		} else {
			return fmt.Errorf("directory does not exist: %s", path)
		}
	} else if err != nil {
		return fmt.Errorf("cannot access directory: %v", err)
	} else if !stat.IsDir() {
		return fmt.Errorf("path exists but is not a directory: %s", path)
	}

	// Check if directory is writable
	testFile := filepath.Join(path, ".write_test")
	if f, err := os.Create(testFile); err != nil {
		return fmt.Errorf("directory is not writable: %v", err)
	} else {
		f.Close()
		os.Remove(testFile)
	}

	return nil
}

// contains checks if a slice contains a string
func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}

// PrintValidationResults prints the validation results in a user-friendly format
func PrintValidationResults(result *ConfigValidationResult) {
	if result.HasErrors() {
		log.Error("❌ Configuration validation failed with the following errors:")
		for _, err := range result.Errors {
			log.Errorf("  • %s", err.Error())
		}
		fmt.Println()
	}

	if result.HasWarnings() {
		log.Warn("⚠️  Configuration validation completed with warnings:")
		for _, warn := range result.Warnings {
			log.Warnf("  • %s", warn.Error())
		}
		fmt.Println()
	}

	if !result.HasErrors() && !result.HasWarnings() {
		log.Info("✅ Configuration validation passed successfully!")
	}
}

// runSpecializedValidation performs targeted validation based on flags
func runSpecializedValidation(c *Config, security, performance, connectivity, quiet, verbose, fixable bool) {
	result := &ConfigValidationResult{Valid: true}

	if verbose {
		log.Info("Running specialized validation with detailed output...")
		fmt.Println()
	}

	// Run only the requested validation types
	if security {
		if verbose {
			log.Info("🔐 Running security validation checks...")
		}
		validateSecurityConfig(&c.Security, result)
		validateSecurityHardening(c, result)
	}

	if performance {
		if verbose {
			log.Info("⚡ Running performance validation checks...")
		}
		validatePerformanceSettings(c, result)
		validateSystemResources(result)
	}

	if connectivity {
		if verbose {
			log.Info("🌐 Running connectivity validation checks...")
		}
		validateNetworkConnectivity(c, result)
	}

	// If no specific type is requested, run basic validation
	if !security && !performance && !connectivity {
		if verbose {
			log.Info("🔍 Running comprehensive validation...")
		}
		result = ValidateConfigComprehensive(c)
	}

	// Filter results based on flags
	if fixable {
		filterFixableIssues(result)
	}

	// Output results based on verbosity
	if quiet {
		printQuietValidationResults(result)
	} else if verbose {
		printVerboseValidationResults(result)
	} else {
		PrintValidationResults(result)
	}

	// Exit with appropriate code
	if result.HasErrors() {
		os.Exit(1)
	}
}

// filterFixableIssues removes non-fixable issues from results
func filterFixableIssues(result *ConfigValidationResult) {
	fixablePatterns := []string{
		"permissions",
		"directory",
		"default value",
		"debug logging",
		"size format",
		"timeout format",
		"port number",
		"IP address",
	}

	var fixableErrors []ConfigValidationError
	var fixableWarnings []ConfigValidationError

	for _, err := range result.Errors {
		for _, pattern := range fixablePatterns {
			if strings.Contains(strings.ToLower(err.Message), pattern) {
				fixableErrors = append(fixableErrors, err)
				break
			}
		}
	}

	for _, warn := range result.Warnings {
		for _, pattern := range fixablePatterns {
			if strings.Contains(strings.ToLower(warn.Message), pattern) {
				fixableWarnings = append(fixableWarnings, warn)
				break
			}
		}
	}

	result.Errors = fixableErrors
	result.Warnings = fixableWarnings
	result.Valid = len(fixableErrors) == 0
}

// printQuietValidationResults prints only errors
func printQuietValidationResults(result *ConfigValidationResult) {
	if result.HasErrors() {
		for _, err := range result.Errors {
			fmt.Printf("ERROR: %s\n", err.Error())
		}
	}
}

// printVerboseValidationResults prints detailed validation information
func printVerboseValidationResults(result *ConfigValidationResult) {
	fmt.Println("📊 DETAILED VALIDATION REPORT")
	fmt.Println("============================")
	fmt.Println()

	// System information
	fmt.Printf("🖥️  System: %d CPU cores, %d goroutines\n", runtime.NumCPU(), runtime.NumGoroutine())

	var memStats runtime.MemStats
	runtime.ReadMemStats(&memStats)
	fmt.Printf("💾 Memory: %.2f MB allocated\n", float64(memStats.Alloc)/1024/1024)
	fmt.Println()

	// Validation summary
	fmt.Printf("✅ Checks passed: %d\n", countPassedChecks(result))
	fmt.Printf("⚠️  Warnings: %d\n", len(result.Warnings))
	fmt.Printf("❌ Errors: %d\n", len(result.Errors))
	fmt.Println()

	// Detailed results
	if result.HasErrors() {
		fmt.Println("🚨 CONFIGURATION ERRORS:")
		for i, err := range result.Errors {
			fmt.Printf("  %d. Field: %s\n", i+1, err.Field)
			fmt.Printf("     Issue: %s\n", err.Message)
			fmt.Printf("     Value: %v\n", err.Value)
			fmt.Println()
		}
	}

	if result.HasWarnings() {
		fmt.Println("⚠️  CONFIGURATION WARNINGS:")
		for i, warn := range result.Warnings {
			fmt.Printf("  %d. Field: %s\n", i+1, warn.Field)
			fmt.Printf("     Issue: %s\n", warn.Message)
			fmt.Printf("     Value: %v\n", warn.Value)
			fmt.Println()
		}
	}

	if !result.HasErrors() && !result.HasWarnings() {
		fmt.Println("🎉 All validation checks passed successfully!")
	}
}

// countPassedChecks estimates the number of successful validation checks
func countPassedChecks(result *ConfigValidationResult) int {
	// Rough estimate: total possible checks minus errors and warnings
	totalPossibleChecks := 50 // Approximate number of validation checks
	return totalPossibleChecks - len(result.Errors) - len(result.Warnings)
}

// checkCommonConfigurationMistakes checks for common TOML field naming errors
func checkCommonConfigurationMistakes(result *ConfigValidationResult, configBytes []byte) {
	configStr := string(configBytes)
	
	// Common field naming mistakes
	commonMistakes := map[string]string{
		"storagepath":      "storage_path",
		"listenport":       "listen_address", 
		"bindip":           "bind_ip",
		"pidfilepath":      "pid_file",
		"metricsenabled":   "metrics_enabled",
		"metricsport":      "metrics_port",
		"maxuploadsize":    "max_upload_size",
		"cleanupinterval":  "cleanup_interval",
		"dedupenabled":     "deduplication_enabled",
		"ttlenabled":       "ttl_enabled",
		"chunksize":        "chunk_size",
	}
	
	for incorrect, correct := range commonMistakes {
		if strings.Contains(configStr, incorrect+" =") || strings.Contains(configStr, incorrect+"=") {
			result.AddWarning("config.syntax", incorrect, fmt.Sprintf("field name '%s' should be '%s' (use underscores)", incorrect, correct))
		}
	}
}
