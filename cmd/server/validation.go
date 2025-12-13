// validation.go - Content type validation using magic bytes

package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
	"sync"
)

// ValidationConfig holds content validation configuration
type ValidationConfig struct {
	CheckMagicBytes bool     `toml:"check_magic_bytes" mapstructure:"check_magic_bytes"`
	AllowedTypes    []string `toml:"allowed_types" mapstructure:"allowed_types"`
	BlockedTypes    []string `toml:"blocked_types" mapstructure:"blocked_types"`
	MaxFileSize     string   `toml:"max_file_size" mapstructure:"max_file_size"`
	StrictMode      bool     `toml:"strict_mode" mapstructure:"strict_mode"` // Reject if type can't be detected
}

// ValidationResult contains the result of content validation
type ValidationResult struct {
	Valid        bool   `json:"valid"`
	DetectedType string `json:"detected_type"`
	DeclaredType string `json:"declared_type,omitempty"`
	Error        string `json:"error,omitempty"`
	Reason       string `json:"reason,omitempty"`
}

// ValidationError represents a validation failure
type ValidationError struct {
	Code         string `json:"error"`
	Message      string `json:"message"`
	DetectedType string `json:"detected_type"`
	DeclaredType string `json:"declared_type,omitempty"`
}

func (e *ValidationError) Error() string {
	return e.Message
}

// ContentValidator handles content type validation
type ContentValidator struct {
	config        *ValidationConfig
	allowedTypes  map[string]bool
	blockedTypes  map[string]bool
	wildcardAllow []string
	wildcardBlock []string
}

var (
	contentValidator *ContentValidator
	validatorOnce    sync.Once
)

// InitContentValidator initializes the content validator
func InitContentValidator(config *ValidationConfig) {
	validatorOnce.Do(func() {
		contentValidator = &ContentValidator{
			config:        config,
			allowedTypes:  make(map[string]bool),
			blockedTypes:  make(map[string]bool),
			wildcardAllow: []string{},
			wildcardBlock: []string{},
		}

		// Process allowed types
		for _, t := range config.AllowedTypes {
			t = strings.ToLower(strings.TrimSpace(t))
			if strings.HasSuffix(t, "/*") {
				contentValidator.wildcardAllow = append(contentValidator.wildcardAllow, strings.TrimSuffix(t, "/*"))
			} else {
				contentValidator.allowedTypes[t] = true
			}
		}

		// Process blocked types
		for _, t := range config.BlockedTypes {
			t = strings.ToLower(strings.TrimSpace(t))
			if strings.HasSuffix(t, "/*") {
				contentValidator.wildcardBlock = append(contentValidator.wildcardBlock, strings.TrimSuffix(t, "/*"))
			} else {
				contentValidator.blockedTypes[t] = true
			}
		}

		log.Infof("Content validator initialized: magic_bytes=%v, allowed=%d types, blocked=%d types",
			config.CheckMagicBytes, len(config.AllowedTypes), len(config.BlockedTypes))
	})
}

// GetContentValidator returns the singleton content validator
func GetContentValidator() *ContentValidator {
	return contentValidator
}

// isTypeAllowed checks if a content type is in the allowed list
func (v *ContentValidator) isTypeAllowed(contentType string) bool {
	contentType = strings.ToLower(contentType)

	// Extract main type (before any parameters like charset)
	if idx := strings.Index(contentType, ";"); idx != -1 {
		contentType = strings.TrimSpace(contentType[:idx])
	}

	// If no allowed types configured, allow all (except blocked)
	if len(v.allowedTypes) == 0 && len(v.wildcardAllow) == 0 {
		return true
	}

	// Check exact match
	if v.allowedTypes[contentType] {
		return true
	}

	// Check wildcard patterns
	for _, prefix := range v.wildcardAllow {
		if strings.HasPrefix(contentType, prefix+"/") {
			return true
		}
	}

	return false
}

// isTypeBlocked checks if a content type is in the blocked list
func (v *ContentValidator) isTypeBlocked(contentType string) bool {
	contentType = strings.ToLower(contentType)

	// Extract main type (before any parameters)
	if idx := strings.Index(contentType, ";"); idx != -1 {
		contentType = strings.TrimSpace(contentType[:idx])
	}

	// Check exact match
	if v.blockedTypes[contentType] {
		return true
	}

	// Check wildcard patterns
	for _, prefix := range v.wildcardBlock {
		if strings.HasPrefix(contentType, prefix+"/") {
			return true
		}
	}

	return false
}

// ValidateContent validates the content type of a reader
// Returns a new reader that includes the buffered bytes, the detected type, and any error
func (v *ContentValidator) ValidateContent(reader io.Reader, declaredType string, size int64) (io.Reader, string, error) {
	if v == nil || !v.config.CheckMagicBytes {
		return reader, declaredType, nil
	}

	// Read first 512 bytes for magic byte detection
	buf := make([]byte, 512)
	n, err := io.ReadFull(reader, buf)
	if err != nil && err != io.ErrUnexpectedEOF && err != io.EOF {
		return nil, "", fmt.Errorf("failed to read content for validation: %w", err)
	}

	// Handle small files
	if n == 0 {
		if v.config.StrictMode {
			return nil, "", &ValidationError{
				Code:         "empty_content",
				Message:      "Cannot validate empty content",
				DetectedType: "",
				DeclaredType: declaredType,
			}
		}
		return bytes.NewReader(buf[:n]), declaredType, nil
	}

	// Detect content type using magic bytes
	detectedType := http.DetectContentType(buf[:n])

	// Normalize detected type
	if idx := strings.Index(detectedType, ";"); idx != -1 {
		detectedType = strings.TrimSpace(detectedType[:idx])
	}

	// Check if type is blocked (highest priority)
	if v.isTypeBlocked(detectedType) {
		return nil, detectedType, &ValidationError{
			Code:         "content_type_blocked",
			Message:      fmt.Sprintf("File type %s is blocked", detectedType),
			DetectedType: detectedType,
			DeclaredType: declaredType,
		}
	}

	// Check if type is allowed
	if !v.isTypeAllowed(detectedType) {
		return nil, detectedType, &ValidationError{
			Code:         "content_type_rejected",
			Message:      fmt.Sprintf("File type %s is not allowed", detectedType),
			DetectedType: detectedType,
			DeclaredType: declaredType,
		}
	}

	// Create a new reader that includes the buffered bytes
	combinedReader := io.MultiReader(bytes.NewReader(buf[:n]), reader)

	return combinedReader, detectedType, nil
}

// ValidateContentType validates a content type without reading content
func (v *ContentValidator) ValidateContentType(contentType string) error {
	if v == nil {
		return nil
	}

	if v.isTypeBlocked(contentType) {
		return &ValidationError{
			Code:         "content_type_blocked",
			Message:      fmt.Sprintf("File type %s is blocked", contentType),
			DetectedType: contentType,
		}
	}

	if !v.isTypeAllowed(contentType) {
		return &ValidationError{
			Code:         "content_type_rejected",
			Message:      fmt.Sprintf("File type %s is not allowed", contentType),
			DetectedType: contentType,
		}
	}

	return nil
}

// WriteValidationError writes a validation error response
func WriteValidationError(w http.ResponseWriter, err *ValidationError) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusUnsupportedMediaType)
	_ = json.NewEncoder(w).Encode(err)
}

// ValidateUploadContent is a helper function for validating upload content
func ValidateUploadContent(r *http.Request, reader io.Reader, declaredType string, size int64) (io.Reader, string, error) {
	validator := GetContentValidator()
	if validator == nil || !validator.config.CheckMagicBytes {
		return reader, declaredType, nil
	}

	newReader, detectedType, err := validator.ValidateContent(reader, declaredType, size)
	if err != nil {
		// Log validation failure to audit
		jid := r.Header.Get("X-User-JID")
		fileName := r.Header.Get("X-File-Name")
		if fileName == "" {
			fileName = "unknown"
		}

		var reason string
		if validErr, ok := err.(*ValidationError); ok {
			reason = validErr.Code
		} else {
			reason = err.Error()
		}

		AuditValidationFailure(r, jid, fileName, declaredType, detectedType, reason)

		return nil, detectedType, err
	}

	return newReader, detectedType, nil
}

// DefaultValidationConfig returns default validation configuration
func DefaultValidationConfig() ValidationConfig {
	return ValidationConfig{
		CheckMagicBytes: false,
		AllowedTypes: []string{
			"image/*",
			"video/*",
			"audio/*",
			"application/pdf",
			"text/plain",
			"text/html",
			"application/json",
			"application/xml",
			"application/zip",
			"application/x-gzip",
			"application/x-tar",
			"application/x-7z-compressed",
			"application/vnd.openxmlformats-officedocument.*",
			"application/vnd.oasis.opendocument.*",
		},
		BlockedTypes: []string{
			"application/x-executable",
			"application/x-msdos-program",
			"application/x-msdownload",
			"application/x-dosexec",
			"application/x-sh",
			"application/x-shellscript",
		},
		MaxFileSize: "100MB",
		StrictMode:  false,
	}
}

// Extended MIME type detection for better accuracy
var customMagicBytes = map[string][]byte{
	"application/x-executable":    {0x7f, 'E', 'L', 'F'},                            // ELF
	"application/x-msdos-program": {0x4d, 0x5a},                                     // MZ (DOS/Windows)
	"application/pdf":             {0x25, 0x50, 0x44, 0x46},                         // %PDF
	"application/zip":             {0x50, 0x4b, 0x03, 0x04},                         // PK
	"image/png":                   {0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a}, // PNG
	"image/jpeg":                  {0xff, 0xd8, 0xff},                               // JPEG
	"image/gif":                   {0x47, 0x49, 0x46, 0x38},                         // GIF8
	"image/webp":                  {0x52, 0x49, 0x46, 0x46},                         // RIFF (WebP starts with RIFF)
	"video/mp4":                   {0x00, 0x00, 0x00},                               // MP4 (variable, check ftyp)
	"audio/mpeg":                  {0xff, 0xfb},                                     // MP3
	"audio/ogg":                   {0x4f, 0x67, 0x67, 0x53},                         // OggS
}

// DetectContentTypeExtended provides extended content type detection
func DetectContentTypeExtended(data []byte) string {
	// First try standard detection
	detected := http.DetectContentType(data)

	// If generic, try custom detection
	if detected == "application/octet-stream" {
		for mimeType, magic := range customMagicBytes {
			if len(data) >= len(magic) && bytes.Equal(data[:len(magic)], magic) {
				return mimeType
			}
		}
	}

	return detected
}
