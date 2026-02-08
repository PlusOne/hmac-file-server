// Package utils provides shared utility functions.
package utils

import (
	"fmt"
	"mime"
	"net/http"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// ParseSize converts a human-readable size string (e.g. "10MB") to bytes.
func ParseSize(sizeStr string) (int64, error) {
	sizeStr = strings.TrimSpace(sizeStr)
	if len(sizeStr) < 2 {
		return 0, fmt.Errorf("invalid size string: %s", sizeStr)
	}

	unit := strings.ToUpper(sizeStr[len(sizeStr)-2:])
	valueStr := sizeStr[:len(sizeStr)-2]
	value, err := strconv.Atoi(valueStr)
	if err != nil {
		return 0, fmt.Errorf("invalid size value: %v", err)
	}

	switch unit {
	case "KB":
		return int64(value) * 1024, nil
	case "MB":
		return int64(value) * 1024 * 1024, nil
	case "GB":
		return int64(value) * 1024 * 1024 * 1024, nil
	default:
		return 0, fmt.Errorf("unknown size unit: %s", unit)
	}
}

// ParseTTL converts a human-readable TTL string to a time.Duration.
func ParseTTL(ttlStr string) (time.Duration, error) {
	ttlStr = strings.ToLower(strings.TrimSpace(ttlStr))
	if ttlStr == "" {
		return 0, fmt.Errorf("TTL string cannot be empty")
	}
	var valueStr string
	var unit rune
	for _, r := range ttlStr {
		if r >= '0' && r <= '9' {
			valueStr += string(r)
		} else {
			unit = r
			break
		}
	}
	val, err := strconv.Atoi(valueStr)
	if err != nil {
		return 0, fmt.Errorf("invalid TTL value: %v", err)
	}
	switch unit {
	case 's':
		return time.Duration(val) * time.Second, nil
	case 'm':
		return time.Duration(val) * time.Minute, nil
	case 'h':
		return time.Duration(val) * time.Hour, nil
	case 'd':
		return time.Duration(val) * 24 * time.Hour, nil
	case 'w':
		return time.Duration(val) * 7 * 24 * time.Hour, nil
	case 'y':
		return time.Duration(val) * 365 * 24 * time.Hour, nil
	default:
		return 0, fmt.Errorf("unknown TTL unit: %c", unit)
	}
}

// Max returns the larger of a or b.
func Max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// GetClientIP extracts the client IP from an HTTP request, checking proxy headers.
func GetClientIP(r *http.Request) string {
	if xff := r.Header.Get("X-Forwarded-For"); xff != "" {
		parts := strings.Split(xff, ",")
		return strings.TrimSpace(parts[0])
	}
	if xri := r.Header.Get("X-Real-IP"); xri != "" {
		return xri
	}
	host, _, _ := strings.Cut(r.RemoteAddr, ":")
	return host
}

// FormatBytes converts a byte count to a human-readable string.
func FormatBytes(bytes int64) string {
	const unit = 1024
	if bytes < unit {
		return fmt.Sprintf("%d B", bytes)
	}
	div, exp := int64(unit), 0
	for n := bytes / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %ciB", float64(bytes)/float64(div), "KMGTPE"[exp])
}

// Contains checks if a string slice contains a given string.
func Contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}

// Enhanced MIME type mappings for better content type detection.
var enhancedMimeTypes = map[string]string{
	".webp": "image/webp",
	".avif": "image/avif",
	".heic": "image/heic",
	".heif": "image/heif",
	".jxl":  "image/jxl",
	".opus": "audio/opus",
	".flac": "audio/flac",
	".m4a":  "audio/mp4",
	".weba": "audio/webm",
	".webm": "video/webm",
	".mkv":  "video/x-matroska",
	".ts":   "video/mp2t",
	".m3u8": "application/x-mpegURL",
	".wasm": "application/wasm",
	".toml": "application/toml",
	".yaml": "application/x-yaml",
	".yml":  "application/x-yaml",
	".md":   "text/markdown",
	".rst":  "text/x-rst",
	".csv":  "text/csv",
	".tsv":  "text/tab-separated-values",
	".ics":  "text/calendar",
	".vcf":  "text/vcard",
	".gpg":  "application/pgp-encrypted",
	".sig":  "application/pgp-signature",
	".asc":  "application/pgp-keys",
	".enc":  "application/octet-stream",
	".pgp":  "application/pgp-encrypted",
}

// GetContentType returns the MIME type for a file based on its extension.
func GetContentType(filename string) string {
	ext := strings.ToLower(filepath.Ext(filename))
	if ct, ok := enhancedMimeTypes[ext]; ok {
		return ct
	}
	if ct := mime.TypeByExtension(ext); ct != "" {
		return ct
	}
	return "application/octet-stream"
}

// GetContentTypeWithFallback returns the MIME type with a caller-specified fallback.
func GetContentTypeWithFallback(filename, fallback string) string {
	ext := strings.ToLower(filepath.Ext(filename))
	if ct, ok := enhancedMimeTypes[ext]; ok {
		return ct
	}
	contentType := mime.TypeByExtension(ext)
	if contentType != "" {
		return contentType
	}
	if fallback != "" {
		return fallback
	}
	return "application/octet-stream"
}

// SanitizeFilePath validates and constructs an absolute path under baseDir.
func SanitizeFilePath(baseDir, filePath string) (string, error) {
	absBaseDir, err := filepath.Abs(baseDir)
	if err != nil {
		return "", err
	}
	absFilePath, err := filepath.Abs(filepath.Join(absBaseDir, filePath))
	if err != nil {
		return "", err
	}
	if !strings.HasPrefix(absFilePath, absBaseDir) {
		return "", fmt.Errorf("invalid file path: %s", filePath)
	}
	return absFilePath, nil
}
