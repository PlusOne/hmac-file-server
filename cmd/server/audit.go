// audit.go - Dedicated audit logging for security-relevant events

package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/sirupsen/logrus"
	"gopkg.in/natefinch/lumberjack.v2"
)

// AuditConfig holds audit logging configuration
type AuditConfig struct {
	Enabled bool     `toml:"enabled" mapstructure:"enabled"`
	Output  string   `toml:"output" mapstructure:"output"`     // "file" | "stdout"
	Path    string   `toml:"path" mapstructure:"path"`         // Log file path
	Format  string   `toml:"format" mapstructure:"format"`     // "json" | "text"
	Events  []string `toml:"events" mapstructure:"events"`     // Events to log
	MaxSize int      `toml:"max_size" mapstructure:"max_size"` // Max size in MB
	MaxAge  int      `toml:"max_age" mapstructure:"max_age"`   // Max age in days
}

// AuditEvent types
const (
	AuditEventUpload            = "upload"
	AuditEventDownload          = "download"
	AuditEventDelete            = "delete"
	AuditEventAuthSuccess       = "auth_success"
	AuditEventAuthFailure       = "auth_failure"
	AuditEventRateLimited       = "rate_limited"
	AuditEventBanned            = "banned"
	AuditEventQuotaExceeded     = "quota_exceeded"
	AuditEventAdminAction       = "admin_action"
	AuditEventValidationFailure = "validation_failure"
)

// AuditLogger handles security audit logging
type AuditLogger struct {
	logger        *logrus.Logger
	config        *AuditConfig
	enabledEvents map[string]bool
	mutex         sync.RWMutex
}

var (
	auditLogger *AuditLogger
	auditOnce   sync.Once
)

// InitAuditLogger initializes the audit logger
func InitAuditLogger(config *AuditConfig) error {
	var initErr error
	auditOnce.Do(func() {
		auditLogger = &AuditLogger{
			logger:        logrus.New(),
			config:        config,
			enabledEvents: make(map[string]bool),
		}

		// Build enabled events map for fast lookup
		for _, event := range config.Events {
			auditLogger.enabledEvents[strings.ToLower(event)] = true
		}

		// Configure formatter
		if config.Format == "json" {
			auditLogger.logger.SetFormatter(&logrus.JSONFormatter{
				TimestampFormat: time.RFC3339,
				FieldMap: logrus.FieldMap{
					logrus.FieldKeyTime: "timestamp",
					logrus.FieldKeyMsg:  "event",
				},
			})
		} else {
			auditLogger.logger.SetFormatter(&logrus.TextFormatter{
				TimestampFormat: time.RFC3339,
				FullTimestamp:   true,
			})
		}

		// Configure output
		if !config.Enabled {
			auditLogger.logger.SetOutput(io.Discard)
			return
		}

		switch config.Output {
		case "stdout":
			auditLogger.logger.SetOutput(os.Stdout)
		case "file":
			if config.Path == "" {
				config.Path = "/var/log/hmac-audit.log"
			}

			// Ensure directory exists
			dir := filepath.Dir(config.Path)
			if err := os.MkdirAll(dir, 0755); err != nil {
				initErr = err
				return
			}

			// Use lumberjack for log rotation
			maxSize := config.MaxSize
			if maxSize <= 0 {
				maxSize = 100 // Default 100MB
			}
			maxAge := config.MaxAge
			if maxAge <= 0 {
				maxAge = 30 // Default 30 days
			}

			auditLogger.logger.SetOutput(&lumberjack.Logger{
				Filename:   config.Path,
				MaxSize:    maxSize,
				MaxAge:     maxAge,
				MaxBackups: 5,
				Compress:   true,
			})
		default:
			auditLogger.logger.SetOutput(os.Stdout)
		}

		auditLogger.logger.SetLevel(logrus.InfoLevel)
		log.Infof("Audit logger initialized: output=%s, path=%s, format=%s, events=%v",
			config.Output, config.Path, config.Format, config.Events)
	})

	return initErr
}

// GetAuditLogger returns the singleton audit logger
func GetAuditLogger() *AuditLogger {
	return auditLogger
}

// IsEventEnabled checks if an event type should be logged
func (a *AuditLogger) IsEventEnabled(event string) bool {
	if a == nil || !a.config.Enabled {
		return false
	}
	a.mutex.RLock()
	defer a.mutex.RUnlock()

	// If no events configured, log all
	if len(a.enabledEvents) == 0 {
		return true
	}
	return a.enabledEvents[strings.ToLower(event)]
}

// LogEvent logs an audit event
func (a *AuditLogger) LogEvent(event string, fields logrus.Fields) {
	if a == nil || !a.config.Enabled || !a.IsEventEnabled(event) {
		return
	}

	// Add standard fields
	fields["event_type"] = event
	if _, ok := fields["timestamp"]; !ok {
		fields["timestamp"] = time.Now().UTC().Format(time.RFC3339)
	}

	a.logger.WithFields(fields).Info(event)
}

// AuditEvent is a helper function for logging audit events from request context
func AuditEvent(event string, r *http.Request, fields logrus.Fields) {
	if auditLogger == nil || !auditLogger.config.Enabled {
		return
	}

	if !auditLogger.IsEventEnabled(event) {
		return
	}

	// Add request context
	if r != nil {
		if fields == nil {
			fields = logrus.Fields{}
		}
		fields["ip"] = getClientIP(r)
		fields["user_agent"] = r.UserAgent()
		fields["method"] = r.Method
		fields["path"] = r.URL.Path

		// Extract JID if available from headers or context
		if jid := r.Header.Get("X-User-JID"); jid != "" {
			fields["jid"] = jid
		}
	}

	auditLogger.LogEvent(event, fields)
}

// AuditUpload logs file upload events
func AuditUpload(r *http.Request, jid, fileID, fileName string, fileSize int64, contentType, result string, err error) {
	fields := logrus.Fields{
		"jid":          jid,
		"file_id":      fileID,
		"file_name":    fileName,
		"file_size":    fileSize,
		"content_type": contentType,
		"result":       result,
	}
	if err != nil {
		fields["error"] = err.Error()
	}
	AuditEvent(AuditEventUpload, r, fields)
}

// AuditDownload logs file download events
func AuditDownload(r *http.Request, jid, fileID, fileName string, fileSize int64, result string, err error) {
	fields := logrus.Fields{
		"jid":       jid,
		"file_id":   fileID,
		"file_name": fileName,
		"file_size": fileSize,
		"result":    result,
	}
	if err != nil {
		fields["error"] = err.Error()
	}
	AuditEvent(AuditEventDownload, r, fields)
}

// AuditDelete logs file deletion events
func AuditDelete(r *http.Request, jid, fileID, fileName string, result string, err error) {
	fields := logrus.Fields{
		"jid":       jid,
		"file_id":   fileID,
		"file_name": fileName,
		"result":    result,
	}
	if err != nil {
		fields["error"] = err.Error()
	}
	AuditEvent(AuditEventDelete, r, fields)
}

// AuditAuth logs authentication events
func AuditAuth(r *http.Request, jid string, success bool, method string, err error) {
	event := AuditEventAuthSuccess
	result := "success"
	if !success {
		event = AuditEventAuthFailure
		result = "failure"
	}

	fields := logrus.Fields{
		"jid":         jid,
		"auth_method": method,
		"result":      result,
	}
	if err != nil {
		fields["error"] = err.Error()
	}
	AuditEvent(event, r, fields)
}

// AuditRateLimited logs rate limiting events
func AuditRateLimited(r *http.Request, jid, reason string) {
	fields := logrus.Fields{
		"jid":    jid,
		"reason": reason,
	}
	AuditEvent(AuditEventRateLimited, r, fields)
}

// AuditBanned logs ban events
func AuditBanned(r *http.Request, jid, ip, reason string, duration time.Duration) {
	fields := logrus.Fields{
		"jid":          jid,
		"banned_ip":    ip,
		"reason":       reason,
		"ban_duration": duration.String(),
	}
	AuditEvent(AuditEventBanned, r, fields)
}

// AuditQuotaExceeded logs quota exceeded events
func AuditQuotaExceeded(r *http.Request, jid string, used, limit, requested int64) {
	fields := logrus.Fields{
		"jid":       jid,
		"used":      used,
		"limit":     limit,
		"requested": requested,
	}
	AuditEvent(AuditEventQuotaExceeded, r, fields)
}

// AuditAdminAction logs admin API actions
func AuditAdminAction(r *http.Request, action, target string, details map[string]interface{}) {
	fields := logrus.Fields{
		"action": action,
		"target": target,
	}
	for k, v := range details {
		fields[k] = v
	}
	AuditEvent(AuditEventAdminAction, r, fields)
}

// AuditValidationFailure logs content validation failures
func AuditValidationFailure(r *http.Request, jid, fileName, declaredType, detectedType, reason string) {
	fields := logrus.Fields{
		"jid":           jid,
		"file_name":     fileName,
		"declared_type": declaredType,
		"detected_type": detectedType,
		"reason":        reason,
	}
	AuditEvent(AuditEventValidationFailure, r, fields)
}

// DefaultAuditConfig returns default audit configuration
func DefaultAuditConfig() AuditConfig {
	return AuditConfig{
		Enabled: false,
		Output:  "file",
		Path:    "/var/log/hmac-audit.log",
		Format:  "json",
		Events: []string{
			AuditEventUpload,
			AuditEventDownload,
			AuditEventDelete,
			AuditEventAuthSuccess,
			AuditEventAuthFailure,
			AuditEventRateLimited,
			AuditEventBanned,
		},
		MaxSize: 100,
		MaxAge:  30,
	}
}

// AuditAuthSuccess is a helper for logging successful authentication
func AuditAuthSuccess(r *http.Request, jid, method string) {
	AuditAuth(r, jid, true, method, nil)
}

// AuditAuthFailure is a helper for logging failed authentication
func AuditAuthFailure(r *http.Request, method, errorMsg string) {
	AuditAuth(r, "", false, method, fmt.Errorf("%s", errorMsg))
}

// AuditUploadSuccess is a helper for logging successful uploads
func AuditUploadSuccess(r *http.Request, jid, fileName string, fileSize int64, contentType string) {
	AuditUpload(r, jid, "", fileName, fileSize, contentType, "success", nil)
}

// AuditUploadFailure is a helper for logging failed uploads
func AuditUploadFailure(r *http.Request, jid, fileName string, fileSize int64, errorMsg string) {
	AuditUpload(r, jid, "", fileName, fileSize, "", "failure", fmt.Errorf("%s", errorMsg))
}

// AuditDownloadSuccess is a helper for logging successful downloads
func AuditDownloadSuccess(r *http.Request, jid, fileName string, fileSize int64) {
	AuditDownload(r, jid, "", fileName, fileSize, "success", nil)
}
