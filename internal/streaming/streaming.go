// Package streaming provides progress-tracking I/O wrappers.
package streaming

import (
	"io"
	"sync/atomic"
	"time"

	"github.com/sirupsen/logrus"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// ProgressWriter wraps an io.Writer and tracks bytes written.
type ProgressWriter struct {
	writer       io.Writer
	totalBytes   int64
	writtenBytes int64
	startTime    time.Time
	filename     string
}

// NewProgressWriter creates a new ProgressWriter.
func NewProgressWriter(w io.Writer, totalBytes int64, filename string) *ProgressWriter {
	return &ProgressWriter{
		writer:     w,
		totalBytes: totalBytes,
		startTime:  time.Now(),
		filename:   filename,
	}
}

// Write implements io.Writer with progress tracking.
func (pw *ProgressWriter) Write(p []byte) (int, error) {
	n, err := pw.writer.Write(p)
	atomic.AddInt64(&pw.writtenBytes, int64(n))
	return n, err
}

// WrittenBytes returns the number of bytes written so far.
func (pw *ProgressWriter) WrittenBytes() int64 {
	return atomic.LoadInt64(&pw.writtenBytes)
}

// CopyWithProgress copies data with progress reporting.
func CopyWithProgress(dst io.Writer, src io.Reader, totalSize int64, filename string) (int64, error) {
	pw := NewProgressWriter(dst, totalSize, filename)

	buf := make([]byte, 32*1024)
	written, err := io.CopyBuffer(pw, src, buf)
	if err != nil {
		return written, err
	}

	elapsed := time.Since(pw.startTime)
	if elapsed > 0 {
		speed := float64(written) / elapsed.Seconds() / 1024 / 1024
		log.Infof("Transfer complete: %s (%.2f MB, %.2f MB/s)", filename, float64(written)/1024/1024, speed)
	}

	return written, nil
}

// CopyWithProgressTracking copies data with progress logging at intervals.
func CopyWithProgressTracking(dst io.Writer, src io.Reader, totalSize int64, filename string) (int64, error) {
	pw := NewProgressWriter(dst, totalSize, filename)

	done := make(chan struct{})
	go func() {
		ticker := time.NewTicker(5 * time.Second)
		defer ticker.Stop()
		for {
			select {
			case <-ticker.C:
				written := pw.WrittenBytes()
				if totalSize > 0 {
					pct := float64(written) / float64(totalSize) * 100
					elapsed := time.Since(pw.startTime)
					speed := float64(written) / elapsed.Seconds() / 1024 / 1024
					log.Infof("Progress: %s - %.1f%% (%.2f MB/s)", filename, pct, speed)
				}
			case <-done:
				return
			}
		}
	}()

	buf := make([]byte, 32*1024)
	written, err := io.CopyBuffer(pw, src, buf)
	close(done)

	return written, err
}
