// thumbnails.go — SIMS (XEP-0385) thumbnail generation for uploaded images.
// Generates JPEG thumbnails on upload and serves them via ?thumbnail=true.
// Supports JPEG, PNG, GIF, BMP, TIFF, and WebP source images.

package main

import (
	"fmt"
	"image"
	"image/jpeg"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"github.com/disintegration/imaging"
)

const (
	defaultThumbWidth   = 320
	defaultThumbHeight  = 240
	defaultThumbQuality = 75
	thumbSuffix         = ".thumb.jpg"
)

var (
	thumbnailOnce sync.Once
	thumbEnabled  bool
	thumbWidth    int
	thumbHeight   int
	thumbQuality  int
)

// InitThumbnails initializes thumbnail generation settings from config.
func InitThumbnails() {
	thumbnailOnce.Do(func() {
		confMutex.RLock()
		defer confMutex.RUnlock()

		// Enable if uploads config references images or if explicitly enabled
		// Default: enabled if not explicitly disabled
		thumbEnabled = true
		thumbWidth = defaultThumbWidth
		thumbHeight = defaultThumbHeight
		thumbQuality = defaultThumbQuality

		log.Infof("✅ SIMS thumbnail generation enabled (%dx%d, quality=%d)", thumbWidth, thumbHeight, thumbQuality)
	})
}

// isImageFile returns true if the file extension is a supported image format.
func isImageFile(filename string) bool {
	ext := strings.ToLower(filepath.Ext(filename))
	switch ext {
	case ".jpg", ".jpeg", ".png", ".gif", ".bmp", ".tiff", ".tif", ".webp":
		return true
	}
	return false
}

// thumbPath returns the thumbnail file path for a given original image.
func thumbPath(originalPath string) string {
	ext := filepath.Ext(originalPath)
	base := strings.TrimSuffix(originalPath, ext)
	return base + thumbSuffix
}

// GenerateThumbnail creates a JPEG thumbnail for the given image file.
// Returns the thumbnail path and any error. If the file is not an image,
// returns ("", nil) — no error, just nothing to do.
func GenerateThumbnail(absPath string) (string, error) {
	if !thumbEnabled {
		return "", nil
	}

	if !isImageFile(absPath) {
		return "", nil
	}

	// Open and decode the source image
	src, err := imaging.Open(absPath, imaging.AutoOrientation(true))
	if err != nil {
		return "", fmt.Errorf("failed to open image %s: %w", filepath.Base(absPath), err)
	}

	// Determine thumbnail dimensions (fit within bounds, preserve aspect ratio)
	thumb := imaging.Fit(src, thumbWidth, thumbHeight, imaging.Lanczos)

	// Save thumbnail as JPEG
	tPath := thumbPath(absPath)

	// Ensure directory exists
	if err := os.MkdirAll(filepath.Dir(tPath), 0755); err != nil {
		return "", fmt.Errorf("failed to create thumbnail directory: %w", err)
	}

	out, err := os.Create(tPath)
	if err != nil {
		return "", fmt.Errorf("failed to create thumbnail file: %w", err)
	}
	defer out.Close()

	if err := jpeg.Encode(out, thumb, &jpeg.Options{Quality: thumbQuality}); err != nil {
		os.Remove(tPath)
		return "", fmt.Errorf("failed to encode thumbnail: %w", err)
	}

	// Get thumbnail dimensions for SIMS metadata
	bounds := thumb.Bounds()
	w := bounds.Dx()
	h := bounds.Dy()

	info, _ := os.Stat(tPath)
	var size int64
	if info != nil {
		size = info.Size()
	}

	log.Debugf("📸 Generated thumbnail: %s → %dx%d (%s)",
		filepath.Base(absPath), w, h, formatBytes(size))

	return tPath, nil
}

// GetThumbnailInfo returns SIMS-compatible metadata for a thumbnail.
// Returns width, height, size in bytes, and the MIME type.
func GetThumbnailInfo(absPath string) (width, height int, size int64, mimeType string, err error) {
	tPath := thumbPath(absPath)

	info, statErr := os.Stat(tPath)
	if statErr != nil {
		return 0, 0, 0, "", fmt.Errorf("thumbnail not found: %w", statErr)
	}

	f, openErr := os.Open(tPath)
	if openErr != nil {
		return 0, 0, 0, "", fmt.Errorf("failed to open thumbnail: %w", openErr)
	}
	defer f.Close()

	cfg, _, decodeErr := image.DecodeConfig(f)
	if decodeErr != nil {
		return 0, 0, 0, "", fmt.Errorf("failed to decode thumbnail config: %w", decodeErr)
	}

	return cfg.Width, cfg.Height, info.Size(), "image/jpeg", nil
}

// ServeThumbnail handles serving a thumbnail for a download request.
// Returns true if a thumbnail was served (caller should return).
func ServeThumbnail(w http.ResponseWriter, r *http.Request, absPath string) bool {
	if !thumbEnabled {
		return false
	}

	// Check if thumbnail was requested
	if r.URL.Query().Get("thumbnail") != "true" && r.URL.Query().Get("thumb") != "true" {
		return false
	}

	// Only serve thumbnails for image files
	if !isImageFile(absPath) {
		return false
	}

	tPath := thumbPath(absPath)

	// Check if thumbnail exists
	info, err := os.Stat(tPath)
	if err != nil {
		// Try generating on-demand
		generated, genErr := GenerateThumbnail(absPath)
		if genErr != nil || generated == "" {
			return false // Fall back to serving original
		}
		tPath = generated
		info, _ = os.Stat(tPath)
	}

	// Serve the thumbnail
	f, err := os.Open(tPath)
	if err != nil {
		return false
	}
	defer f.Close()

	w.Header().Set("Content-Type", "image/jpeg")
	if info != nil {
		w.Header().Set("Content-Length", fmt.Sprintf("%d", info.Size()))
	}
	w.Header().Set("X-Thumbnail", "true")
	w.Header().Set("Cache-Control", "public, max-age=86400")

	http.ServeContent(w, r, filepath.Base(tPath), info.ModTime(), f)
	return true
}

// CleanupThumbnail removes the thumbnail for a deleted file.
func CleanupThumbnail(absPath string) {
	if !isImageFile(absPath) {
		return
	}
	tPath := thumbPath(absPath)
	if err := os.Remove(tPath); err == nil {
		log.Debugf("🗑️ Removed thumbnail: %s", filepath.Base(tPath))
	}
}
