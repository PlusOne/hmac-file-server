package main

import (
	"mime"
	"path/filepath"
	"strings"
)

// Enhanced MIME type mappings for better file type support
// These supplement Go's built-in mime.TypeByExtension()
var enhancedMimeTypes = map[string]string{
	// Audio formats
	".m4a":  "audio/mp4",
	".flac": "audio/flac",
	".ogg":  "audio/ogg",
	".opus": "audio/opus",
	".aac":  "audio/aac",
	".wma":  "audio/x-ms-wma",
	".amr":  "audio/amr",

	// Video formats
	".webm": "video/webm",
	".mkv":  "video/x-matroska",
	".m4v":  "video/x-m4v",
	".3gp":  "video/3gpp",
	".asf":  "video/x-ms-asf",
	".wmv":  "video/x-ms-wmv",
	".flv":  "video/x-flv",

	// Archive formats
	".7z":  "application/x-7z-compressed",
	".rar": "application/vnd.rar",
	".tar": "application/x-tar",
	".bz2": "application/x-bzip2",
	".xz":  "application/x-xz",
	".lz4": "application/x-lz4",
	".zst": "application/zstd",

	// Document formats
	".epub": "application/epub+zip",
	".mobi": "application/x-mobipocket-ebook",
	".docx": "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
	".xlsx": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
	".pptx": "application/vnd.openxmlformats-officedocument.presentationml.presentation",
	".odt":  "application/vnd.oasis.opendocument.text",
	".ods":  "application/vnd.oasis.opendocument.spreadsheet",
	".odp":  "application/vnd.oasis.opendocument.presentation",

	// Package formats
	".apk": "application/vnd.android.package-archive",
	".deb": "application/vnd.debian.binary-package",
	".rpm": "application/x-rpm",
	".dmg": "application/x-apple-diskimage",
	".msi": "application/x-ms-installer",
	".pkg": "application/x-apple-package",

	// Disk image formats
	".iso":   "application/x-cd-image",
	".img":   "application/x-raw-disk-image",
	".vdi":   "application/x-virtualbox-vdi",
	".vmdk":  "application/x-vmware-vmdk",
	".qcow2": "application/x-qemu-disk",

	// Programming formats
	".py":    "text/x-python",
	".go":    "text/x-go",
	".rs":    "text/x-rust",
	".php":   "application/x-php",
	".pl":    "text/x-perl",
	".rb":    "text/x-ruby",
	".swift": "text/x-swift",
	".kt":    "text/x-kotlin",
	".scala": "text/x-scala",
	".r":     "text/x-r",
	".sql":   "application/sql",
	".toml":  "application/toml",
	".yaml":  "application/x-yaml",
	".yml":   "application/x-yaml",

	// Configuration formats
	".ini":  "text/plain",
	".conf": "text/plain",
	".cfg":  "text/plain",
	".env":  "text/plain",

	// Font formats
	".woff":  "font/woff",
	".woff2": "font/woff2",
	".eot":   "application/vnd.ms-fontobject",
	".ttf":   "font/ttf",
	".otf":   "font/otf",

	// 3D and CAD formats
	".stl":  "model/stl",
	".obj":  "model/obj",
	".ply":  "model/ply",
	".3mf":  "model/3mf",
	".step": "model/step",
	".dwg":  "image/vnd.dwg",

	// Backup and database formats
	".bak":     "application/x-backup",
	".db":      "application/x-sqlite3",
	".sqlite":  "application/x-sqlite3",
	".sqlite3": "application/x-sqlite3",
	".mdb":     "application/x-msaccess",
}

// GetContentType returns the appropriate MIME type for a file
// This function supplements Go's mime.TypeByExtension() with additional mappings
func GetContentType(filename string) string {
	ext := strings.ToLower(filepath.Ext(filename))

	// First try Go's built-in MIME detection
	if mimeType := mime.TypeByExtension(ext); mimeType != "" {
		return mimeType
	}

	// Try our enhanced mappings
	if mimeType, found := enhancedMimeTypes[ext]; found {
		return mimeType
	}

	// Fallback to generic binary type
	return "application/octet-stream"
}

// GetContentTypeWithFallback is the same as GetContentType but with explicit fallback
func GetContentTypeWithFallback(filename, fallback string) string {
	if contentType := GetContentType(filename); contentType != "application/octet-stream" {
		return contentType
	}
	if fallback != "" {
		return fallback
	}
	return "application/octet-stream"
}
