package main

import (
	"fmt"
	"mime"
	"path/filepath"
)

// Enhanced MIME type support with additional mappings
var customMimeTypes = map[string]string{
	".m4a":  "audio/mp4",
	".flac": "audio/flac",
	".ogg":  "audio/ogg",
	".webm": "video/webm",
	".mkv":  "video/x-matroska",
	".epub": "application/epub+zip",
	".mobi": "application/x-mobipocket-ebook",
	".apk":  "application/vnd.android.package-archive",
	".deb":  "application/vnd.debian.binary-package",
	".rpm":  "application/x-rpm",
	".dmg":  "application/x-apple-diskimage",
	".iso":  "application/x-iso9660-image",
	".tar":  "application/x-tar",
	".gz":   "application/gzip",
	".bz2":  "application/x-bzip2",
	".xz":   "application/x-xz",
	".7z":   "application/x-7z-compressed",
	".rar":  "application/vnd.rar",
	".docx": "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
	".xlsx": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
	".pptx": "application/vnd.openxmlformats-officedocument.presentationml.presentation",
}

// GetMimeType returns the MIME type for a file extension
func GetMimeType(filename string) string {
	ext := filepath.Ext(filename)
	
	// First try standard Go mime detection
	mimeType := mime.TypeByExtension(ext)
	if mimeType != "" {
		return mimeType
	}
	
	// Try custom mappings
	if customType, found := customMimeTypes[ext]; found {
		return customType
	}
	
	// Fallback to octet-stream
	return "application/octet-stream"
}

func main() {
	testFiles := []string{
		"test.jpg", "document.pdf", "archive.zip", "video.mp4",
		"audio.m4a", "book.epub", "package.deb", "disk.iso",
		"unknown.xyz", "noext", "document.docx", "video.webm",
	}
	
	fmt.Println("🔍 Enhanced MIME Type Detection:")
	fmt.Println("┌─────────────────┬────────────────────────────────────────────────┐")
	fmt.Println("│ File            │ MIME Type                                      │")
	fmt.Println("├─────────────────┼────────────────────────────────────────────────┤")
	
	for _, file := range testFiles {
		mimeType := GetMimeType(file)
		fmt.Printf("│ %-15s │ %-46s │\n", file, mimeType)
	}
	
	fmt.Println("└─────────────────┴────────────────────────────────────────────────┘")
}
