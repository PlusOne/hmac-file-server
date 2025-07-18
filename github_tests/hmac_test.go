package main

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"mime"
	"net/http"
	"net/url"
	"os"
	"path/filepath" // Added this import for filepath usage
	"strconv"
	"testing"
)

const (
	serverURL    = "http://[::1]:8080"				// Replace with your actual server URL
	secret       = "hmac-file-server-is-the-win"             // Replace with your HMAC secret key
	uploadPath   = "hmac_icon.png"                 			// Test file to upload
	protocolType = "v2"						// Use v2, v, or token as needed
)

// TestUpload performs a basic HMAC validation and upload test.
func TestUpload(t *testing.T) {
	// File setup for testing
	file, err := os.Open(uploadPath)
	if err != nil {
		t.Fatalf("Error opening file: %v", err)
	}
	defer file.Close()

	fileInfo, _ := file.Stat()
	fileStorePath := uploadPath
	contentLength := fileInfo.Size()

	// Generate HMAC based on protocol type
	hmacValue := generateHMAC(fileStorePath, contentLength, protocolType)

	// Formulate request URL with HMAC in query params
	reqURL := fmt.Sprintf("%s/%s?%s=%s", serverURL, fileStorePath, protocolType, url.QueryEscape(hmacValue))

	// Prepare HTTP PUT request with file data
	req, err := http.NewRequest(http.MethodPut, reqURL, file)
	if err != nil {
		t.Fatalf("Error creating request: %v", err)
	}
	req.Header.Set("Content-Type", "application/octet-stream")
	req.Header.Set("Content-Length", strconv.FormatInt(contentLength, 10))

	// Execute HTTP request
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatalf("Error executing request: %v", err)
	}
	defer resp.Body.Close()

	t.Logf("Response status: %s", resp.Status)
}

// Generates the HMAC based on your protocol version
func generateHMAC(filePath string, contentLength int64, protocol string) string {
	mac := hmac.New(sha256.New, []byte(secret))
	macString := ""

	// Calculate HMAC according to protocol
	if protocol == "v" {
		mac.Write([]byte(filePath + "\x20" + strconv.FormatInt(contentLength, 10)))
		macString = hex.EncodeToString(mac.Sum(nil))
	} else if protocol == "v2" || protocol == "token" {
		contentType := mime.TypeByExtension(filepath.Ext(filePath))
		if contentType == "" {
			contentType = "application/octet-stream"
		}
		mac.Write([]byte(filePath + "\x00" + strconv.FormatInt(contentLength, 10) + "\x00" + contentType))
		macString = hex.EncodeToString(mac.Sum(nil))
	}

	return macString
}
