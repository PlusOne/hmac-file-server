package handlers

import (
	"bytes"
	"io"
	"mime/multipart"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/renz/hmac-file-server/config"
)

func TestUploadFileHandler(t *testing.T) {
	t.Log("Starting TestUploadFileHandler")

	body := &bytes.Buffer{}
	writer := multipart.NewWriter(body)

	part, err := writer.CreateFormFile("file", "test.txt")
	if err != nil {
		t.Fatalf("Error creating form file: %v", err)
	}
	io.WriteString(part, "Sample file content")
	writer.Close()

	req, err := http.NewRequest("PUT", "/uploads/test.txt", body)
	if err != nil {
		t.Fatalf("Error creating request: %v", err)
	}
	req.Header.Set("Content-Type", writer.FormDataContentType())
	req.Header.Set("Authorization", "Bearer dummyToken")

	rr := httptest.NewRecorder()
	handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		UploadFileHandler(w, r, nil) // Pass nil or appropriate config if needed
	})
	handler.ServeHTTP(rr, req)

	t.Logf("Response Status Code: %d", rr.Code)
	t.Logf("Response Body: %s", rr.Body.String())

	if status := rr.Code; status != http.StatusCreated {
		t.Errorf("Handler returned wrong status code: got %v want %v", status, http.StatusCreated)
	}
	if rr.Body.String() != "File uploaded successfully" {
		t.Errorf("Handler returned unexpected body: got %v", rr.Body.String())
	}

	t.Log("Completed TestUploadFileHandler")
}

func TestUploadHandler(t *testing.T) {
	cfg := &config.Config{
		Uploads: config.UploadsConfig{
			ResumableUploadsEnabled: true,
		},
	}
	h := NewHandler(cfg)

	req, err := http.NewRequest("POST", "/upload", nil)
	if err != nil {
		t.Fatal(err)
	}
	rr := httptest.NewRecorder()
	handler := http.HandlerFunc(h.UploadHandler)
	handler.ServeHTTP(rr, req)

	// Check the status code
	if status := rr.Code; status != http.StatusOK {
		t.Errorf("handler returned wrong status code: got %v want %v",
			status, http.StatusOK)
	}

	// Add more assertions as needed
}

func TestDownloadHandler(t *testing.T) {
	cfg := &config.Config{
		Downloads: config.DownloadsConfig{
			ChunkedDownloadsEnabled: true,
		},
	}
	h := NewHandler(cfg)

	req, err := http.NewRequest("GET", "/download", nil)
	if err != nil {
		t.Fatal(err)
	}
	rr := httptest.NewRecorder()
	handler := http.HandlerFunc(h.DownloadHandler)
	handler.ServeHTTP(rr, req)

	// Check the status code
	if status := rr.Code; status != http.StatusOK {
		t.Errorf("handler returned wrong status code: got %v want %v",
			status, http.StatusOK)
	}

	// Add more assertions as needed
}