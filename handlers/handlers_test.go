package handlers

import (
	"bytes"
	"io"
	"mime/multipart"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestUploadFileHandler(t *testing.T) {
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

	if status := rr.Code; status != http.StatusCreated {
		t.Errorf("Handler returned wrong status code: got %v want %v", status, http.StatusCreated)
	}
	if rr.Body.String() != "File uploaded successfully" {
		t.Errorf("Handler returned unexpected body: got %v", rr.Body.String())
	}
}