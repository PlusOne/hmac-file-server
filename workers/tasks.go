package workers

import "time"

// UploadTask repräsentiert eine Aufgabe zum Hochladen von Dateien
type UploadTask struct {
	FileName   string
	FileSize   int64
	UploadedAt time.Time
	// Füge weitere Felder bei Bedarf hinzu
}

// ScanTask repräsentiert eine Aufgabe zum Scannen von Dateien
type ScanTask struct {
	FileName   string
	FileSize   int64
	UploadedAt time.Time
	// Füge weitere Felder bei Bedarf hinzu
}
