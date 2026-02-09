// metadata_store.go — SQLite-backed file metadata tracking.
// Records every upload with JID, filename, size, content type, timestamps,
// and download counts. Provides query functions for the admin dashboard.

package main

import (
	"context"
	"database/sql"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"time"

	_ "modernc.org/sqlite"
)

// MetadataStore manages file metadata persistence via SQLite.
type MetadataStore struct {
	db     *sql.DB
	mu     sync.Mutex
	dbPath string
}

// StoredFileMetadata represents a tracked file's metadata.
type StoredFileMetadata struct {
	ID            int64     `json:"id"`
	FilePath      string    `json:"file_path"`
	FileName      string    `json:"file_name"`
	FileSize      int64     `json:"file_size"`
	ContentType   string    `json:"content_type"`
	UploaderJID   string    `json:"uploader_jid"`
	UploaderIP    string    `json:"uploader_ip"`
	UploadTime    time.Time `json:"upload_time"`
	LastAccess    time.Time `json:"last_access"`
	DownloadCount int64     `json:"download_count"`
	Checksum      string    `json:"checksum,omitempty"`
	Deleted       bool      `json:"deleted"`
}

// MetadataQuery filters for querying metadata.
type MetadataQuery struct {
	JID         string
	ContentType string
	MinSize     int64
	MaxSize     int64
	Since       time.Time
	Until       time.Time
	Deleted     *bool
	Limit       int
	Offset      int
	OrderBy     string // "upload_time", "file_size", "download_count"
	OrderDir    string // "ASC", "DESC"
}

// MetadataStats holds aggregate statistics from the metadata store.
type MetadataStats struct {
	TotalFiles     int64            `json:"total_files"`
	TotalSize      int64            `json:"total_size"`
	TotalDownloads int64            `json:"total_downloads"`
	UniqueUsers    int64            `json:"unique_users"`
	ContentTypes   map[string]int64 `json:"content_types"`
	TopUploaders   []UploaderStat   `json:"top_uploaders"`
}

// UploaderStat represents per-user upload statistics.
type UploaderStat struct {
	JID       string `json:"jid"`
	FileCount int64  `json:"file_count"`
	TotalSize int64  `json:"total_size"`
}

var (
	metadataStore     *MetadataStore
	metadataStoreLock sync.Mutex
)

// InitMetadataStore initializes the SQLite metadata store.
func InitMetadataStore(dbPath string) error {
	metadataStoreLock.Lock()
	defer metadataStoreLock.Unlock()

	if metadataStore != nil {
		return nil // already initialized
	}

	// Ensure directory exists
	dir := filepath.Dir(dbPath)
	if err := os.MkdirAll(dir, 0750); err != nil {
		return fmt.Errorf("failed to create metadata directory %s: %w", dir, err)
	}

	db, err := sql.Open("sqlite", dbPath+"?_journal=WAL&_busy_timeout=5000")
	if err != nil {
		return fmt.Errorf("failed to open metadata database: %w", err)
	}

	// Connection pool settings for SQLite
	db.SetMaxOpenConns(1) // SQLite supports only 1 writer
	db.SetMaxIdleConns(2)
	db.SetConnMaxLifetime(0) // Keep connections alive

	store := &MetadataStore{
		db:     db,
		dbPath: dbPath,
	}

	if err := store.migrate(); err != nil {
		db.Close()
		return fmt.Errorf("failed to migrate metadata database: %w", err)
	}

	metadataStore = store
	log.Infof("✅ SQLite metadata store initialized at %s", dbPath)
	return nil
}

// GetMetadataStore returns the global metadata store (may be nil if disabled).
func GetMetadataStore() *MetadataStore {
	return metadataStore
}

// migrate creates or updates the database schema.
func (m *MetadataStore) migrate() error {
	schema := `
	CREATE TABLE IF NOT EXISTS file_metadata (
		id             INTEGER PRIMARY KEY AUTOINCREMENT,
		file_path      TEXT NOT NULL UNIQUE,
		file_name      TEXT NOT NULL,
		file_size      INTEGER NOT NULL DEFAULT 0,
		content_type   TEXT NOT NULL DEFAULT 'application/octet-stream',
		uploader_jid   TEXT NOT NULL DEFAULT '',
		uploader_ip    TEXT NOT NULL DEFAULT '',
		upload_time    DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
		last_access    DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
		download_count INTEGER NOT NULL DEFAULT 0,
		checksum       TEXT NOT NULL DEFAULT '',
		deleted        INTEGER NOT NULL DEFAULT 0
	);

	CREATE INDEX IF NOT EXISTS idx_file_metadata_jid        ON file_metadata(uploader_jid);
	CREATE INDEX IF NOT EXISTS idx_file_metadata_upload_time ON file_metadata(upload_time);
	CREATE INDEX IF NOT EXISTS idx_file_metadata_content     ON file_metadata(content_type);
	CREATE INDEX IF NOT EXISTS idx_file_metadata_deleted     ON file_metadata(deleted);
	CREATE INDEX IF NOT EXISTS idx_file_metadata_size        ON file_metadata(file_size);

	CREATE TABLE IF NOT EXISTS schema_version (
		version INTEGER NOT NULL
	);
	`

	_, err := m.db.Exec(schema)
	if err != nil {
		return fmt.Errorf("schema migration failed: %w", err)
	}

	// Set version if not exists
	var count int
	_ = m.db.QueryRow("SELECT COUNT(*) FROM schema_version").Scan(&count)
	if count == 0 {
		_, _ = m.db.Exec("INSERT INTO schema_version (version) VALUES (?)", 1)
	}

	return nil
}

// RecordUpload records metadata for a newly uploaded file.
func (m *MetadataStore) RecordUpload(ctx context.Context, filePath, fileName string, fileSize int64, contentType, uploaderJID, uploaderIP, checksum string) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	now := time.Now().UTC()

	_, err := m.db.ExecContext(ctx, `
		INSERT INTO file_metadata (file_path, file_name, file_size, content_type, uploader_jid, uploader_ip, upload_time, last_access, checksum)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
		ON CONFLICT(file_path) DO UPDATE SET
			file_size = excluded.file_size,
			content_type = excluded.content_type,
			uploader_jid = excluded.uploader_jid,
			uploader_ip = excluded.uploader_ip,
			upload_time = excluded.upload_time,
			last_access = excluded.last_access,
			checksum = excluded.checksum,
			deleted = 0
	`, filePath, fileName, fileSize, contentType, uploaderJID, uploaderIP, now, now, checksum)

	if err != nil {
		return fmt.Errorf("failed to record upload metadata: %w", err)
	}

	log.Debugf("📝 Metadata recorded: %s (%s, %s, %d bytes)", fileName, uploaderJID, contentType, fileSize)
	return nil
}

// RecordDownload increments the download count and updates last access time.
func (m *MetadataStore) RecordDownload(ctx context.Context, filePath string) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	result, err := m.db.ExecContext(ctx, `
		UPDATE file_metadata
		SET download_count = download_count + 1, last_access = ?
		WHERE file_path = ? AND deleted = 0
	`, time.Now().UTC(), filePath)

	if err != nil {
		return fmt.Errorf("failed to record download: %w", err)
	}

	rows, _ := result.RowsAffected()
	if rows == 0 {
		// File not tracked — ignore silently (may be from before metadata store was enabled)
		return nil
	}

	return nil
}

// RecordDeletion marks a file as deleted in the metadata store.
func (m *MetadataStore) RecordDeletion(ctx context.Context, filePath string) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	_, err := m.db.ExecContext(ctx, `
		UPDATE file_metadata SET deleted = 1 WHERE file_path = ?
	`, filePath)

	if err != nil {
		return fmt.Errorf("failed to record deletion: %w", err)
	}

	return nil
}

// GetStoredFileMetadata returns metadata for a specific file.
func (m *MetadataStore) GetStoredFileMetadata(ctx context.Context, filePath string) (*StoredFileMetadata, error) {
	row := m.db.QueryRowContext(ctx, `
		SELECT id, file_path, file_name, file_size, content_type,
		       uploader_jid, uploader_ip, upload_time, last_access,
		       download_count, checksum, deleted
		FROM file_metadata
		WHERE file_path = ?
	`, filePath)

	meta := &StoredFileMetadata{}
	var deleted int
	err := row.Scan(
		&meta.ID, &meta.FilePath, &meta.FileName, &meta.FileSize,
		&meta.ContentType, &meta.UploaderJID, &meta.UploaderIP,
		&meta.UploadTime, &meta.LastAccess, &meta.DownloadCount,
		&meta.Checksum, &deleted,
	)
	if err == sql.ErrNoRows {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("failed to get file metadata: %w", err)
	}

	meta.Deleted = deleted != 0
	return meta, nil
}

// QueryFiles returns files matching the given query.
func (m *MetadataStore) QueryFiles(ctx context.Context, q MetadataQuery) ([]StoredFileMetadata, int64, error) {
	// Build WHERE clause
	where := "1=1"
	args := []interface{}{}

	if q.JID != "" {
		where += " AND uploader_jid = ?"
		args = append(args, q.JID)
	}
	if q.ContentType != "" {
		where += " AND content_type LIKE ?"
		args = append(args, q.ContentType+"%")
	}
	if q.MinSize > 0 {
		where += " AND file_size >= ?"
		args = append(args, q.MinSize)
	}
	if q.MaxSize > 0 {
		where += " AND file_size <= ?"
		args = append(args, q.MaxSize)
	}
	if !q.Since.IsZero() {
		where += " AND upload_time >= ?"
		args = append(args, q.Since)
	}
	if !q.Until.IsZero() {
		where += " AND upload_time <= ?"
		args = append(args, q.Until)
	}
	if q.Deleted != nil {
		if *q.Deleted {
			where += " AND deleted = 1"
		} else {
			where += " AND deleted = 0"
		}
	}

	// Count total
	var total int64
	countQuery := "SELECT COUNT(*) FROM file_metadata WHERE " + where
	if err := m.db.QueryRowContext(ctx, countQuery, args...).Scan(&total); err != nil {
		return nil, 0, fmt.Errorf("failed to count files: %w", err)
	}

	// Order
	orderBy := "upload_time"
	orderDir := "DESC"
	switch q.OrderBy {
	case "file_size", "download_count", "last_access", "file_name":
		orderBy = q.OrderBy
	}
	if q.OrderDir == "ASC" {
		orderDir = "ASC"
	}

	// Limit / offset
	limit := 50
	if q.Limit > 0 && q.Limit <= 1000 {
		limit = q.Limit
	}
	offset := 0
	if q.Offset > 0 {
		offset = q.Offset
	}

	query := fmt.Sprintf(`
		SELECT id, file_path, file_name, file_size, content_type,
		       uploader_jid, uploader_ip, upload_time, last_access,
		       download_count, checksum, deleted
		FROM file_metadata
		WHERE %s
		ORDER BY %s %s
		LIMIT ? OFFSET ?
	`, where, orderBy, orderDir)

	args = append(args, limit, offset)

	rows, err := m.db.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, 0, fmt.Errorf("failed to query files: %w", err)
	}
	defer rows.Close()

	var results []StoredFileMetadata
	for rows.Next() {
		var meta StoredFileMetadata
		var deleted int
		if err := rows.Scan(
			&meta.ID, &meta.FilePath, &meta.FileName, &meta.FileSize,
			&meta.ContentType, &meta.UploaderJID, &meta.UploaderIP,
			&meta.UploadTime, &meta.LastAccess, &meta.DownloadCount,
			&meta.Checksum, &deleted,
		); err != nil {
			return nil, 0, fmt.Errorf("failed to scan file row: %w", err)
		}
		meta.Deleted = deleted != 0
		results = append(results, meta)
	}

	return results, total, nil
}

// GetStats returns aggregate statistics from the metadata store.
func (m *MetadataStore) GetStats(ctx context.Context) (*MetadataStats, error) {
	stats := &MetadataStats{
		ContentTypes: make(map[string]int64),
	}

	// Totals
	err := m.db.QueryRowContext(ctx, `
		SELECT COALESCE(COUNT(*), 0), COALESCE(SUM(file_size), 0), COALESCE(SUM(download_count), 0)
		FROM file_metadata WHERE deleted = 0
	`).Scan(&stats.TotalFiles, &stats.TotalSize, &stats.TotalDownloads)
	if err != nil {
		return nil, fmt.Errorf("failed to get totals: %w", err)
	}

	// Unique users
	_ = m.db.QueryRowContext(ctx, `
		SELECT COUNT(DISTINCT uploader_jid) FROM file_metadata WHERE deleted = 0 AND uploader_jid != ''
	`).Scan(&stats.UniqueUsers)

	// Content type distribution
	rows, err := m.db.QueryContext(ctx, `
		SELECT
			CASE
				WHEN content_type LIKE 'image/%' THEN 'image'
				WHEN content_type LIKE 'video/%' THEN 'video'
				WHEN content_type LIKE 'audio/%' THEN 'audio'
				WHEN content_type LIKE 'text/%'  THEN 'text'
				WHEN content_type LIKE 'application/pdf' THEN 'pdf'
				ELSE 'other'
			END as category,
			COUNT(*) as cnt
		FROM file_metadata WHERE deleted = 0
		GROUP BY category ORDER BY cnt DESC
	`)
	if err == nil {
		defer rows.Close()
		for rows.Next() {
			var cat string
			var cnt int64
			if err := rows.Scan(&cat, &cnt); err == nil {
				stats.ContentTypes[cat] = cnt
			}
		}
	}

	// Top uploaders
	uploaderRows, err := m.db.QueryContext(ctx, `
		SELECT uploader_jid, COUNT(*) as file_count, COALESCE(SUM(file_size), 0) as total_size
		FROM file_metadata
		WHERE deleted = 0 AND uploader_jid != ''
		GROUP BY uploader_jid
		ORDER BY total_size DESC
		LIMIT 10
	`)
	if err == nil {
		defer uploaderRows.Close()
		for uploaderRows.Next() {
			var s UploaderStat
			if err := uploaderRows.Scan(&s.JID, &s.FileCount, &s.TotalSize); err == nil {
				stats.TopUploaders = append(stats.TopUploaders, s)
			}
		}
	}

	return stats, nil
}

// PurgeDeleted permanently removes soft-deleted records older than the given age.
func (m *MetadataStore) PurgeDeleted(ctx context.Context, olderThan time.Duration) (int64, error) {
	m.mu.Lock()
	defer m.mu.Unlock()

	cutoff := time.Now().Add(-olderThan).UTC()
	result, err := m.db.ExecContext(ctx, `
		DELETE FROM file_metadata WHERE deleted = 1 AND upload_time < ?
	`, cutoff)
	if err != nil {
		return 0, fmt.Errorf("failed to purge deleted records: %w", err)
	}

	return result.RowsAffected()
}

// Close closes the metadata store.
func (m *MetadataStore) Close() error {
	if m.db != nil {
		return m.db.Close()
	}
	return nil
}
