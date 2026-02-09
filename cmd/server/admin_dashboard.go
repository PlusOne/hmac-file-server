// admin_dashboard.go — HTMX-powered admin web dashboard.
// Serves HTML pages that consume the existing JSON admin API endpoints.
// Uses inline templates (no embed FS) for simplicity and single-binary deployment.

package main

import (
	"fmt"
	"html/template"
	"io/fs"
	"net/http"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"time"
)

var (
	dashboardTmpl   *template.Template
	dashboardPrefix string
)

// getAdminPrefix returns the admin path prefix.
func getAdminPrefix() string {
	if dashboardPrefix != "" {
		return dashboardPrefix
	}
	if adminConfig != nil && adminConfig.PathPrefix != "" {
		return adminConfig.PathPrefix
	}
	return "/admin"
}

// InitAdminDashboard initializes the admin dashboard HTML routes.
func InitAdminDashboard(mux *http.ServeMux, cfg *AdminConfig) {
	if cfg == nil || !cfg.Enabled {
		return
	}

	prefix := cfg.PathPrefix
	if prefix == "" {
		prefix = "/admin"
	}
	dashboardPrefix = prefix

	// Parse inline templates
	funcMap := template.FuncMap{
		"formatBytes": formatBytes,
		"timeAgo":     timeAgo,
		"truncate":    truncateStr,
		"lower":       strings.ToLower,
		"pct": func(used, limit int64) int {
			if limit <= 0 {
				return 0
			}
			p := int(float64(used) / float64(limit) * 100)
			if p > 100 {
				p = 100
			}
			return p
		},
	}

	dashboardTmpl = template.Must(template.New("").Funcs(funcMap).Parse(dashboardTemplateHTML))

	// Dashboard HTML routes — wrapped in admin auth middleware
	dashMux := http.NewServeMux()
	dashMux.HandleFunc(prefix+"/dashboard", handleDashboardIndex)
	dashMux.HandleFunc(prefix+"/dashboard/files", handleDashboardFiles)
	dashMux.HandleFunc(prefix+"/dashboard/users", handleDashboardUsers)

	// HTMX partial endpoints (return HTML fragments for dynamic updates)
	dashMux.HandleFunc(prefix+"/dashboard/htmx/stats", handleHTMXStats)
	dashMux.HandleFunc(prefix+"/dashboard/htmx/recent-files", handleHTMXRecentFiles)
	dashMux.HandleFunc(prefix+"/dashboard/htmx/users", handleHTMXUsers)

	mux.Handle(prefix+"/dashboard", AdminAuthMiddleware(dashMux))
	mux.Handle(prefix+"/dashboard/", AdminAuthMiddleware(dashMux))

	log.Infof("✅ Admin dashboard enabled at %s/dashboard", prefix)
}

// --- Page Handlers ---

func handleDashboardIndex(w http.ResponseWriter, r *http.Request) {
	confMutex.RLock()
	storagePath := conf.Server.StoragePath
	confMutex.RUnlock()

	storage := calculateStorageStats(storagePath)
	users := calculateUserStats(r.Context())

	var mem runtime.MemStats
	runtime.ReadMemStats(&mem)

	data := map[string]interface{}{
		"Page":          "dashboard",
		"Version":       versionString,
		"Prefix":        getAdminPrefix(),
		"Uptime":        time.Since(serverStartTime).Round(time.Second).String(),
		"Storage":       storage,
		"Users":         users,
		"Goroutines":    runtime.NumGoroutine(),
		"MemoryMB":      int64(mem.Alloc / 1024 / 1024),
		"RecentFiles":   getRecentFilesList(storagePath, 15),
		"RedisStatus":   redisStatusStr(),
		"RateLimiter":   rateLimiterStatusStr(),
		"KeyRotation":   keyRotationStatusStr(),
		"CleanupStatus": cleanupStatusStr(),
	}

	renderDashboard(w, "layout", data)
}

func handleDashboardFiles(w http.ResponseWriter, r *http.Request) {
	confMutex.RLock()
	storagePath := conf.Server.StoragePath
	confMutex.RUnlock()

	files := getRecentFilesList(storagePath, 100)

	data := map[string]interface{}{
		"Page":    "files",
		"Version": versionString,
		"Prefix":  getAdminPrefix(),
		"Files":   files,
	}

	renderDashboard(w, "layout", data)
}

func handleDashboardUsers(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	qm := GetQuotaManager()

	var userList []UserInfo
	if qm != nil && qm.config.Enabled {
		quotas, err := qm.GetAllQuotas(ctx)
		if err == nil {
			for _, q := range quotas {
				userList = append(userList, UserInfo{
					JID:        q.JID,
					QuotaUsed:  q.Used,
					QuotaLimit: q.Limit,
					FileCount:  q.FileCount,
					IsBanned:   q.Used > q.Limit,
				})
			}
		}
	}

	data := map[string]interface{}{
		"Page":    "users",
		"Version": versionString,
		"Prefix":  getAdminPrefix(),
		"Users":   userList,
	}

	renderDashboard(w, "layout", data)
}

// --- HTMX Partial Handlers ---

func handleHTMXStats(w http.ResponseWriter, r *http.Request) {
	confMutex.RLock()
	storagePath := conf.Server.StoragePath
	confMutex.RUnlock()

	storage := calculateStorageStats(storagePath)
	users := calculateUserStats(r.Context())

	var mem runtime.MemStats
	runtime.ReadMemStats(&mem)

	data := map[string]interface{}{
		"Storage":    storage,
		"Users":      users,
		"Uptime":     time.Since(serverStartTime).Round(time.Second).String(),
		"Goroutines": runtime.NumGoroutine(),
		"MemoryMB":   int64(mem.Alloc / 1024 / 1024),
	}

	renderDashboard(w, "partial_stats", data)
}

func handleHTMXRecentFiles(w http.ResponseWriter, r *http.Request) {
	confMutex.RLock()
	storagePath := conf.Server.StoragePath
	confMutex.RUnlock()

	data := map[string]interface{}{
		"Files":  getRecentFilesList(storagePath, 15),
		"Prefix": getAdminPrefix(),
	}

	renderDashboard(w, "partial_files", data)
}

func handleHTMXUsers(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	qm := GetQuotaManager()

	var userList []UserInfo
	if qm != nil && qm.config.Enabled {
		quotas, err := qm.GetAllQuotas(ctx)
		if err == nil {
			for _, q := range quotas {
				userList = append(userList, UserInfo{
					JID:        q.JID,
					QuotaUsed:  q.Used,
					QuotaLimit: q.Limit,
					FileCount:  q.FileCount,
				})
			}
		}
	}

	data := map[string]interface{}{
		"Users":  userList,
		"Prefix": getAdminPrefix(),
	}

	renderDashboard(w, "partial_users", data)
}

// --- Helpers ---

func renderDashboard(w http.ResponseWriter, name string, data interface{}) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	if err := dashboardTmpl.ExecuteTemplate(w, name, data); err != nil {
		log.Errorf("Dashboard template error (%s): %v", name, err)
		http.Error(w, "Template rendering error", http.StatusInternalServerError)
	}
}

func timeAgo(t time.Time) string {
	d := time.Since(t)
	switch {
	case d < time.Minute:
		return "just now"
	case d < time.Hour:
		return fmt.Sprintf("%dm ago", int(d.Minutes()))
	case d < 24*time.Hour:
		return fmt.Sprintf("%dh ago", int(d.Hours()))
	default:
		return fmt.Sprintf("%dd ago", int(d.Hours()/24))
	}
}

func truncateStr(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n-3] + "..."
}

func redisStatusStr() string {
	if redisClient == nil {
		return "disabled"
	}
	if redisConnected {
		return "connected"
	}
	return "disconnected"
}

func rateLimiterStatusStr() string {
	rl := GetRateLimiter()
	if rl == nil || !rl.config.Enabled {
		return "disabled"
	}
	return fmt.Sprintf("%d req/min", rl.config.RequestsPerMin)
}

func keyRotationStatusStr() string {
	ks := GetHMACKeyStore()
	if ks == nil || ks.Previous == "" {
		return "disabled"
	}
	return fmt.Sprintf("active (rotated %s)", timeAgo(ks.RotatedAt))
}

func cleanupStatusStr() string {
	confMutex.RLock()
	enabled := conf.Server.FileTTLEnabled
	ttl := conf.Server.FileTTL
	confMutex.RUnlock()
	if !enabled {
		return "disabled"
	}
	return fmt.Sprintf("enabled (TTL: %s)", ttl)
}

func getRecentFilesList(storagePath string, limit int) []FileInfo {
	var files []FileInfo

	_ = filepath.WalkDir(storagePath, func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return nil
		}
		info, infoErr := d.Info()
		if infoErr != nil {
			return nil
		}
		relPath, _ := filepath.Rel(storagePath, path)
		files = append(files, FileInfo{
			ID:          relPath,
			Path:        relPath,
			Name:        filepath.Base(path),
			Size:        info.Size(),
			SizeHuman:   formatBytes(info.Size()),
			ContentType: GetContentType(path),
			ModTime:     info.ModTime(),
		})
		return nil
	})

	// Sort newest first
	sort.Slice(files, func(i, j int) bool {
		return files[i].ModTime.After(files[j].ModTime)
	})

	if len(files) > limit {
		files = files[:limit]
	}
	return files
}

// --- Inline HTML Templates ---
// Using Tailwind CSS via CDN + HTMX for a zero-dependency admin dashboard.

const dashboardTemplateHTML = `
{{define "layout"}}<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>HMAC File Server — {{if eq .Page "dashboard"}}Dashboard{{else if eq .Page "files"}}Files{{else if eq .Page "users"}}Users{{end}}</title>
  <script src="https://unpkg.com/htmx.org@1.9.12"></script>
  <script src="https://cdn.tailwindcss.com"></script>
  <style>
    [hx-indicator] .htmx-indicator { display: none; }
    .htmx-request .htmx-indicator { display: inline-block; }
    .htmx-request.htmx-indicator { display: inline-block; }
  </style>
</head>
<body class="bg-gray-950 text-gray-200 min-h-screen">
  <!-- Navigation -->
  <nav class="bg-gray-900 border-b border-gray-800 px-6 py-3">
    <div class="max-w-7xl mx-auto flex items-center justify-between">
      <div class="flex items-center space-x-3">
        <span class="text-xl font-bold text-emerald-400">☕</span>
        <span class="text-lg font-semibold text-gray-100">hmac-file-server</span>
        <span class="text-xs bg-gray-800 text-gray-400 px-2 py-0.5 rounded">v{{.Version}}</span>
      </div>
      <div class="flex space-x-1">
        <a href="{{.Prefix}}/dashboard"
           class="px-4 py-2 rounded text-sm {{if eq .Page "dashboard"}}bg-emerald-600 text-white{{else}}text-gray-400 hover:bg-gray-800 hover:text-gray-200{{end}}">
          Dashboard
        </a>
        <a href="{{.Prefix}}/dashboard/files"
           class="px-4 py-2 rounded text-sm {{if eq .Page "files"}}bg-emerald-600 text-white{{else}}text-gray-400 hover:bg-gray-800 hover:text-gray-200{{end}}">
          Files
        </a>
        <a href="{{.Prefix}}/dashboard/users"
           class="px-4 py-2 rounded text-sm {{if eq .Page "users"}}bg-emerald-600 text-white{{else}}text-gray-400 hover:bg-gray-800 hover:text-gray-200{{end}}">
          Users
        </a>
        <a href="{{.Prefix}}/stats" target="_blank"
           class="px-4 py-2 rounded text-sm text-gray-400 hover:bg-gray-800 hover:text-gray-200">
          API ↗
        </a>
      </div>
    </div>
  </nav>

  <main class="max-w-7xl mx-auto px-6 py-8">
    {{if eq .Page "dashboard"}}{{template "page_dashboard" .}}{{end}}
    {{if eq .Page "files"}}{{template "page_files" .}}{{end}}
    {{if eq .Page "users"}}{{template "page_users" .}}{{end}}
  </main>

  <footer class="max-w-7xl mx-auto px-6 py-4 text-center text-gray-600 text-xs">
    HMAC File Server v{{.Version}} · XEP-0363 HTTP File Upload
  </footer>
</body>
</html>{{end}}

{{define "page_dashboard"}}
<!-- Stats cards — auto-refresh every 30s -->
<div id="stats-area" hx-get="{{.Prefix}}/dashboard/htmx/stats" hx-trigger="every 30s" hx-swap="innerHTML">
  {{template "partial_stats" .}}
</div>

<!-- Feature status -->
<div class="grid grid-cols-2 md:grid-cols-4 gap-4 mt-6">
  <div class="bg-gray-900 border border-gray-800 rounded-lg p-4">
    <div class="text-xs text-gray-500 uppercase tracking-wide">Redis</div>
    <div class="mt-1 text-sm font-medium {{if eq .RedisStatus "connected"}}text-emerald-400{{else if eq .RedisStatus "disabled"}}text-gray-500{{else}}text-red-400{{end}}">
      {{.RedisStatus}}
    </div>
  </div>
  <div class="bg-gray-900 border border-gray-800 rounded-lg p-4">
    <div class="text-xs text-gray-500 uppercase tracking-wide">Rate Limiter</div>
    <div class="mt-1 text-sm font-medium text-gray-300">{{.RateLimiter}}</div>
  </div>
  <div class="bg-gray-900 border border-gray-800 rounded-lg p-4">
    <div class="text-xs text-gray-500 uppercase tracking-wide">Key Rotation</div>
    <div class="mt-1 text-sm font-medium text-gray-300">{{.KeyRotation}}</div>
  </div>
  <div class="bg-gray-900 border border-gray-800 rounded-lg p-4">
    <div class="text-xs text-gray-500 uppercase tracking-wide">File Cleanup</div>
    <div class="mt-1 text-sm font-medium text-gray-300">{{.CleanupStatus}}</div>
  </div>
</div>

<!-- Recent files -->
<div class="mt-8">
  <h2 class="text-lg font-semibold text-gray-200 mb-4">Recent Uploads</h2>
  <div id="recent-files" hx-get="{{.Prefix}}/dashboard/htmx/recent-files" hx-trigger="every 60s" hx-swap="innerHTML">
    {{template "partial_files" .}}
  </div>
</div>
{{end}}

{{define "page_files"}}
<h1 class="text-2xl font-bold text-gray-100 mb-6">File Manager</h1>
<div id="files-table" hx-get="{{.Prefix}}/dashboard/htmx/recent-files" hx-trigger="every 60s" hx-swap="innerHTML">
  {{template "partial_files" .}}
</div>
{{end}}

{{define "page_users"}}
<h1 class="text-2xl font-bold text-gray-100 mb-6">Users & Quotas</h1>
<div id="users-table" hx-get="{{.Prefix}}/dashboard/htmx/users" hx-trigger="every 30s" hx-swap="innerHTML">
  {{template "partial_users" .}}
</div>
{{end}}

{{define "partial_stats"}}
<div class="grid grid-cols-1 md:grid-cols-4 gap-4">
  <div class="bg-gray-900 border border-gray-800 rounded-lg p-5">
    <div class="text-xs text-gray-500 uppercase tracking-wide">Storage Used</div>
    <div class="mt-2 text-2xl font-bold text-emerald-400">{{.Storage.UsedHuman}}</div>
    <div class="mt-1 text-xs text-gray-500">{{.Storage.FileCount}} files</div>
  </div>
  <div class="bg-gray-900 border border-gray-800 rounded-lg p-5">
    <div class="text-xs text-gray-500 uppercase tracking-wide">Users</div>
    <div class="mt-2 text-2xl font-bold text-blue-400">{{.Users.Total}}</div>
    <div class="mt-1 text-xs text-gray-500">tracked users</div>
  </div>
  <div class="bg-gray-900 border border-gray-800 rounded-lg p-5">
    <div class="text-xs text-gray-500 uppercase tracking-wide">Uptime</div>
    <div class="mt-2 text-2xl font-bold text-purple-400">{{.Uptime}}</div>
    <div class="mt-1 text-xs text-gray-500">server uptime</div>
  </div>
  <div class="bg-gray-900 border border-gray-800 rounded-lg p-5">
    <div class="text-xs text-gray-500 uppercase tracking-wide">Memory / Goroutines</div>
    <div class="mt-2 text-2xl font-bold text-amber-400">{{.MemoryMB}} MB</div>
    <div class="mt-1 text-xs text-gray-500">{{.Goroutines}} goroutines</div>
  </div>
</div>
{{end}}

{{define "partial_files"}}
<div class="bg-gray-900 border border-gray-800 rounded-lg overflow-hidden">
  <table class="w-full text-sm">
    <thead class="bg-gray-800/50">
      <tr>
        <th class="px-4 py-3 text-left text-xs font-medium text-gray-400 uppercase tracking-wider">Name</th>
        <th class="px-4 py-3 text-left text-xs font-medium text-gray-400 uppercase tracking-wider">Size</th>
        <th class="px-4 py-3 text-left text-xs font-medium text-gray-400 uppercase tracking-wider">Type</th>
        <th class="px-4 py-3 text-left text-xs font-medium text-gray-400 uppercase tracking-wider">Modified</th>
        <th class="px-4 py-3 text-right text-xs font-medium text-gray-400 uppercase tracking-wider">Actions</th>
      </tr>
    </thead>
    <tbody class="divide-y divide-gray-800">
      {{range .Files}}
      <tr class="hover:bg-gray-800/30 transition-colors">
        <td class="px-4 py-3">
          <div class="flex items-center">
            <span class="text-gray-300 font-mono text-xs" title="{{.Path}}">{{truncate .Name 40}}</span>
          </div>
        </td>
        <td class="px-4 py-3 text-gray-400">{{.SizeHuman}}</td>
        <td class="px-4 py-3">
          <span class="px-2 py-0.5 text-xs rounded bg-gray-800 text-gray-400">{{truncate .ContentType 20}}</span>
        </td>
        <td class="px-4 py-3 text-gray-500 text-xs">{{timeAgo .ModTime}}</td>
        <td class="px-4 py-3 text-right">
          <button hx-delete="{{$.Prefix}}/files/{{.ID}}" hx-confirm="Delete {{.Name}}?"
                  hx-target="closest tr" hx-swap="outerHTML swap:0.3s"
                  class="text-red-400/60 hover:text-red-400 text-xs px-2 py-1 rounded hover:bg-red-400/10 transition-colors">
            Delete
          </button>
        </td>
      </tr>
      {{else}}
      <tr><td colspan="5" class="px-4 py-8 text-center text-gray-600">No files found</td></tr>
      {{end}}
    </tbody>
  </table>
</div>
{{end}}

{{define "partial_users"}}
<div class="bg-gray-900 border border-gray-800 rounded-lg overflow-hidden">
  <table class="w-full text-sm">
    <thead class="bg-gray-800/50">
      <tr>
        <th class="px-4 py-3 text-left text-xs font-medium text-gray-400 uppercase tracking-wider">JID</th>
        <th class="px-4 py-3 text-left text-xs font-medium text-gray-400 uppercase tracking-wider">Files</th>
        <th class="px-4 py-3 text-left text-xs font-medium text-gray-400 uppercase tracking-wider">Usage</th>
        <th class="px-4 py-3 text-left text-xs font-medium text-gray-400 uppercase tracking-wider">Quota</th>
      </tr>
    </thead>
    <tbody class="divide-y divide-gray-800">
      {{range .Users}}
      <tr class="hover:bg-gray-800/30 transition-colors">
        <td class="px-4 py-3 font-mono text-xs text-gray-300">{{.JID}}</td>
        <td class="px-4 py-3 text-gray-400">{{.FileCount}}</td>
        <td class="px-4 py-3">
          <div class="flex items-center space-x-2">
            <div class="flex-1 bg-gray-800 rounded-full h-2 max-w-[120px]">
              <div class="h-2 rounded-full {{if gt (pct .QuotaUsed .QuotaLimit) 90}}bg-red-500{{else if gt (pct .QuotaUsed .QuotaLimit) 70}}bg-amber-500{{else}}bg-emerald-500{{end}}"
                   style="width: {{pct .QuotaUsed .QuotaLimit}}%"></div>
            </div>
            <span class="text-xs text-gray-500">{{pct .QuotaUsed .QuotaLimit}}%</span>
          </div>
        </td>
        <td class="px-4 py-3 text-gray-500 text-xs">{{formatBytes .QuotaUsed}} / {{formatBytes .QuotaLimit}}</td>
      </tr>
      {{else}}
      <tr><td colspan="4" class="px-4 py-8 text-center text-gray-600">No users tracked (enable quotas)</td></tr>
      {{end}}
    </tbody>
  </table>
</div>
{{end}}
`
