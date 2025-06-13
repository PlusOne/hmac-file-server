package main

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gdamore/tcell/v2"
	"github.com/pelletier/go-toml"
	"github.com/prometheus/common/expfmt"
	"github.com/rivo/tview"
	"github.com/shirou/gopsutil/v3/cpu"
	"github.com/shirou/gopsutil/v3/mem"
	"github.com/shirou/gopsutil/v3/process"
)

var (
	prometheusURL  string
	configFilePath string // Pfad der gefundenen Konfiguration
	logFilePath    string // Pfad der Logdatei aus der Konfiguration
	metricsEnabled bool   // Neue Variable für die Aktivierung von Metriken
	bindIP         string // Neue Variable für die gebundene IP-Adresse
)

func init() {
	configPaths := []string{
		"/etc/hmac-file-server/config.toml",
		"../config.toml",
		"./config.toml",
	}

	var config *toml.Tree
	var err error

	// Lade die config.toml aus den definierten Pfaden
	for _, path := range configPaths {
		config, err = toml.LoadFile(path)
		if err == nil {
			configFilePath = path
			log.Printf("Using config file: %s", configFilePath)
			break
		}
	}

	if err != nil {
		log.Fatalf("Error loading config file: %v\nPlease create a config.toml in one of the following locations:\n%v", err, configPaths)
	}

	// Metricsport auslesen
	portValue := config.Get("server.metricsport")
	if portValue == nil {
		log.Println("Warning: 'server.metricsport' is missing in the configuration, using default port 9090")
		portValue = int64(9090)
	}

	var port int64
	switch v := portValue.(type) {
	case int64:
		port = v
	case string:
		parsedPort, err := strconv.ParseInt(v, 10, 64)
		if err != nil {
			log.Fatalf("Error parsing 'server.metricsport' as int64: %v", err)
		}
		port = parsedPort
	default:
		log.Fatalf("Error: 'server.metricsport' is not of type int64 or string, got %T", v)
	}

	// Lesen von 'metricsenabled' aus der Konfiguration
	metricsEnabledValue := config.Get("server.metricsenabled")
	if metricsEnabledValue == nil {
		log.Println("Warning: 'server.metricsenabled' ist in der Konfiguration nicht gesetzt. Standardmäßig deaktiviert.")
		metricsEnabled = false
	} else {
		var ok bool
		metricsEnabled, ok = metricsEnabledValue.(bool)
		if !ok {
			log.Fatalf("Konfigurationsfehler: 'server.metricsenabled' sollte ein boolescher Wert sein, aber %T wurde gefunden.", metricsEnabledValue)
		}
	}

	// Lesen von 'bind_ip' aus der Konfiguration
	bindIPValue := config.Get("server.bind_ip")
	if bindIPValue == nil {
		log.Println("Warning: 'server.bind_ip' ist in der Konfiguration nicht gesetzt. Standardmäßig auf 'localhost' gesetzt.")
		bindIP = "localhost"
	} else {
		var ok bool
		bindIP, ok = bindIPValue.(string)
		if !ok {
			log.Fatalf("Konfigurationsfehler: 'server.bind_ip' sollte ein String sein, aber %T wurde gefunden.", bindIPValue)
		}
	}

	// Konstruktion der prometheusURL basierend auf 'bind_ip' und 'metricsport'
	prometheusURL = fmt.Sprintf("http://%s:%d/metrics", bindIP, port)
	log.Printf("Metrics URL gesetzt auf: %s", prometheusURL)

	// Log-Datei auslesen über server.logfile
	logFileValue := config.Get("server.logfile")
	if logFileValue == nil {
		log.Println("Warning: 'server.logfile' is missing, using default '/var/log/hmac-file-server.log'")
		logFilePath = "/var/log/hmac-file-server.log"
	} else {
		lf, ok := logFileValue.(string)
		if !ok {
			log.Fatalf("Error: 'server.logfile' is not of type string, got %T", logFileValue)
		}
		logFilePath = lf
	}
}

// Thresholds for color coding
const (
	HighUsage   = 80.0
	MediumUsage = 50.0
)

// ProcessInfo holds information about a process
type ProcessInfo struct {
	PID                 int32
	Name                string
	CPUPercent          float64
	MemPercent          float32
	CommandLine         string
	Uptime              string  // Neues Feld für die Uptime
	Status              string  // Neues Feld für den Status
	ErrorCount          int     // Neues Feld für die Anzahl der Fehler
	TotalRequests       int64   // Neues Feld für die Gesamtanzahl der Anfragen
	ActiveConnections   int     // Neues Feld für aktive Verbindungen
	AverageResponseTime float64 // Neues Feld für die durchschnittliche Antwortzeit in Millisekunden
}

// Optimized metrics fetching with timeout and connection reuse
func fetchMetrics() (map[string]float64, error) {
	// Create HTTP client with timeout and connection reuse
	client := &http.Client{
		Timeout: 5 * time.Second,
		Transport: &http.Transport{
			MaxIdleConns:       10,
			IdleConnTimeout:    30 * time.Second,
			DisableCompression: true,
		},
	}

	resp, err := client.Get(prometheusURL)
	if err != nil {
		return nil, fmt.Errorf("failed to fetch metrics: %w", err)
	}
	defer resp.Body.Close()

	// Limit response body size to prevent memory issues
	limitedReader := io.LimitReader(resp.Body, 1024*1024) // 1MB limit

	parser := &expfmt.TextParser{}
	metricFamilies, err := parser.TextToMetricFamilies(limitedReader)
	if err != nil {
		return nil, fmt.Errorf("failed to parse metrics: %w", err)
	}

	metrics := make(map[string]float64)

	// More selective metric filtering to reduce processing
	relevantPrefixes := []string{
		"hmac_file_server_",
		"memory_usage_bytes",
		"cpu_usage_percent",
		"active_connections_total",
		"goroutines_count",
		"total_requests",
		"average_response_time_ms",
	}

	for name, mf := range metricFamilies {
		// Quick prefix check to skip irrelevant metrics
		relevant := false
		for _, prefix := range relevantPrefixes {
			if strings.HasPrefix(name, prefix) || name == prefix {
				relevant = true
				break
			}
		}
		if !relevant {
			continue
		}

		for _, m := range mf.GetMetric() {
			var value float64
			if m.GetGauge() != nil {
				value = m.GetGauge().GetValue()
			} else if m.GetCounter() != nil {
				value = m.GetCounter().GetValue()
			} else if m.GetUntyped() != nil {
				value = m.GetUntyped().GetValue()
			} else {
				continue
			}

			// Simplified label handling
			if len(m.GetLabel()) > 0 {
				labels := make([]string, 0, len(m.GetLabel()))
				for _, label := range m.GetLabel() {
					labels = append(labels, fmt.Sprintf("%s=\"%s\"", label.GetName(), label.GetValue()))
				}
				metricKey := fmt.Sprintf("%s{%s}", name, strings.Join(labels, ","))
				metrics[metricKey] = value
			} else {
				metrics[name] = value
			}
		}
	}

	return metrics, nil
}

// Function to fetch system data
func fetchSystemData() (float64, float64, int, error) {
	v, err := mem.VirtualMemory()
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to fetch memory data: %w", err)
	}

	c, err := cpu.Percent(0, false)
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to fetch CPU data: %w", err)
	}

	cores, err := cpu.Counts(true)
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to fetch CPU cores: %w", err)
	}

	cpuUsage := 0.0
	if len(c) > 0 {
		cpuUsage = c[0]
	}

	return v.UsedPercent, cpuUsage, cores, nil
}

// Optimized process list fetching with better resource management
func fetchProcessList() ([]ProcessInfo, error) {
	processes, err := process.Processes()
	if err != nil {
		return nil, fmt.Errorf("failed to fetch processes: %w", err)
	}

	// Pre-allocate slice with reasonable capacity
	processList := make([]ProcessInfo, 0, len(processes))
	var mu sync.Mutex
	var wg sync.WaitGroup

	// Limit concurrent goroutines to prevent resource exhaustion
	sem := make(chan struct{}, 5)           // Reduced from 10 to 5
	timeout := time.After(10 * time.Second) // Add timeout

	// Process only a subset of processes to reduce load
	maxProcesses := 200
	if len(processes) > maxProcesses {
		processes = processes[:maxProcesses]
	}

	for _, p := range processes {
		select {
		case <-timeout:
			log.Printf("Process list fetch timeout, returning partial results")
			return processList, nil
		default:
		}

		wg.Add(1)
		sem <- struct{}{} // Enter semaphore

		go func(p *process.Process) {
			defer wg.Done()
			defer func() {
				<-sem // Exit semaphore
				// Recover from any panics in process info fetching
				if r := recover(); r != nil {
					log.Printf("Process info fetch panic: %v", r)
				}
			}()

			// Set shorter timeout for individual process operations
			ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
			defer cancel()

			// Use context for process operations where possible
			cpuPercent, err := p.CPUPercentWithContext(ctx)
			if err != nil {
				return
			}

			memPercent, err := p.MemoryPercentWithContext(ctx)
			if err != nil {
				return
			}

			name, err := p.NameWithContext(ctx)
			if err != nil {
				return
			}

			// Skip if CPU and memory usage are both very low to reduce noise
			if cpuPercent < 0.1 && memPercent < 0.1 {
				return
			}

			// Limit command line length to prevent memory bloat
			cmdline, err := p.CmdlineWithContext(ctx)
			if err != nil {
				cmdline = ""
			}
			if len(cmdline) > 100 {
				cmdline = cmdline[:100] + "..."
			}

			info := ProcessInfo{
				PID:         p.Pid,
				Name:        name,
				CPUPercent:  cpuPercent,
				MemPercent:  memPercent,
				CommandLine: cmdline,
			}

			mu.Lock()
			processList = append(processList, info)
			mu.Unlock()
		}(p)
	}

	// Wait with timeout
	done := make(chan struct{})
	go func() {
		wg.Wait()
		close(done)
	}()

	select {
	case <-done:
		// All goroutines completed
	case <-time.After(15 * time.Second):
		log.Printf("Process list fetch timeout after 15 seconds, returning partial results")
	}

	return processList, nil
}

// Function to fetch detailed information about hmac-file-server
func fetchHmacFileServerInfo() (*ProcessInfo, error) {
	processes, err := process.Processes()
	if err != nil {
		return nil, fmt.Errorf("failed to fetch processes: %w", err)
	}

	for _, p := range processes {
		name, err := p.Name()
		if err != nil {
			continue
		}

		if name == "hmac-file-server" {
			cpuPercent, err := p.CPUPercent()
			if err != nil {
				cpuPercent = 0.0
			}

			memPercent, err := p.MemoryPercent()
			if err != nil {
				memPercent = 0.0
			}

			cmdline, err := p.Cmdline()
			if err != nil {
				cmdline = ""
			}

			createTime, err := p.CreateTime()
			if err != nil {
				return nil, fmt.Errorf("failed to get process start time: %w", err)
			}
			uptime := time.Since(time.Unix(0, createTime*int64(time.Millisecond)))

			status := "Running" // Standardstatus

			// Überprüfung, ob der Prozess aktiv ist
			isRunning, err := p.IsRunning()
			if err != nil || !isRunning {
				status = "Stopped"
			}

			errorCount, err := countHmacErrors()
			if err != nil {
				errorCount = 0
			}

			metrics, err := fetchMetrics()
			if err != nil {
				return nil, fmt.Errorf("failed to fetch metrics: %w", err)
			}

			totalRequests, ok := metrics["total_requests"]
			if !ok {
				totalRequests = 0
			}

			activeConnections, ok := metrics["active_connections_total"]
			if !ok {
				activeConnections = 0
			}

			averageResponseTime, ok := metrics["average_response_time_ms"]
			if !ok {
				averageResponseTime = 0.0
			}

			return &ProcessInfo{
				PID:                 p.Pid,
				Name:                name,
				CPUPercent:          cpuPercent,
				MemPercent:          memPercent,
				CommandLine:         cmdline,
				Uptime:              uptime.String(),
				Status:              status,
				ErrorCount:          errorCount,
				TotalRequests:       int64(totalRequests),
				ActiveConnections:   int(activeConnections),
				AverageResponseTime: averageResponseTime,
			}, nil
		}
	}

	return nil, fmt.Errorf("hmac-file-server process not found")
}

// Optimized error counting with caching and limits
var (
	errorCountCache     int
	errorCountCacheTime time.Time
	errorCountMutex     sync.RWMutex
)

func countHmacErrors() (int, error) {
	// Use cached value if recent (within 30 seconds)
	errorCountMutex.RLock()
	if time.Since(errorCountCacheTime) < 30*time.Second {
		count := errorCountCache
		errorCountMutex.RUnlock()
		return count, nil
	}
	errorCountMutex.RUnlock()

	// Use the configured log file path
	file, err := os.Open(logFilePath)
	if err != nil {
		return 0, err
	}
	defer file.Close()

	// Get file size to limit reading for very large files
	stat, err := file.Stat()
	if err != nil {
		return 0, err
	}

	// Limit to last 1MB for large log files
	var startPos int64 = 0
	if stat.Size() > 1024*1024 {
		startPos = stat.Size() - 1024*1024
		file.Seek(startPos, io.SeekStart)
	}

	scanner := bufio.NewScanner(file)
	errorCount := 0
	lineCount := 0
	maxLines := 1000 // Limit lines scanned

	for scanner.Scan() && lineCount < maxLines {
		line := scanner.Text()
		if strings.Contains(line, "level=error") {
			errorCount++
		}
		lineCount++
	}

	if err := scanner.Err(); err != nil {
		return 0, err
	}

	// Update cache
	errorCountMutex.Lock()
	errorCountCache = errorCount
	errorCountCacheTime = time.Now()
	errorCountMutex.Unlock()

	return errorCount, nil
}

// Optimized data structure for caching
type cachedData struct {
	systemData systemData
	metrics    map[string]float64
	processes  []ProcessInfo
	hmacInfo   *ProcessInfo
	lastUpdate time.Time
	mu         sync.RWMutex
}

type systemData struct {
	memUsage float64
	cpuUsage float64
	cores    int
}

var cache = &cachedData{}

// Optimized updateUI with reduced frequency and better resource management
func updateUI(ctx context.Context, app *tview.Application, pages *tview.Pages, sysPage, hmacPage tview.Primitive) {
	// Reduce update frequency significantly
	fastTicker := time.NewTicker(5 * time.Second)  // UI updates
	slowTicker := time.NewTicker(15 * time.Second) // Process list updates
	defer fastTicker.Stop()
	defer slowTicker.Stop()

	// Worker pool to limit concurrent operations
	workerPool := make(chan struct{}, 3) // Max 3 concurrent operations

	// Single goroutine for data collection
	go func() {
		defer func() {
			if r := recover(); r != nil {
				log.Printf("Data collection goroutine recovered from panic: %v", r)
			}
		}()

		for {
			select {
			case <-ctx.Done():
				return
			case <-fastTicker.C:
				// Only update system data and metrics (lightweight operations)
				select {
				case workerPool <- struct{}{}:
					go func() {
						defer func() { <-workerPool }()
						updateSystemAndMetrics()
					}()
				default:
					// Skip if worker pool is full
				}
			case <-slowTicker.C:
				// Update process list less frequently (expensive operation)
				select {
				case workerPool <- struct{}{}:
					go func() {
						defer func() { <-workerPool }()
						updateProcessData()
					}()
				default:
					// Skip if worker pool is full
				}
			}
		}
	}()

	// UI update loop
	uiTicker := time.NewTicker(2 * time.Second)
	defer uiTicker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-uiTicker.C:
			app.QueueUpdateDraw(func() {
				updateUIComponents(pages, sysPage, hmacPage)
			})
		}
	}
}

// Separate function to update system data and metrics
func updateSystemAndMetrics() {
	defer func() {
		if r := recover(); r != nil {
			log.Printf("updateSystemAndMetrics recovered from panic: %v", r)
		}
	}()

	// Get system data
	memUsage, cpuUsage, cores, err := fetchSystemData()
	if err != nil {
		log.Printf("Error fetching system data: %v", err)
		return
	}

	// Get metrics if enabled
	var metrics map[string]float64
	if metricsEnabled {
		metrics, err = fetchMetrics()
		if err != nil {
			log.Printf("Error fetching metrics: %v", err)
			metrics = make(map[string]float64) // Use empty map on error
		}
	}

	// Update cache
	cache.mu.Lock()
	cache.systemData = systemData{memUsage, cpuUsage, cores}
	cache.metrics = metrics
	cache.lastUpdate = time.Now()
	cache.mu.Unlock()
}

// Separate function to update process data (expensive operation)
func updateProcessData() {
	defer func() {
		if r := recover(); r != nil {
			log.Printf("updateProcessData recovered from panic: %v", r)
		}
	}()

	// Get process list
	processes, err := fetchProcessList()
	if err != nil {
		log.Printf("Error fetching process list: %v", err)
		return
	}

	// Get HMAC info
	hmacInfo, err := fetchHmacFileServerInfo()
	if err != nil {
		log.Printf("Error fetching HMAC info: %v", err)
	}

	// Update cache
	cache.mu.Lock()
	cache.processes = processes
	cache.hmacInfo = hmacInfo
	cache.mu.Unlock()
}

// Update UI components with cached data
func updateUIComponents(pages *tview.Pages, sysPage, hmacPage tview.Primitive) {
	currentPage, _ := pages.GetFrontPage()

	cache.mu.RLock()
	defer cache.mu.RUnlock()

	switch currentPage {
	case "system":
		sysFlex := sysPage.(*tview.Flex)

		// Update system table
		sysTable := sysFlex.GetItem(0).(*tview.Table)
		updateSystemTable(sysTable, cache.systemData.memUsage, cache.systemData.cpuUsage, cache.systemData.cores)

		// Update metrics table
		if metricsEnabled && len(cache.metrics) > 0 {
			metricsTable := sysFlex.GetItem(1).(*tview.Table)
			updateMetricsTable(metricsTable, cache.metrics)
		}

		// Update process table
		if len(cache.processes) > 0 {
			processTable := sysFlex.GetItem(2).(*tview.Table)
			updateProcessTable(processTable, cache.processes)
		}

	case "hmac":
		if cache.hmacInfo != nil {
			hmacFlex := hmacPage.(*tview.Flex)
			hmacTable := hmacFlex.GetItem(0).(*tview.Table)
			updateHmacTable(hmacTable, cache.hmacInfo, cache.metrics)
		}
	}
}

// Helper function to update system data table
func updateSystemTable(sysTable *tview.Table, memUsage, cpuUsage float64, cores int) {
	sysTable.Clear()
	sysTable.SetCell(0, 0, tview.NewTableCell("Metric").SetAttributes(tcell.AttrBold))
	sysTable.SetCell(0, 1, tview.NewTableCell("Value").SetAttributes(tcell.AttrBold))

	// CPU Usage Row
	cpuUsageCell := tview.NewTableCell(fmt.Sprintf("%.2f%%", cpuUsage))
	if cpuUsage > HighUsage {
		cpuUsageCell.SetTextColor(tcell.ColorRed)
	} else if cpuUsage > MediumUsage {
		cpuUsageCell.SetTextColor(tcell.ColorYellow)
	} else {
		cpuUsageCell.SetTextColor(tcell.ColorGreen)
	}
	sysTable.SetCell(1, 0, tview.NewTableCell("CPU Usage"))
	sysTable.SetCell(1, 1, cpuUsageCell)

	// Memory Usage Row
	memUsageCell := tview.NewTableCell(fmt.Sprintf("%.2f%%", memUsage))
	if memUsage > HighUsage {
		memUsageCell.SetTextColor(tcell.ColorRed)
	} else if memUsage > MediumUsage {
		memUsageCell.SetTextColor(tcell.ColorYellow)
	} else {
		memUsageCell.SetTextColor(tcell.ColorGreen)
	}
	sysTable.SetCell(2, 0, tview.NewTableCell("Memory Usage"))
	sysTable.SetCell(2, 1, memUsageCell)

	// CPU Cores Row
	sysTable.SetCell(3, 0, tview.NewTableCell("CPU Cores"))
	sysTable.SetCell(3, 1, tview.NewTableCell(fmt.Sprintf("%d", cores)))
}

// Helper function to update metrics table
func updateMetricsTable(metricsTable *tview.Table, metrics map[string]float64) {
	metricsTable.Clear()
	metricsTable.SetCell(0, 0, tview.NewTableCell("Metric").SetAttributes(tcell.AttrBold))
	metricsTable.SetCell(0, 1, tview.NewTableCell("Value").SetAttributes(tcell.AttrBold))

	row := 1
	for key, value := range metrics {
		metricsTable.SetCell(row, 0, tview.NewTableCell(key))
		metricsTable.SetCell(row, 1, tview.NewTableCell(fmt.Sprintf("%.2f", value)))
		row++
	}
}

// Helper function to update process table
func updateProcessTable(processTable *tview.Table, processes []ProcessInfo) {
	processTable.Clear()
	processTable.SetCell(0, 0, tview.NewTableCell("PID").SetAttributes(tcell.AttrBold))
	processTable.SetCell(0, 1, tview.NewTableCell("Name").SetAttributes(tcell.AttrBold))
	processTable.SetCell(0, 2, tview.NewTableCell("CPU%").SetAttributes(tcell.AttrBold))
	processTable.SetCell(0, 3, tview.NewTableCell("Mem%").SetAttributes(tcell.AttrBold))
	processTable.SetCell(0, 4, tview.NewTableCell("Command").SetAttributes(tcell.AttrBold))

	// Sort processes by CPU usage
	sort.Slice(processes, func(i, j int) bool {
		return processes[i].CPUPercent > processes[j].CPUPercent
	})

	// Limit to top 20 processes
	maxRows := 20
	if len(processes) < maxRows {
		maxRows = len(processes)
	}

	for i := 0; i < maxRows; i++ {
		p := processes[i]
		processTable.SetCell(i+1, 0, tview.NewTableCell(fmt.Sprintf("%d", p.PID)))
		processTable.SetCell(i+1, 1, tview.NewTableCell(p.Name))
		processTable.SetCell(i+1, 2, tview.NewTableCell(fmt.Sprintf("%.2f", p.CPUPercent)))
		processTable.SetCell(i+1, 3, tview.NewTableCell(fmt.Sprintf("%.2f", p.MemPercent)))
		processTable.SetCell(i+1, 4, tview.NewTableCell(p.CommandLine))
	}
}

// Helper function to update hmac-table
func updateHmacTable(hmacTable *tview.Table, hmacInfo *ProcessInfo, metrics map[string]float64) {
	hmacTable.Clear()
	hmacTable.SetCell(0, 0, tview.NewTableCell("Property").SetAttributes(tcell.AttrBold))
	hmacTable.SetCell(0, 1, tview.NewTableCell("Value").SetAttributes(tcell.AttrBold))

	// Process information
	hmacTable.SetCell(1, 0, tview.NewTableCell("PID"))
	hmacTable.SetCell(1, 1, tview.NewTableCell(fmt.Sprintf("%d", hmacInfo.PID)))

	hmacTable.SetCell(2, 0, tview.NewTableCell("CPU%"))
	hmacTable.SetCell(2, 1, tview.NewTableCell(fmt.Sprintf("%.2f", hmacInfo.CPUPercent)))

	hmacTable.SetCell(3, 0, tview.NewTableCell("Mem%"))
	hmacTable.SetCell(3, 1, tview.NewTableCell(fmt.Sprintf("%.2f", hmacInfo.MemPercent)))

	hmacTable.SetCell(4, 0, tview.NewTableCell("Command"))
	hmacTable.SetCell(4, 1, tview.NewTableCell(hmacInfo.CommandLine))

	hmacTable.SetCell(5, 0, tview.NewTableCell("Uptime"))
	hmacTable.SetCell(5, 1, tview.NewTableCell(hmacInfo.Uptime)) // Neue Zeile für Uptime

	hmacTable.SetCell(6, 0, tview.NewTableCell("Status"))
	hmacTable.SetCell(6, 1, tview.NewTableCell(hmacInfo.Status)) // Neue Zeile für Status

	hmacTable.SetCell(7, 0, tview.NewTableCell("Error Count"))
	hmacTable.SetCell(7, 1, tview.NewTableCell(fmt.Sprintf("%d", hmacInfo.ErrorCount))) // Neue Zeile für Error Count

	hmacTable.SetCell(8, 0, tview.NewTableCell("Total Requests"))
	hmacTable.SetCell(8, 1, tview.NewTableCell(fmt.Sprintf("%d", hmacInfo.TotalRequests))) // Neue Zeile für Total Requests

	hmacTable.SetCell(9, 0, tview.NewTableCell("Active Connections"))
	hmacTable.SetCell(9, 1, tview.NewTableCell(fmt.Sprintf("%d", hmacInfo.ActiveConnections))) // Neue Zeile für Active Connections

	hmacTable.SetCell(10, 0, tview.NewTableCell("Avg. Response Time (ms)"))
	hmacTable.SetCell(10, 1, tview.NewTableCell(fmt.Sprintf("%.2f", hmacInfo.AverageResponseTime))) // Neue Zeile für Average Response Time

	// Metrics related to hmac-file-server
	row := 12
	hmacTable.SetCell(row, 0, tview.NewTableCell("Metric").SetAttributes(tcell.AttrBold))
	hmacTable.SetCell(row, 1, tview.NewTableCell("Value").SetAttributes(tcell.AttrBold))
	row++

	for key, value := range metrics {
		if strings.Contains(key, "hmac_file_server_") {
			hmacTable.SetCell(row, 0, tview.NewTableCell(key))
			hmacTable.SetCell(row, 1, tview.NewTableCell(fmt.Sprintf("%.2f", value)))
			row++
		}
	}
}

func createSystemPage() tview.Primitive {
	// Create system data table
	sysTable := tview.NewTable().SetBorders(false)
	sysTable.SetTitle(" [::b]System Data ").SetBorder(true)

	// Create Prometheus metrics table
	metricsTable := tview.NewTable().SetBorders(false)
	metricsTable.SetTitle(" [::b]Prometheus Metrics ").SetBorder(true)

	// Create process list table
	processTable := tview.NewTable().SetBorders(false)
	processTable.SetTitle(" [::b]Process List ").SetBorder(true)

	// Create a flex layout to hold the tables
	sysFlex := tview.NewFlex().
		SetDirection(tview.FlexRow).
		AddItem(sysTable, 7, 0, false).
		AddItem(metricsTable, 0, 1, false).
		AddItem(processTable, 0, 2, false)

	return sysFlex
}

func createHmacPage() tview.Primitive {
	hmacTable := tview.NewTable().SetBorders(false)
	hmacTable.SetTitle(" [::b]hmac-file-server Details ").SetBorder(true)

	hmacFlex := tview.NewFlex().
		SetDirection(tview.FlexRow).
		AddItem(hmacTable, 0, 1, false)

	return hmacFlex
}

func createLogsPage(ctx context.Context, app *tview.Application, logFilePath string) tview.Primitive {
	logsTextView := tview.NewTextView().
		SetDynamicColors(true).
		SetRegions(true).
		SetWordWrap(true)
	logsTextView.SetTitle(" [::b]Logs ").SetBorder(true)

	const numLines = 50 // Reduced from 100 to 50 lines

	// Cache for log content to avoid reading file too frequently
	var lastLogUpdate time.Time
	var logMutex sync.RWMutex

	// Read logs less frequently and only when on logs page
	go func() {
		ticker := time.NewTicker(5 * time.Second) // Increased from 2 to 5 seconds
		defer ticker.Stop()

		for {
			select {
			case <-ctx.Done():
				return
			case <-ticker.C:
				// Only update if we haven't updated recently
				logMutex.RLock()
				timeSinceUpdate := time.Since(lastLogUpdate)
				logMutex.RUnlock()

				if timeSinceUpdate < 4*time.Second {
					continue
				}

				content, err := readLastNLines(logFilePath, numLines)
				if err != nil {
					app.QueueUpdateDraw(func() {
						logsTextView.SetText(fmt.Sprintf("[red]Error reading log file: %v[white]", err))
					})
					continue
				}

				// Process the log content with color coding
				lines := strings.Split(content, "\n")
				var coloredLines []string

				// Limit the number of lines processed
				maxLines := min(len(lines), numLines)
				coloredLines = make([]string, 0, maxLines)

				for i := len(lines) - maxLines; i < len(lines); i++ {
					if i < 0 {
						continue
					}
					line := lines[i]
					if strings.Contains(line, "level=info") {
						coloredLines = append(coloredLines, "[green]"+line+"[white]")
					} else if strings.Contains(line, "level=warn") {
						coloredLines = append(coloredLines, "[yellow]"+line+"[white]")
					} else if strings.Contains(line, "level=error") {
						coloredLines = append(coloredLines, "[red]"+line+"[white]")
					} else {
						coloredLines = append(coloredLines, line)
					}
				}

				logContent := strings.Join(coloredLines, "\n")

				// Update cache
				logMutex.Lock()
				lastLogUpdate = time.Now()
				logMutex.Unlock()

				app.QueueUpdateDraw(func() {
					logsTextView.SetText(logContent)
				})
			}
		}
	}()

	return logsTextView
}

// Helper function for min
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// Optimized readLastNLines to handle large files efficiently
func readLastNLines(filePath string, n int) (string, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return "", err
	}
	defer file.Close()

	const bufferSize = 1024
	buffer := make([]byte, bufferSize)
	var content []byte
	var fileSize int64

	fileInfo, err := file.Stat()
	if err != nil {
		return "", err
	}
	fileSize = fileInfo.Size()

	var offset int64 = 0
	for {
		if fileSize-offset < bufferSize {
			offset = fileSize
		} else {
			offset += bufferSize
		}

		_, err := file.Seek(-offset, io.SeekEnd)
		if err != nil {
			return "", err
		}

		bytesRead, err := file.Read(buffer)
		if err != nil && err != io.EOF {
			return "", err
		}

		content = append(buffer[:bytesRead], content...)

		if bytesRead < bufferSize || len(strings.Split(string(content), "\n")) > n+1 {
			break
		}

		if offset >= fileSize {
			break
		}
	}

	lines := strings.Split(string(content), "\n")
	if len(lines) > n {
		lines = lines[len(lines)-n:]
	}
	return strings.Join(lines, "\n"), nil
}

func main() {
	app := tview.NewApplication()

	// Create a cancellable context
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Create pages
	pages := tview.NewPages()

	// System page
	sysPage := createSystemPage()
	pages.AddPage("system", sysPage, true, true)

	// hmac-file-server page
	hmacPage := createHmacPage()
	pages.AddPage("hmac", hmacPage, true, false)

	// Logs page mit dem gelesenen logFilePath
	logsPage := createLogsPage(ctx, app, logFilePath)
	pages.AddPage("logs", logsPage, true, false)

	// Add key binding to switch views and handle exit
	app.SetInputCapture(func(event *tcell.EventKey) *tcell.EventKey {
		if event.Key() == tcell.KeyRune {
			switch event.Rune() {
			case 'q', 'Q':
				cancel()
				app.Stop()
				return nil
			case 's', 'S':
				// Switch to system page
				pages.SwitchToPage("system")
			case 'h', 'H':
				// Switch to hmac-file-server page
				pages.SwitchToPage("hmac")
			case 'l', 'L':
				// Switch to logs page
				pages.SwitchToPage("logs")
			}
		}
		return event
	})

	// Start the UI update loop in a separate goroutine
	go updateUI(ctx, app, pages, sysPage, hmacPage)

	// Set the root and run the application
	if err := app.SetRoot(pages, true).EnableMouse(true).Run(); err != nil {
		log.Fatalf("Error running application: %v", err)
		log.Fatalf("Error running application: %v", err)
	}
}
