package main

import (
    "bufio"
    "fmt"
    "log"
    "net/http"
    "os"
    "sort"
    "strconv"
    "strings"
    "time"

    "github.com/gdamore/tcell/v2"
    "github.com/pelletier/go-toml"
    "github.com/prometheus/common/expfmt"
    "github.com/rivo/tview"
    "github.com/shirou/gopsutil/v3/cpu"
    "github.com/shirou/gopsutil/v3/mem"
    "github.com/shirou/gopsutil/v3/process"
)

var prometheusURL string
var configFilePath string // Pfad der gefundenen Konfiguration
var logFilePath string    // Pfad der Logdatei aus der Konfiguration

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
        log.Fatalf("Error loading config file: %v", err)
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

    prometheusURL = fmt.Sprintf("http://localhost:%d/metrics", port)

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
    PID         int32
    Name        string
    CPUPercent  float64
    MemPercent  float32
    CommandLine string
}

// Function to fetch and parse Prometheus metrics
func fetchMetrics() (map[string]float64, error) {
    resp, err := http.Get(prometheusURL)
    if err != nil {
        return nil, fmt.Errorf("failed to fetch metrics: %w", err)
    }
    defer resp.Body.Close()

    parser := &expfmt.TextParser{}
    metricFamilies, err := parser.TextToMetricFamilies(resp.Body)
    if err != nil {
        return nil, fmt.Errorf("failed to parse metrics: %w", err)
    }

    metrics := make(map[string]float64)
    for name, mf := range metricFamilies {
        // Filter the metrics you're interested in
        if strings.HasPrefix(name, "hmac_file_server_") ||
            name == "memory_usage_bytes" ||
            name == "cpu_usage_percent" ||
            name == "active_connections_total" ||
            name == "goroutines_count" {

            for _, m := range mf.GetMetric() {
                var value float64
                if m.GetGauge() != nil {
                    value = m.GetGauge().GetValue()
                } else if m.GetCounter() != nil {
                    value = m.GetCounter().GetValue()
                } else if m.GetUntyped() != nil {
                    value = m.GetUntyped().GetValue()
                } else {
                    // If the metric type is not handled, skip it
                    continue
                }

                // Handle metrics with labels
                if len(m.GetLabel()) > 0 {
                    labels := make([]string, 0)
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