// main.go — HMAC File Server entry point.
package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"sync"
	"time"

	"crypto/sha256"
	"encoding/hex"

	"github.com/dutchcoders/go-clamd"
	"github.com/go-redis/redis/v8"
	"github.com/patrickmn/go-cache"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/sirupsen/logrus"

	"git.uuxo.net/uuxo/hmac-file-server/internal/compression"
	"git.uuxo.net/uuxo/hmac-file-server/internal/config"
)

// UploadContext maintains upload state across network changes.
type UploadContext struct {
	Filename      string    `json:"filename"`
	TotalSize     int64     `json:"total_size"`
	UploadedBytes int64     `json:"uploaded_bytes"`
	ChunkSize     int64     `json:"chunk_size"`
	LastChunk     int       `json:"last_chunk"`
	ETag          string    `json:"etag,omitempty"`
	UploadPath    string    `json:"upload_path"`
	ContentType   string    `json:"content_type"`
	LastUpdate    time.Time `json:"last_update"`
	SessionID     string    `json:"session_id"`
	PauseChan     chan bool `json:"-"`
	ResumeChan    chan bool `json:"-"`
	CancelChan    chan bool `json:"-"`
	IsPaused      bool      `json:"is_paused"`
}

// NetworkEvent tracks network transitions during session.
type NetworkEvent struct {
	Timestamp   time.Time `json:"timestamp"`
	FromNetwork string    `json:"from_network"`
	ToNetwork   string    `json:"to_network"`
	ClientIP    string    `json:"client_ip"`
	UserAgent   string    `json:"user_agent"`
	EventType   string    `json:"event_type"` // "switch", "resume", "refresh"
}

var (
	conf          config.Config
	versionString string
	log           = logrus.New()

	fileInfoCache     *cache.Cache //nolint:unused
	fileMetadataCache *cache.Cache //nolint:unused

	clamClient     *clamd.Clamd
	redisClient    *redis.Client
	redisConnected bool
	confMutex      sync.RWMutex

	// Prometheus metrics
	uploadDuration      prometheus.Histogram
	uploadErrorsTotal   prometheus.Counter
	uploadsTotal        prometheus.Counter
	downloadDuration    prometheus.Histogram
	downloadsTotal      prometheus.Counter
	downloadErrorsTotal prometheus.Counter
	memoryUsage         prometheus.Gauge
	cpuUsage            prometheus.Gauge
	activeConnections   prometheus.Gauge
	requestsTotal       *prometheus.CounterVec
	goroutines          prometheus.Gauge
	uploadSizeBytes     prometheus.Histogram
	downloadSizeBytes   prometheus.Histogram

	filesDeduplicatedTotal    prometheus.Counter
	deduplicationErrorsTotal  prometheus.Counter
	isoContainersCreatedTotal prometheus.Counter
	isoCreationErrorsTotal    prometheus.Counter
	isoContainersMountedTotal prometheus.Counter
	isoMountErrorsTotal       prometheus.Counter

	workerAdjustmentsTotal   prometheus.Counter
	workerReAdjustmentsTotal prometheus.Counter

	workerPool    *WorkerPool
	networkEvents chan NetworkEvent //nolint:unused
)

var bufferPool = sync.Pool{
	New: func() interface{} {
		buf := make([]byte, 32*1024)
		return &buf
	},
}

// Global client connection tracker for multi-interface support
var clientTracker *ClientConnectionTracker

//nolint:unused
var dualStackClient *http.Client

func main() {
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")

	var genConfig, genConfigAdvanced, validateOnly, runConfigTestsFlag bool
	var validateQuiet, validateVerbose, validateFixable bool
	var validateSecurity, validatePerformance, validateConnectivity bool
	var listValidationChecks, showVersion bool
	var genConfigPath string

	flag.BoolVar(&genConfig, "genconfig", false, "Print minimal configuration example and exit.")
	flag.BoolVar(&genConfigAdvanced, "genconfig-advanced", false, "Print advanced configuration template and exit.")
	flag.StringVar(&genConfigPath, "genconfig-path", "", "Generate config file at specified path and exit.")
	flag.BoolVar(&validateOnly, "validate-config", false, "Validate configuration and exit.")
	flag.BoolVar(&runConfigTestsFlag, "run-config-tests", false, "Run configuration test scenarios and exit.")
	flag.BoolVar(&validateQuiet, "validate-quiet", false, "Quiet validation - errors only.")
	flag.BoolVar(&validateVerbose, "validate-verbose", false, "Verbose validation output.")
	flag.BoolVar(&validateFixable, "check-fixable", false, "Show auto-fixable issues.")
	flag.BoolVar(&validateSecurity, "check-security", false, "Security checks only.")
	flag.BoolVar(&validatePerformance, "check-performance", false, "Performance checks only.")
	flag.BoolVar(&validateConnectivity, "check-connectivity", false, "Network connectivity checks only.")
	flag.BoolVar(&listValidationChecks, "list-checks", false, "List all validation checks and exit.")
	flag.BoolVar(&showVersion, "version", false, "Show version and exit.")
	flag.Parse()

	if showVersion {
		fmt.Println("hmac-file-server version 3.3.0")
		os.Exit(0)
	}

	if listValidationChecks {
		printValidationChecks()
		os.Exit(0)
	}

	if genConfig {
		fmt.Println("# Option 1: Minimal Configuration (recommended for most users)")
		fmt.Println(config.GenerateMinimalConfig())
		fmt.Println("\n# Option 2: Advanced Configuration Template (for fine-tuning)")
		fmt.Println("# Use -genconfig-advanced to generate the advanced template")
		os.Exit(0)
	}
	if genConfigAdvanced {
		fmt.Println(config.GenerateAdvancedConfigTemplate())
		os.Exit(0)
	}
	if genConfigPath != "" {
		var content string
		if genConfigAdvanced {
			content = config.GenerateAdvancedConfigTemplate()
		} else {
			content = config.GenerateMinimalConfig()
		}
		f, err := os.Create(genConfigPath)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to create file: %v\n", err)
			os.Exit(1)
		}
		defer f.Close()
		w := bufio.NewWriter(f)
		fmt.Fprint(w, content)
		w.Flush()
		fmt.Printf("Configuration written to %s\n", genConfigPath)
		os.Exit(0)
	}
	if runConfigTestsFlag {
		RunConfigTests()
		os.Exit(0)
	}

	// Load configuration
	loadedConfig, err := config.LoadSimplifiedConfig(configFile)
	if err != nil {
		if configFile == "./config.toml" || configFile == "" {
			fmt.Println("No configuration file found. Creating a minimal config.toml...")
			if err := config.CreateMinimalConfig(); err != nil {
				log.Fatalf("Failed to create minimal config: %v", err)
			}
			fmt.Println("Minimal config.toml created. Please review and modify as needed, then restart the server.")
			os.Exit(0)
		}
		log.Fatalf("Failed to load configuration: %v", err)
	}
	conf = *loadedConfig
	config.ConfigFileGlobal = configFile
	log.Info("Configuration loaded successfully.")

	err = validateConfig(&conf)
	if err != nil {
		log.Fatalf("Configuration validation failed: %v", err)
	}
	log.Info("Configuration validated successfully.")

	// Perform comprehensive configuration validation
	validationResult := ValidateConfigComprehensive(&conf)

	// Initialize client connection tracker for multi-interface support
	clientNetworkConfig := &ClientNetworkConfig{
		SessionBasedTracking:   conf.ClientNetwork.SessionBasedTracking,
		AllowIPChanges:         conf.ClientNetwork.AllowIPChanges,
		MaxIPChangesPerSession: conf.ClientNetwork.MaxIPChangesPerSession,
		AdaptToClientNetwork:   conf.ClientNetwork.AdaptToClientNetwork,
	}
	if conf.ClientNetwork.SessionMigrationTimeout != "" {
		if timeout, err := time.ParseDuration(conf.ClientNetwork.SessionMigrationTimeout); err == nil {
			clientNetworkConfig.SessionMigrationTimeout = timeout
		} else {
			clientNetworkConfig.SessionMigrationTimeout = 5 * time.Minute
		}
	} else {
		clientNetworkConfig.SessionMigrationTimeout = 5 * time.Minute
	}
	if clientNetworkConfig.MaxIPChangesPerSession == 0 {
		clientNetworkConfig.MaxIPChangesPerSession = 10
	}
	clientTracker = NewClientConnectionTracker(clientNetworkConfig)
	if clientTracker != nil {
		clientTracker.StartCleanupRoutine()
		log.Info("Client multi-interface support initialized")
	}

	// Initialize session store for network resilience
	initializeSessionStore()
	log.Info("Session store for network switching initialized")

	PrintValidationResults(validationResult)

	if validationResult.HasErrors() {
		log.Fatal("Cannot start server due to configuration errors. Please fix the above issues and try again.")
	}

	// Handle specialized validation flags
	if validateSecurity || validatePerformance || validateConnectivity || validateQuiet || validateVerbose || validateFixable {
		runSpecializedValidation(&conf, validateSecurity, validatePerformance, validateConnectivity, validateQuiet, validateVerbose, validateFixable)
		os.Exit(0)
	}

	if validateOnly {
		if validationResult.HasErrors() {
			log.Error("Configuration validation failed with errors. Review the errors above.")
			os.Exit(1)
		} else if validationResult.HasWarnings() {
			log.Info("Configuration is valid but has warnings. Review the warnings above.")
			os.Exit(0)
		} else {
			log.Info("Configuration validation completed successfully!")
			os.Exit(0)
		}
	}

	// Set log level
	level, err := logrus.ParseLevel(conf.Logging.Level)
	if err != nil {
		log.Warnf("Invalid log level '%s', defaulting to 'info'", conf.Logging.Level)
		level = logrus.InfoLevel
	}
	log.SetLevel(level)
	log.Infof("Log level set to: %s", level.String())

	// Log configuration
	log.Infof("Server ListenAddress: %s", conf.Server.ListenAddress)
	log.Infof("Server StoragePath: %s", conf.Server.StoragePath)
	log.Infof("Logging Level: %s", conf.Logging.Level)

	err = writePIDFile(conf.Server.PIDFilePath)
	if err != nil {
		log.Fatalf("Error writing PID file: %v", err)
	}

	setupLogging()
	logSystemInfo()

	// Log compression auto-selection based on CPU ISA extensions
	compressionProfile := compression.AutoSelect()
	log.Infof("Compression Auto-Selection: %s", compressionProfile)
	log.Debugf("\n%s", compression.ISAImpactTable(compressionProfile.Features))

	// Initialize metrics
	initMetrics()

	initializeWorkerSettings(&conf.Server, &conf.Workers, &conf.ClamAV)

	if conf.ISO.Enabled {
		err := createAndMountISO(conf.ISO.Size, conf.ISO.MountPoint, conf.ISO.Charset)
		if err != nil {
			log.Fatalf("Failed to create and mount ISO container: %v", err)
		}
		log.Infof("ISO container mounted at %s", conf.ISO.MountPoint)
	}

	storagePath := conf.Server.StoragePath
	if conf.ISO.Enabled {
		storagePath = conf.ISO.MountPoint
	}

	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)
	fileMetadataCache = cache.New(5*time.Minute, 10*time.Minute)

	if conf.Server.PreCaching {
		go func() {
			log.Info("Starting pre-caching of storage path...")
			err := precacheStoragePath(storagePath)
			if err != nil {
				log.Warnf("Pre-caching storage path failed: %v", err)
			} else {
				log.Info("Pre-cached all files in the storage path.")
			}
		}()
	}

	err = os.MkdirAll(storagePath, os.ModePerm)
	if err != nil {
		log.Fatalf("Error creating store directory: %v", err)
	}
	log.WithField("directory", storagePath).Info("Store directory is ready")

	err = checkFreeSpaceWithRetry(storagePath, 3, 5*time.Second)
	if err != nil {
		log.Fatalf("Insufficient free space: %v", err)
	}

	networkEvents = make(chan NetworkEvent, 100)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	go updateSystemMetrics(ctx)

	if conf.ClamAV.ClamAVEnabled {
		var clamErr error
		clamClient, clamErr = initClamAV(conf.ClamAV.ClamAVSocket)
		if clamErr != nil {
			log.WithError(clamErr).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
		} else {
			log.Info("ClamAV client initialized successfully.")
		}
	}

	if conf.Redis.RedisEnabled {
		initRedis()
	}

	// Initialize enhancements
	// Convert config types for local package compatibility
	auditCfg := AuditConfig{
		Enabled: conf.Audit.Enabled,
		Output:  conf.Audit.Output,
		Path:    conf.Audit.Path,
		Format:  conf.Audit.Format,
		Events:  conf.Audit.Events,
		MaxSize: conf.Audit.MaxSize,
		MaxAge:  conf.Audit.MaxAge,
	}
	if err := InitAuditLogger(&auditCfg); err != nil {
		log.Warnf("Failed to initialize audit logger: %v", err)
	}
	validationCfg := ValidationConfig{
		CheckMagicBytes: conf.Validation.CheckMagicBytes,
		AllowedTypes:    conf.Validation.AllowedTypes,
		BlockedTypes:    conf.Validation.BlockedTypes,
		MaxFileSize:     conf.Validation.MaxFileSize,
		StrictMode:      conf.Validation.StrictMode,
	}
	InitContentValidator(&validationCfg)
	quotaCfg := QuotaConfig{
		Enabled:  conf.Quotas.Enabled,
		Default:  conf.Quotas.Default,
		Tracking: conf.Quotas.Tracking,
		Custom:   conf.Quotas.Custom,
	}
	if err := InitQuotaManager(&quotaCfg, redisClient); err != nil {
		log.Warnf("Failed to initialize quota manager: %v", err)
	}

	// Initialize rate limiter
	rlCfg := &RateLimiterConfig{
		Enabled:         conf.RateLimit.Enabled,
		RequestsPerMin:  conf.RateLimit.RequestsPerMin,
		BurstSize:       conf.RateLimit.BurstSize,
		CleanupInterval: conf.RateLimit.CleanupInterval,
		ByJID:           conf.RateLimit.ByJID,
		ByIP:            conf.RateLimit.ByIP,
		WhitelistedIPs:  conf.RateLimit.WhitelistedIPs,
		WhitelistedJIDs: conf.RateLimit.WhitelistedJIDs,
	}
	InitRateLimiter(rlCfg)

	// Initialize HMAC key rotation
	krCfg := &KeyRotationConfig{
		Enabled:          conf.KeyRotation.Enabled,
		RotationInterval: conf.KeyRotation.RotationInterval,
		GracePeriod:      conf.KeyRotation.GracePeriod,
		KeyStoragePath:   conf.KeyRotation.KeyStoragePath,
	}
	if err := InitHMACKeyRotation(krCfg, conf.Security.Secret); err != nil {
		log.Warnf("Failed to initialize HMAC key rotation: %v", err)
	}

	// Initialize SQLite metadata store
	if conf.Metadata.Enabled {
		dbPath := conf.Metadata.DBPath
		if dbPath == "" {
			dbPath = filepath.Join(conf.Server.StoragePath, ".metadata", "files.db")
		}
		if err := InitMetadataStore(dbPath); err != nil {
			log.Warnf("Failed to initialize metadata store: %v", err)
		}
	}

	// Initialize SIMS thumbnail generation
	InitThumbnails()

	router := setupRouter()
	adminCfg := AdminConfig{
		Enabled:    conf.Admin.Enabled,
		Bind:       conf.Admin.Bind,
		PathPrefix: conf.Admin.PathPrefix,
		Auth: AdminAuthConfig{
			Type:     conf.Admin.Auth.Type,
			Token:    conf.Admin.Auth.Token,
			Username: conf.Admin.Auth.Username,
			Password: conf.Admin.Auth.Password,
		},
	}
	SetupAdminRoutes(router, &adminCfg)
	InitAdminDashboard(router, &adminCfg)
	InitializeEnhancements(router)

	go handleFileCleanup(&conf)

	readTimeout, err := time.ParseDuration(conf.Timeouts.Read)
	if err != nil {
		log.Fatalf("Invalid ReadTimeout: %v", err)
	}
	writeTimeout, err := time.ParseDuration(conf.Timeouts.Write)
	if err != nil {
		log.Fatalf("Invalid WriteTimeout: %v", err)
	}
	idleTimeout, err := time.ParseDuration(conf.Timeouts.Idle)
	if err != nil {
		log.Fatalf("Invalid IdleTimeout: %v", err)
	}

	dialer, err := initializeNetworkProtocol(conf.Server.ForceProtocol)
	if err != nil {
		log.Fatalf("Failed to initialize network protocol: %v", err)
	}
	dualStackClient = &http.Client{
		Transport: &http.Transport{
			DialContext:           dialer.DialContext,
			IdleConnTimeout:       90 * time.Second,
			MaxIdleConns:          100,
			MaxIdleConnsPerHost:   10,
			TLSHandshakeTimeout:   10 * time.Second,
			ResponseHeaderTimeout: 15 * time.Second,
		},
	}

	server := &http.Server{
		Addr:           conf.Server.BindIP + ":" + conf.Server.ListenAddress,
		Handler:        router,
		ReadTimeout:    readTimeout,
		WriteTimeout:   writeTimeout,
		IdleTimeout:    idleTimeout,
		MaxHeaderBytes: 1 << 20,
	}

	if conf.Server.MetricsEnabled {
		go func() {
			http.Handle("/metrics", promhttp.Handler())
			log.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
			if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
				log.Fatalf("Metrics server failed: %v", err)
			}
		}()
	}

	setupGracefulShutdown(server, cancel)

	if conf.Server.AutoAdjustWorkers {
		go monitorWorkerPerformance(ctx, &conf.Server, &conf.Workers, &conf.ClamAV)
	}

	versionString = "3.3.0"
	if conf.Build.Version != "" {
		versionString = conf.Build.Version
	}
	log.Infof("Starting HMAC file server %s...", versionString)

	if conf.Server.UnixSocket {
		socketPath := "/tmp/hmac-file-server.sock"
		if err := os.RemoveAll(socketPath); err != nil {
			log.Fatalf("Failed to remove existing Unix socket: %v", err)
		}
		listener, err := net.Listen("unix", socketPath)
		if err != nil {
			log.Fatalf("Failed to listen on Unix socket %s: %v", socketPath, err)
		}
		defer listener.Close()
		log.Infof("Server listening on Unix socket: %s", socketPath)
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	} else {
		if conf.Server.BindIP == "0.0.0.0" {
			log.Info("Binding to 0.0.0.0. Any net/http logs you see are normal for this universal address.")
		}
		log.Infof("Server listening on %s", server.Addr)
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	}
}

// processScan processes a scan task.
func processScan(task ScanTask) error {
	confMutex.RLock()
	clamEnabled := conf.ClamAV.ClamAVEnabled
	confMutex.RUnlock()

	if !clamEnabled {
		log.Infof("ClamAV disabled, skipping scan for file: %s", task.AbsFilename)
		return nil
	}

	log.Infof("Started processing scan for file: %s", task.AbsFilename)

	clam := clamd.NewClamd(conf.ClamAV.ClamAVSocket)
	response, err := clam.ScanFile(task.AbsFilename)
	if err != nil {
		log.WithFields(logrus.Fields{"file": task.AbsFilename, "error": err}).Error("Failed to scan file")
		return err
	}

	for s := range response {
		if s.Status == clamd.RES_FOUND {
			log.Warnf("ClamAV found threat in %s: %s", task.AbsFilename, s.Description)
			return fmt.Errorf("virus found: %s", s.Description)
		}
	}

	log.Infof("Finished processing scan for file: %s", task.AbsFilename)
	return nil
}

// printValidationChecks prints all available validation checks.
func printValidationChecks() {
	fmt.Println("HMAC File Server Configuration Validation Checks")
	fmt.Println("=================================================")
	fmt.Println()
	fmt.Println("CORE VALIDATION CHECKS:")
	fmt.Println("  server.*        - Server configuration (ports, paths, protocols)")
	fmt.Println("  security.*      - Security settings (secrets, JWT, authentication)")
	fmt.Println("  logging.*       - Logging configuration (levels, files, rotation)")
	fmt.Println("  timeouts.*      - Timeout settings (read, write, idle)")
	fmt.Println("  uploads.*       - Upload configuration (extensions, chunk size)")
	fmt.Println("  downloads.*     - Download configuration (extensions, chunk size)")
	fmt.Println("  workers.*       - Worker pool configuration (count, queue size)")
	fmt.Println("  redis.*         - Redis configuration (address, credentials)")
	fmt.Println("  clamav.*        - ClamAV antivirus configuration")
	fmt.Println("  versioning.*    - File versioning configuration")
	fmt.Println("  deduplication.* - File deduplication configuration")
	fmt.Println("  iso.*           - ISO filesystem configuration")
	fmt.Println()
	fmt.Println("USAGE EXAMPLES:")
	fmt.Println("  hmac-file-server --validate-config              # Full validation")
	fmt.Println("  hmac-file-server --check-security               # Security checks only")
	fmt.Println("  hmac-file-server --check-performance            # Performance checks only")
	fmt.Println("  hmac-file-server --check-connectivity           # Network checks only")
	fmt.Println("  hmac-file-server --validate-quiet               # Errors only")
	fmt.Println("  hmac-file-server --validate-verbose             # Detailed output")
	fmt.Println("  hmac-file-server --check-fixable                # Auto-fixable issues")
	fmt.Println()
}

// UploadTask represents a queued upload task.
type UploadTask struct {
	AbsFilename string
	Request     *http.Request
	Result      chan error
}

// ScanTask represents a queued virus-scan task.
type ScanTask struct {
	AbsFilename string
	Result      chan error
}

// FileMetadata stores extra file metadata like creation date.
type FileMetadata struct {
	CreationDate time.Time
}

// generateSessionID creates a unique session ID from JID and filename.
func generateSessionID(userJID, filename string) string {
	h := sha256.New()
	h.Write([]byte(fmt.Sprintf("%s:%s:%d", userJID, filename, time.Now().UnixNano())))
	return fmt.Sprintf("sess_%s", hex.EncodeToString(h.Sum(nil))[:16])
}

// generateUploadSessionID creates a unique session ID for multi-upload scenarios.
func generateUploadSessionID(uploadType, userAgent, clientIP string) string {
	h := sha256.New()
	h.Write([]byte(fmt.Sprintf("%s:%s:%s:%d", uploadType, userAgent, clientIP, time.Now().UnixNano())))
	return fmt.Sprintf("upload_%s", hex.EncodeToString(h.Sum(nil))[:16])
}

// Type aliases for backward compatibility with auxiliary files.
type Config = config.Config
type ServerConfig = config.ServerConfig
type UploadsConfig = config.UploadsConfig
type DownloadsConfig = config.DownloadsConfig
type SecurityConfig = config.SecurityConfig
type LoggingConfig = config.LoggingConfig
type DeduplicationConfig = config.DeduplicationConfig
type ISOConfig = config.ISOConfig
type TimeoutConfig = config.TimeoutConfig
type VersioningConfig = config.VersioningConfig
type ClamAVConfig = config.ClamAVConfig
type RedisConfig = config.RedisConfig
type WorkersConfig = config.WorkersConfig
type FileConfig = config.FileConfig
type BuildConfig = config.BuildConfig
type NetworkResilienceConfig = config.NetworkResilienceConfig
type ClientNetworkConfigTOML = config.ClientNetworkConfigTOML
type RateLimitConfig = config.RateLimitConfig
type KeyRotationConfig = config.KeyRotationConfig
