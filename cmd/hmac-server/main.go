package main
import ( 
        // TODO: Add required imports here 
)
func main() {
	// Set default configuration values
	setDefaults()

	// Flags for configuration file
	var configFile string
	flag.StringVar(&configFile, "config", "./config.toml", "Path to configuration file \"config.toml\".")
	flag.Parse()

	// Load configuration
	err := readConfig(configFile, &conf)
	if err != nil {
		log.Fatalf("Error reading config: %v", err) // Fatal: application cannot proceed
	}
	log.Info("Configuration loaded successfully.")

	// Verify and create ISO container if it doesn't exist
	if conf.ISO.Enabled {
		err = verifyAndCreateISOContainer()
		if err != nil {
			log.Fatalf("ISO container verification failed: %v", err)
		}
	}

	// Initialize file info cache
	fileInfoCache = cache.New(5*time.Minute, 10*time.Minute)

	// Create store directory
	err = os.MkdirAll(conf.Server.StoragePath, os.ModePerm)
	if err != nil {
		log.Fatalf("Error creating store directory: %v", err)
	}
	log.WithField("directory", conf.Server.StoragePath).Info("Store directory is ready")

	// Check free space with retry
	err = checkFreeSpaceWithRetry(conf.Server.StoragePath, 3, 5*time.Second)
	if err != nil {
		log.Fatalf("Insufficient free space: %v", err)
	}

	// Setup logging
	setupLogging()

	// Log system information
	logSystemInfo()

	// Initialize Prometheus metrics
	initMetrics()
	log.Info("Prometheus metrics initialized.")

	// Initialize upload and scan queues
	uploadQueue = make(chan UploadTask, conf.Workers.UploadQueueSize)
	log.Infof("Upload queue initialized with size: %d", conf.Workers.UploadQueueSize)
	scanQueue = make(chan ScanTask, conf.Workers.UploadQueueSize)
	networkEvents = make(chan NetworkEvent, 100)
	log.Info("Upload, scan, and network event channels initialized.")

	// Context for goroutines
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Start network monitoring
	go monitorNetwork(ctx)
	go handleNetworkEvents(ctx)

	// Update system metrics
	go updateSystemMetrics(ctx)

	// Initialize ClamAV client if enabled
	if conf.ClamAV.ClamAVEnabled {
		clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			log.WithFields(logrus.Fields{
				"error": err.Error(),
			}).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
		} else {
			log.Info("ClamAV client initialized successfully.")
		}
	}

	// Initialize Redis client if enabled
	if conf.Redis.RedisEnabled {
		initRedis()
	}

	// Redis Initialization
	initRedis()
	log.Info("Redis client initialized and connected successfully.")

	// ClamAV Initialization
	if conf.ClamAV.ClamAVEnabled {
		clamClient, err = initClamAV(conf.ClamAV.ClamAVSocket)
		if err != nil {
			log.WithFields(logrus.Fields{
				"error": err.Error(),
			}).Warn("ClamAV client initialization failed. Continuing without ClamAV.")
		} else {
			log.Info("ClamAV client initialized successfully.")
		}
	}

	// Initialize worker pools
	initializeUploadWorkerPool(ctx)
	if conf.ClamAV.ClamAVEnabled && clamClient != nil {
		initializeScanWorkerPool(ctx)
	}

	// Start Redis health monitor if Redis is enabled
	if conf.Redis.RedisEnabled && redisClient != nil {
		go MonitorRedisHealth(ctx, redisClient, parseDuration(conf.Redis.RedisHealthCheckInterval))
	}

	// Setup router
	router := setupRouter()

	// Start file cleaner
	fileTTL, err := time.ParseDuration(conf.Server.FileTTL)
	if err != nil {
		log.Fatalf("Invalid FileTTL: %v", err)
	}
	go runFileCleaner(ctx, conf.Server.StoragePath, fileTTL)

	// Parse timeout durations
	readTimeout, err := time.ParseDuration(conf.Timeouts.ReadTimeout)
	if err != nil {
		log.Fatalf("Invalid ReadTimeout: %v", err)
	}

	writeTimeout, err := time.ParseDuration(conf.Timeouts.WriteTimeout)
	if err != nil {
		log.Fatalf("Invalid WriteTimeout: %v", err)
	}

	idleTimeout, err := time.ParseDuration(conf.Timeouts.IdleTimeout)
	if err != nil {
		log.Fatalf("Invalid IdleTimeout: %v", err)
	}

	// Configure HTTP server
	server := &http.Server{
		Addr:         ":" + conf.Server.ListenPort, // Prepend colon to ListenPort
		Handler:      router,
		ReadTimeout:  readTimeout,
		WriteTimeout: writeTimeout,
		IdleTimeout:  idleTimeout,
	}

	// Start metrics server if enabled
	if conf.Server.MetricsEnabled {
		go func() {
			http.Handle("/metrics", promhttp.Handler())
			log.Infof("Metrics server started on port %s", conf.Server.MetricsPort)
			if err := http.ListenAndServe(":"+conf.Server.MetricsPort, nil); err != nil {
				log.Fatalf("Metrics server failed: %v", err)
			}
		}()
	}

	// Setup graceful shutdown
	setupGracefulShutdown(server, cancel)

	// Start server
	log.Infof("Starting HMAC file server %s...", versionString)
	if conf.Server.UnixSocket {
		// Listen on Unix socket
		if err := os.RemoveAll(conf.Server.ListenPort); err != nil {
			log.Fatalf("Failed to remove existing Unix socket: %v", err)
		}
		listener, err := net.Listen("unix", conf.Server.ListenPort)
		if err != nil {
			log.Fatalf("Failed to listen on Unix socket %s: %v", conf.Server.ListenPort, err)
		}
		defer listener.Close()
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	} else {
		// Listen on TCP port
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server failed: %v", err)
		}
	}

	// Example files to include in the ISO container
	files := []string{"file1.txt", "file2.txt"}
	isoPath := "/path/to/container.iso"

	// Create ISO container
	err = CreateISOContainer(files, isoPath, conf.ISO.Size, conf.ISO.Charset)
	if err != nil {
		fmt.Printf("Failed to create ISO container: %v\n", err)
		return
	}

	// Mount ISO container
	err = MountISOContainer(isoPath, conf.ISO.MountPoint)
	if err != nil {
		fmt.Printf("Failed to mount ISO container: %v\n", err)
		return
	}

	// Unmount ISO container (example)
	err = UnmountISOContainer(conf.ISO.MountPoint)
	if err != nil {
		fmt.Printf("Failed to unmount ISO container: %v\n", err)
		return
	}
}
