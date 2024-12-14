func handleUpload(w http.ResponseWriter, r *http.Request, absFilename, fileStorePath string, a url.Values) {
	// Log the storage path being used
	log.Infof("Using storage path: %s", conf.Server.StoragePath)

	// Determine protocol version based on query parameters
	var protocolVersion string
	if a.Get("v2") != "" {
		protocolVersion = "v2"
	} else if a.Get("token") != "" {
		protocolVersion = "token"
	} else if a.Get("v") != "" {
		protocolVersion = "v"
	} else {
		log.Warn("No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC")
		http.Error(w, "No HMAC attached to URL. Expecting 'v', 'v2', or 'token' parameter as MAC", http.StatusForbidden)
		return
	}
	log.Debugf("Protocol version determined: %s", protocolVersion)

	// Initialize HMAC
	mac := hmac.New(sha256.New, []byte(conf.Security.Secret))

	// Calculate MAC based on protocolVersion
	if protocolVersion == "v" {
		mac.Write([]byte(fileStorePath + "\x20" + strconv.FormatInt(r.ContentLength, 10)))
	} else if protocolVersion == "v2" || protocolVersion == "token" {
		contentType := mime.TypeByExtension(filepath.Ext(fileStorePath))
		if contentType == "" {
			contentType = "application/octet-stream"
		}
		mac.Write([]byte(fileStorePath + "\x00" + strconv.FormatInt(r.ContentLength, 10) + "\x00" + contentType))
	}

	calculatedMAC := mac.Sum(nil)
	log.Debugf("Calculated MAC: %x", calculatedMAC)

	// Decode provided MAC from hex
	providedMACHex := a.Get(protocolVersion)
	providedMAC, err := hex.DecodeString(providedMACHex)
	if err != nil {
		log.Warn("Invalid MAC encoding")
		http.Error(w, "Invalid MAC encoding", http.StatusForbidden)
		return
	}
	log.Debugf("Provided MAC: %x", providedMAC)

	// Validate the HMAC
	if !hmac.Equal(calculatedMAC, providedMAC) {
		log.Warn("Invalid MAC")
		http.Error(w, "Invalid MAC", http.StatusForbidden)
		return
	}
	log.Debug("HMAC validation successful")

	// Validate file extension
	if !isExtensionAllowed(fileStorePath) {
		log.WithFields(logrus.Fields{
			// No need to sanitize and validate the file path here since absFilename is already sanitized in handleRequest
			"file":  fileStorePath,
			"error": err,
		}).Warn("Invalid file path")
		http.Error(w, "Invalid file path", http.StatusBadRequest)
		uploadErrorsTotal.Inc()
		return
	}
	// absFilename = sanitizedFilename

	// Check if there is enough free space
	err = checkStorageSpace(conf.Server.StoragePath, conf.Server.MinFreeBytes)
	if err != nil {
		log.WithFields(logrus.Fields{
			"storage_path": conf.Server.StoragePath,
			"error":        err,
		}).Warn("Not enough free space")
		http.Error(w, "Not enough free space", http.StatusInsufficientStorage)
		uploadErrorsTotal.Inc()
		return
	}

	// Create an UploadTask with a result channel
	result := make(chan error)
	task := UploadTask{
		AbsFilename: absFilename,
		Request:     r,
		Result:      result,
	}

	// Submit task to the upload queue
	select {
	case uploadQueue <- task:
		// Successfully added to the queue
		log.Debug("Upload task enqueued successfully")
	default:
		// Queue is full
		log.Warn("Upload queue is full. Rejecting upload")
		http.Error(w, "Server busy. Try again later.", http.StatusServiceUnavailable)
		uploadErrorsTotal.Inc()
		return
	}

	// Wait for the worker to process the upload
	err = <-result
	if err != nil {
		// The worker has already logged the error; send an appropriate HTTP response
		http.Error(w, fmt.Sprintf("Upload failed: %v", err), http.StatusInternalServerError)
		return
	}

	// Upload was successful
	w.WriteHeader(http.StatusCreated)
}
func handleMultipartUpload(w http.ResponseWriter, r *http.Request, absFilename string) error {
	err := r.ParseMultipartForm(32 << 20) // 32MB is the default used by FormFile
	if err != nil {
		log.WithError(err).Error("Failed to parse multipart form")
		http.Error(w, "Failed to parse multipart form", http.StatusBadRequest)
		return err
	}

	file, handler, err := r.FormFile("file")
	if err != nil {
		log.WithError(err).Error("Failed to retrieve file from form data")
		http.Error(w, "Failed to retrieve file from form data", http.StatusBadRequest)
		return err
	}
	defer file.Close()

	// Validate file extension
	if !isExtensionAllowed(handler.Filename) {
		log.WithFields(logrus.Fields{
			"filename":  handler.Filename,
			"extension": filepath.Ext(handler.Filename),
		}).Warn("Attempted upload with disallowed file extension")
		http.Error(w, "Disallowed file extension. Allowed extensions are: "+strings.Join(conf.Uploads.AllowedExtensions, ", "), http.StatusForbidden)
		uploadErrorsTotal.Inc()
		return fmt.Errorf("disallowed file extension")
	}

	// Create a temporary file
	tempFilename := absFilename + ".tmp"
	tempFile, err := os.OpenFile(tempFilename, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		log.WithError(err).Error("Failed to create temporary file")
		http.Error(w, "Failed to create temporary file", http.StatusInternalServerError)
		return err
	}
	defer tempFile.Close()

	// Copy the uploaded file to the temporary file
	_, err = io.Copy(tempFile, file)
	if err != nil {
		log.WithError(err).Error("Failed to copy uploaded file to temporary file")
		http.Error(w, "Failed to copy uploaded file", http.StatusInternalServerError)
		return err
	}

	// Perform ClamAV scan on the temporary file
	if clamClient != nil {
		err := scanFileWithClamAV(tempFilename)
		if err != nil {
			log.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Warn("ClamAV detected a virus or scan failed")
			os.Remove(tempFilename)
			uploadErrorsTotal.Inc()
			return err
		}
	}

	// Handle file versioning if enabled
	if conf.Versioning.EnableVersioning {
		existing, _ := fileExists(absFilename)
		if existing {
			err := versionFile(absFilename)
			if err != nil {
				log.WithFields(logrus.Fields{
					"file":  absFilename,
					"error": err,
				}).Error("Error versioning file")
				os.Remove(tempFilename)
				return err
			}
		}
	}

	// Move the temporary file to the final destination
	err = os.Rename(tempFilename, absFilename)
	if err != nil {
		log.WithFields(logrus.Fields{
			"temp_file":  tempFilename,
			"final_file": absFilename,
			"error":      err,
		}).Error("Failed to move file to final destination")
		os.Remove(tempFilename)
		return err
	}

	log.WithFields(logrus.Fields{
		"file": absFilename,
	}).Info("File uploaded and scanned successfully")

	uploadsTotal.Inc()
	return nil
}
func isExtensionAllowed(filename string) bool {
	if len(conf.Uploads.AllowedExtensions) == 0 {
		return true // No restrictions if the list is empty
	}
	ext := strings.ToLower(filepath.Ext(filename))
	for _, allowedExt := range conf.Uploads.AllowedExtensions {
		if strings.ToLower(allowedExt) == ext {
			return true
		}
	}
	return false
}
func processUpload(task UploadTask) error {
	absFilename := task.AbsFilename
	tempFilename := absFilename + ".tmp"
	r := task.Request

	log.Infof("Processing upload for file: %s", absFilename)
	startTime := time.Now()

	// Handle uploads and write to a temporary file
	if conf.Uploads.ChunkedUploadsEnabled {
		log.Debugf("Chunked uploads enabled. Handling chunked upload for %s", tempFilename)
		err := handleChunkedUpload(tempFilename, r)
		if err != nil {
			uploadDuration.Observe(time.Since(startTime).Seconds())
			log.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Failed to handle chunked upload")
			return err
		}
	} else {
		log.Debugf("Handling standard upload for %s", tempFilename)
		err := createFile(tempFilename, r)
		if err != nil {
			log.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Error("Error creating file")
			uploadDuration.Observe(time.Since(startTime).Seconds())
			return err
		}
	}

	// Perform ClamAV scan on the temporary file
	if clamClient != nil {
		log.Debugf("Scanning %s with ClamAV", tempFilename)
		err := scanFileWithClamAV(tempFilename)
		if err != nil {
			log.WithFields(logrus.Fields{
				"file":  tempFilename,
				"error": err,
			}).Warn("ClamAV detected a virus or scan failed")
			os.Remove(tempFilename)
			uploadErrorsTotal.Inc()
			return err
		}
		log.Infof("ClamAV scan passed for file: %s", tempFilename)
	}

	// Handle file versioning if enabled
	if conf.Versioning.EnableVersioning {
		existing, _ := fileExists(absFilename)
		if existing {
			log.Infof("File %s exists. Initiating versioning.", absFilename)
			err := versionFile(absFilename)
			if err != nil {
				log.WithFields(logrus.Fields{
					"file":  absFilename,
					"error": err,
				}).Error("Error versioning file")
				os.Remove(tempFilename)
				return err
			}
			log.Infof("File versioned successfully: %s", absFilename)
		}
	}

	// Rename temporary file to final destination
	err := os.Rename(tempFilename, absFilename)
	if err != nil {
		log.WithFields(logrus.Fields{
			"temp_file":  tempFilename,
			"final_file": absFilename,
			"error":      err,
		}).Error("Failed to move file to final destination")
		os.Remove(tempFilename)
		return err
	}
	log.Infof("File moved to final destination: %s", absFilename)

	// Handle deduplication if enabled
	if conf.Server.DeduplicationEnabled {
		log.Debugf("Deduplication enabled. Checking duplicates for %s", absFilename)
		err = handleDeduplication(context.Background(), absFilename)
		if err != nil {
			log.WithError(err).Error("Deduplication failed")
			uploadErrorsTotal.Inc()
			return err
		}
		log.Infof("Deduplication handled successfully for file: %s", absFilename)
	}

	// Handle ISO container if enabled
	if conf.ISO.Enabled {
		err = handleISOContainer(absFilename)
		if err != nil {
			log.WithError(err).Error("ISO container handling failed")
			uploadErrorsTotal.Inc()
			return err
		}
		log.Infof("ISO container handled successfully for file: %s", absFilename)
	}

	log.WithFields(logrus.Fields{
		"file": absFilename,
	}).Info("File uploaded and processed successfully")

	uploadDuration.Observe(time.Since(startTime).Seconds())
	uploadsTotal.Inc()
	return nil
}
func uploadWorker(ctx context.Context, workerID int) {
    log.Infof("Upload worker %d started.", workerID)
    defer log.Infof("Upload worker %d stopped.", workerID)
    for {
        select {
        case <-ctx.Done():
            return
        case task, ok := <-uploadQueue:
            if !ok {
                return
            }
            err := processUpload(task)
            task.Result <- err
        }
    }
}
func initializeUploadWorkerPool(ctx context.Context) {
    for i := 0; i < conf.Workers.NumWorkers; i++ {
        go uploadWorker(ctx, i)
    }
    log.Infof("Initialized %d upload workers", conf.Workers.NumWorkers)
}
func createFile(tempFilename string, r *http.Request) error {
    absDirectory := filepath.Dir(tempFilename)
    err := os.MkdirAll(absDirectory, os.ModePerm)
    if err != nil {
        return fmt.Errorf("failed to create directory: %v", err)
    }

    // Open the file for writing
    targetFile, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
    if err != nil {
        return fmt.Errorf("failed to open file: %v", err)
    }
    defer targetFile.Close()

    // Use a larger buffer for efficient file writing
    bufferSize := 8 * 1024 * 1024 // 8 MB buffer
    writer := bufio.NewWriterSize(targetFile, bufferSize)
    buffer := make([]byte, bufferSize)

    totalBytes := int64(0)
    for {
        n, err := r.Body.Read(buffer)
        if err != nil && err != io.EOF {
            return fmt.Errorf("failed to read request body: %v", err)
        }
        if n == 0 {
            break
        }

        _, err = writer.Write(buffer[:n])
        if err != nil {
            return fmt.Errorf("failed to write to file: %v", err)
        }
        totalBytes += int64(n)
    }

    err = writer.Flush()
    if err != nil {
        return fmt.Errorf("failed to flush writer: %v", err)
    }

    log.WithFields(logrus.Fields{
        "temp_file":   tempFilename,
        "total_bytes": totalBytes,
    }).Info("File uploaded successfully")

    uploadSizeBytes.Observe(float64(totalBytes))
    return nil
}
func handleChunkedUpload(tempFilename string, r *http.Request) error {
	log.WithField("file", tempFilename).Info("Handling chunked upload to temporary file")

	// Ensure the directory exists
	absDirectory := filepath.Dir(tempFilename)
	err := os.MkdirAll(absDirectory, os.ModePerm)
	if err != nil {
		return fmt.Errorf("failed to create directory: %v", err)
	}

	targetFile, err := os.OpenFile(tempFilename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return fmt.Errorf("failed to open file: %v", err)
	}
	defer targetFile.Close()

	writer := bufio.NewWriterSize(targetFile, int(conf.Uploads.ChunkSize))
	buffer := make([]byte, conf.Uploads.ChunkSize)

	totalBytes := int64(0)
	for {
		n, err := r.Body.Read(buffer)
		if err != nil && err != io.EOF {
			return fmt.Errorf("failed to read request body: %v", err)
		}
		if n == 0 {
			break
		}

		_, err = writer.Write(buffer[:n])
		if err != nil {
			return fmt.Errorf("failed to write to file: %v", err)
		}
		totalBytes += int64(n)
	}

	err = writer.Flush()
	if err != nil {
		return fmt.Errorf("failed to flush writer: %v", err)
	}

	log.WithFields(logrus.Fields{
		"temp_file":   tempFilename,
		"total_bytes": totalBytes,
	}).Info("Chunked upload completed successfully")

	uploadSizeBytes.Observe(float64(totalBytes))
	return nil
}
