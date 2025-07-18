# Queue Resilience Configuration Guide

## Overview

HMAC File Server 3.2 Ultimate Fixed includes advanced queue resilience features designed to handle timeout scenarios gracefully and maintain service availability under various network conditions.

## Enhanced Configuration Sections

### 1. Server-Level Timeout Resilience

```toml
[server]
# Enhanced timeout resilience settings
graceful_shutdown_timeout = "300s"    # Time to wait for active uploads during shutdown
request_timeout = "7200s"             # Maximum time for any single request (2 hours)
keep_alive_timeout = "300s"           # HTTP keep-alive timeout
connection_drain_timeout = "180s"     # Time to drain connections during shutdown
upload_stall_timeout = "600s"         # Timeout if upload stalls (no data received)
download_stall_timeout = "300s"       # Timeout if download stalls
retry_after_timeout = "60s"           # Retry-After header when rejecting due to overload
max_concurrent_uploads = 100          # Maximum concurrent upload operations
upload_rate_limit = "10MB/s"          # Per-connection upload rate limit
connection_pool_size = 200            # Maximum connection pool size
```

**Key Benefits:**
- **Graceful Degradation**: Server doesn't abruptly terminate active uploads during shutdown
- **Stall Detection**: Automatically detects and handles stalled uploads/downloads
- **Connection Management**: Limits concurrent operations to prevent resource exhaustion
- **Rate Limiting**: Prevents individual connections from overwhelming the server

### 2. Enhanced Worker Configuration

```toml
[workers]
# Enhanced queue robustness settings
queue_timeout = "300s"               # Maximum time a job can wait in queue
queue_drain_timeout = "120s"         # Time to wait for queue drain during shutdown
worker_health_check = "30s"          # How often to check worker health
max_queue_retries = 3                # Max retries for failed queue operations
priority_queue_enabled = true        # Enable priority queuing for different file sizes
large_file_queue_size = 20           # Separate queue for files > 100MB
small_file_queue_size = 100          # Queue for files < 10MB
queue_backpressure_threshold = 0.8   # Queue usage % to start backpressure
circuit_breaker_enabled = true       # Enable circuit breaker for queue failures
circuit_breaker_threshold = 10       # Failures before opening circuit
circuit_breaker_timeout = "60s"      # Time before retrying after circuit opens
```

**Key Benefits:**
- **Priority Queuing**: Large files don't block small file uploads
- **Health Monitoring**: Workers are continuously monitored for failures
- **Circuit Breaking**: Automatic failure detection and recovery
- **Backpressure Control**: Gradual slowdown instead of hard failures

### 3. Advanced Queue Resilience

```toml
[queue_resilience]
enabled = true
# Timeout handling
queue_operation_timeout = "30s"      # Max time for queue operations
queue_full_behavior = "reject_oldest" # How to handle full queues
spillover_to_disk = true             # Use disk when memory queue is full
spillover_directory = "/tmp/hmac-queue-spillover"
spillover_max_size = "1GB"           # Max disk spillover size

# Queue persistence and recovery
persistent_queue = true              # Persist queue state
queue_recovery_enabled = true        # Recover queue state on restart
max_recovery_age = "24h"             # Max age of items to recover

# Health monitoring
queue_health_check_interval = "15s"  # Queue health check frequency
dead_letter_queue_enabled = true     # Failed items queue
dead_letter_max_retries = 5          # Max retries before dead letter
dead_letter_retention = "7d"         # Dead letter retention time

# Load balancing and prioritization  
priority_levels = 3                  # Number of priority levels
priority_aging_enabled = true        # Age items to higher priority
priority_aging_threshold = "300s"    # Time before aging up
load_balancing_strategy = "least_connections"

# Memory management
queue_memory_limit = "500MB"         # Max memory for queues
queue_gc_interval = "60s"            # Garbage collection interval
emergency_mode_threshold = 0.95      # Emergency mode trigger
```

**Key Benefits:**
- **Disk Spillover**: Never lose uploads due to memory constraints
- **Queue Recovery**: Resume operations after server restarts
- **Dead Letter Queuing**: Handle persistently failing uploads
- **Priority Aging**: Prevent starvation of lower-priority items

### 4. Comprehensive Timeout Configuration

```toml
[timeouts]
# Basic timeouts (existing)
readtimeout = "4800s"
writetimeout = "4800s" 
idletimeout = "4800s"

# Enhanced timeout resilience
handshake_timeout = "30s"           # TLS handshake timeout
header_timeout = "60s"              # HTTP header read timeout
body_timeout = "7200s"              # HTTP body read timeout
dial_timeout = "30s"                # Connection dial timeout
keep_alive_probe_interval = "30s"   # TCP keep-alive probe interval
keep_alive_probe_count = 9          # Keep-alive probes before giving up

# Adaptive timeouts based on file size
small_file_timeout = "60s"          # Files < 10MB
medium_file_timeout = "600s"        # Files 10MB-100MB
large_file_timeout = "3600s"        # Files 100MB-1GB
huge_file_timeout = "7200s"         # Files > 1GB

# Retry and backoff settings
retry_base_delay = "1s"             # Base delay between retries
retry_max_delay = "60s"             # Maximum delay between retries
retry_multiplier = 2.0              # Exponential backoff multiplier
max_retry_attempts = 5              # Maximum retry attempts
```

**Key Benefits:**
- **Adaptive Timeouts**: Different timeouts based on file size
- **Connection Resilience**: TCP keep-alive prevents silent failures
- **Exponential Backoff**: Intelligent retry timing reduces server load
- **Granular Control**: Fine-tuned timeouts for different operations

## Timeout Scenario Handling

### 1. Network Interruption Scenarios

**Mobile Network Switching:**
- Keep-alive probes detect network changes
- Chunked uploads can resume after network restoration
- Upload sessions persist through network interruptions

**Slow Network Conditions:**
- Adaptive timeouts prevent premature termination
- Rate limiting prevents network saturation
- Progress monitoring detects actual stalls vs. slow transfers

### 2. Server Overload Scenarios

**High Load Conditions:**
- Circuit breaker prevents cascade failures
- Backpressure slows down new requests gracefully
- Priority queuing ensures critical uploads continue

**Memory Pressure:**
- Disk spillover prevents memory exhaustion
- Queue garbage collection manages memory usage
- Emergency mode provides last-resort protection

### 3. Application Restart Scenarios

**Graceful Shutdown:**
- Active uploads get time to complete
- Queue state is persisted before shutdown
- Connections are drained properly

**Recovery After Restart:**
- Queue state is restored from persistence
- Upload sessions are recovered
- Dead letter items are reprocessed

## Monitoring and Observability

### Queue Health Metrics

The enhanced configuration provides comprehensive metrics:

- **Queue Length**: Current items in each queue
- **Queue Processing Time**: Time items spend in queue
- **Worker Health**: Individual worker status and performance
- **Circuit Breaker State**: Open/closed status and failure counts
- **Spillover Usage**: Disk spillover utilization
- **Dead Letter Queue**: Failed item counts and reasons

### Log Messages

Enhanced logging provides visibility into queue operations:

```
INFO: Queue backpressure activated (80% full)
WARN: Circuit breaker opened for upload queue (10 consecutive failures)
INFO: Spillover activated: 50MB written to disk
ERROR: Dead letter queue: Upload failed after 5 retries
INFO: Queue recovery: Restored 23 items from persistence
```

## Best Practices

### 1. Configuration Tuning

**For High-Volume Servers:**
```toml
uploadqueuesize = 200
large_file_queue_size = 50
small_file_queue_size = 500
max_concurrent_uploads = 200
queue_memory_limit = "1GB"
```

**For Memory-Constrained Environments:**
```toml
uploadqueuesize = 50
spillover_to_disk = true
queue_memory_limit = "200MB"
emergency_mode_threshold = 0.85
```

**For Mobile/Unreliable Networks:**
```toml
keep_alive_probe_interval = "15s"
upload_stall_timeout = "300s"
max_retry_attempts = 8
retry_max_delay = "120s"
```

### 2. Monitoring Setup

**Essential Metrics to Monitor:**
- Queue length trends
- Worker health status
- Circuit breaker activations
- Spillover usage
- Dead letter queue growth

**Alert Thresholds:**
- Queue length > 80% capacity
- Circuit breaker open for > 5 minutes
- Dead letter queue growth > 10 items/hour
- Spillover usage > 50% of limit

### 3. Troubleshooting

**Common Issues and Solutions:**

**Frequent Timeouts:**
- Check network stability
- Increase adaptive timeouts for file size
- Enable more aggressive keep-alive settings

**Queue Backlogs:**
- Monitor worker health
- Check for resource constraints
- Consider increasing worker count

**Memory Issues:**
- Enable disk spillover
- Reduce queue memory limit
- Increase garbage collection frequency

## Implementation Notes

The enhanced queue resilience features are designed to be:

1. **Backward Compatible**: Existing configurations continue to work
2. **Opt-in**: Features can be enabled individually
3. **Performance Conscious**: Minimal overhead when not actively needed
4. **Configurable**: All aspects can be tuned for specific environments

These enhancements make HMAC File Server significantly more robust in handling timeout scenarios while maintaining high performance and reliability.
