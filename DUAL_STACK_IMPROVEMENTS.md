# Upload/Download Dual Stack Improvements

## Current State Analysis

The HMAC file server has a multi-layered upload/download system with:
- Standard POST uploads (`handleUpload`)
- Legacy PUT uploads (`handleLegacyUpload`)
- Chunked/resumable uploads (`handleChunkedUpload`)
- Network resilience management
- Simple download handler with buffer pooling
- 32KB buffer pool for I/O operations

## Key Issues Identified

### 1. Buffer Size Limitations
- **Current**: Fixed 32KB buffer size
- **Issue**: Too small for modern high-bandwidth connections
- **Impact**: Suboptimal throughput on fast networks

### 2. Inconsistent I/O Patterns
- **Current**: Different handlers use different copying strategies
- **Issue**: Code duplication and inconsistent performance
- **Impact**: Maintenance burden and varying user experience

### 3. Limited Adaptive Optimization
- **Current**: Static configuration for most parameters
- **Issue**: No runtime adaptation to network conditions
- **Impact**: Poor performance in varying network conditions

### 4. Missing Progressive Enhancement
- **Current**: Basic chunked uploads without intelligent sizing
- **Issue**: Fixed chunk sizes regardless of network speed
- **Impact**: Inefficient for both slow and fast connections

## Proposed Improvements

### 1. Adaptive Buffer Management

```go
// Enhanced buffer pool with adaptive sizing
type AdaptiveBufferPool struct {
    pools map[int]*sync.Pool  // Different sizes
    metrics *NetworkMetrics
    currentOptimalSize int
}

func NewAdaptiveBufferPool() *AdaptiveBufferPool {
    return &AdaptiveBufferPool{
        pools: map[int]*sync.Pool{
            32*1024:   {New: func() interface{} { buf := make([]byte, 32*1024); return &buf }},
            64*1024:   {New: func() interface{} { buf := make([]byte, 64*1024); return &buf }},
            128*1024:  {New: func() interface{} { buf := make([]byte, 128*1024); return &buf }},
            256*1024:  {New: func() interface{} { buf := make([]byte, 256*1024); return &buf }},
            512*1024:  {New: func() interface{} { buf := make([]byte, 512*1024); return &buf }},
            1024*1024: {New: func() interface{} { buf := make([]byte, 1024*1024); return &buf }},
        },
        currentOptimalSize: 32*1024,
    }
}
```

### 2. Unified I/O Engine

```go
// Unified streaming engine for uploads and downloads
type StreamingEngine struct {
    bufferPool *AdaptiveBufferPool
    metrics    *PerformanceMetrics
    resilience *NetworkResilienceManager
}

func (se *StreamingEngine) StreamWithAdaptation(
    dst io.Writer, 
    src io.Reader, 
    contentLength int64,
    sessionID string,
) (int64, error) {
    // Adaptive buffer selection based on:
    // - Network speed
    // - Content length
    // - Historical performance
    // - Available memory
}
```

### 3. Intelligent Chunk Sizing

```go
// Dynamic chunk size calculation
func calculateOptimalChunkSize(
    fileSize int64,
    networkSpeed int64,
    latency time.Duration,
    reliability float64,
) int64 {
    // For high-speed, low-latency networks: larger chunks
    if networkSpeed > 100*1024*1024 && latency < 50*time.Millisecond {
        return min(fileSize/10, 10*1024*1024) // Up to 10MB chunks
    }
    
    // For mobile/unreliable networks: smaller chunks
    if reliability < 0.8 || latency > 200*time.Millisecond {
        return min(fileSize/50, 512*1024) // Up to 512KB chunks
    }
    
    // Default balanced approach
    return min(fileSize/20, 2*1024*1024) // Up to 2MB chunks
}
```

### 4. Progressive Download Enhancement

```go
// Enhanced download with range support and adaptive streaming
func handleDownloadEnhanced(w http.ResponseWriter, r *http.Request) {
    // Support HTTP Range requests
    rangeHeader := r.Header.Get("Range")
    
    if rangeHeader != "" {
        // Handle partial content requests
        return handleRangeDownload(w, r, rangeHeader)
    }
    
    // Adaptive streaming based on client capabilities
    userAgent := r.Header.Get("User-Agent")
    connectionType := detectConnectionType(r)
    
    // Use appropriate buffer size and streaming strategy
    streamingEngine.StreamWithClientOptimization(w, file, fileInfo.Size(), userAgent, connectionType)
}
```

### 5. Performance Monitoring Integration

```go
// Enhanced metrics for optimization feedback
type StreamingMetrics struct {
    ThroughputHistory []ThroughputSample
    LatencyHistory    []time.Duration
    ErrorRates        map[string]float64
    OptimalBufferSize int
    ClientPatterns    map[string]ClientProfile
}

type ClientProfile struct {
    OptimalChunkSize int64
    PreferredProtocol string
    ReliabilityScore float64
    AverageThroughput int64
}
```

## Implementation Plan

### Phase 1: Buffer Pool Enhancement
1. Implement adaptive buffer pool
2. Add performance monitoring
3. Create buffer size optimization algorithm

### Phase 2: Unified I/O Engine
1. Create common streaming interface
2. Migrate all handlers to use unified engine
3. Add network condition awareness

### Phase 3: Intelligent Chunking
1. Implement dynamic chunk sizing
2. Add client-specific optimizations
3. Create predictive algorithms

### Phase 4: Advanced Features
1. Add HTTP Range support
2. Implement connection multiplexing
3. Add client capability detection

## Configuration Enhancements

```toml
[performance]
# Buffer management
adaptive_buffers = true
min_buffer_size = "32KB"
max_buffer_size = "1MB"
buffer_optimization_interval = "5m"

# Chunking strategy
intelligent_chunking = true
min_chunk_size = "256KB"
max_chunk_size = "10MB"
chunk_adaptation_algorithm = "adaptive" # "fixed", "adaptive", "predictive"

# Client optimization
client_profiling = true
profile_persistence_duration = "24h"
connection_type_detection = true

[streaming]
# Progressive enhancement
range_requests = true
connection_multiplexing = false
bandwidth_estimation = true
quality_adaptation = true

# Resilience features
automatic_retry = true
exponential_backoff = true
circuit_breaker = true
```

## Expected Benefits

### Performance Improvements
- **Throughput**: 30-50% improvement on high-speed connections
- **Latency**: Reduced overhead through adaptive buffering
- **Reliability**: Better handling of network issues

### Resource Efficiency
- **Memory**: Dynamic allocation based on actual needs
- **CPU**: Reduced copying overhead
- **Network**: Optimal utilization of available bandwidth

### User Experience
- **Resumability**: Enhanced chunked uploads
- **Responsiveness**: Adaptive to client capabilities
- **Reliability**: Better error handling and recovery

## Compatibility Considerations

- Maintain backward compatibility with existing APIs
- Gradual migration path for existing clients
- Feature detection for progressive enhancement
- Fallback mechanisms for legacy clients

## Monitoring and Observability

```go
// Enhanced metrics for the dual stack
type DualStackMetrics struct {
    // Upload metrics
    UploadThroughput   prometheus.Histogram
    ChunkUploadSize    prometheus.Histogram
    UploadLatency      prometheus.Histogram
    UploadErrors       prometheus.Counter
    
    // Download metrics
    DownloadThroughput prometheus.Histogram
    RangeRequests      prometheus.Counter
    DownloadLatency    prometheus.Histogram
    DownloadErrors     prometheus.Counter
    
    // Buffer metrics
    BufferUtilization  prometheus.Gauge
    OptimalBufferSize  prometheus.Gauge
    BufferSizeChanges  prometheus.Counter
    
    // Network metrics
    NetworkSpeed       prometheus.Gauge
    NetworkLatency     prometheus.Gauge
    NetworkReliability prometheus.Gauge
}
```

This comprehensive improvement plan addresses the current limitations while maintaining the existing functionality and adding significant performance and reliability enhancements.
