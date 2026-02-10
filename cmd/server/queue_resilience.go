// queue_resilience.go - Enhanced queue resilience implementation

package main

import (
	"context"
	"errors"
	"sync"
	"sync/atomic"
	"time"
)

// RobustQueue represents an enhanced queue with timeout resilience
type RobustQueue struct {
	// Core queue components
	items          chan QueueItem
	spillover      []QueueItem
	spilloverMutex sync.RWMutex

	// Configuration
	config *QueueResilienceConfig

	// State tracking
	length          int64
	processed       int64
	failed          int64
	spilloverActive bool

	// Circuit breaker
	circuitBreaker *CircuitBreaker

	// Priority queues
	highPriority   chan QueueItem
	mediumPriority chan QueueItem
	lowPriority    chan QueueItem

	// Worker management
	workers      []*QueueWorker //nolint:unused
	workerHealth map[int]*WorkerHealth
	healthMutex  sync.RWMutex

	// Context and lifecycle
	ctx    context.Context
	cancel context.CancelFunc
	wg     sync.WaitGroup
}

// QueueItem represents an item in the queue
type QueueItem struct {
	ID          string
	Data        interface{}
	Priority    int
	EnqueueTime time.Time
	Retries     int
	MaxRetries  int
	Timeout     time.Duration
	Context     context.Context
}

// QueueResilienceConfig holds the resilience configuration
type QueueResilienceConfig struct {
	// Basic settings
	Enabled          bool
	QueueSize        int
	SpilloverEnabled bool
	SpilloverMaxSize int64

	// Timeout settings
	QueueOperationTimeout     time.Duration
	QueueDrainTimeout         time.Duration
	WorkerHealthCheckInterval time.Duration

	// Circuit breaker settings
	CircuitBreakerEnabled   bool
	CircuitBreakerThreshold int
	CircuitBreakerTimeout   time.Duration

	// Priority settings
	PriorityLevels         int
	PriorityAgingEnabled   bool
	PriorityAgingThreshold time.Duration

	// Backpressure settings
	BackpressureThreshold  float64
	EmergencyModeThreshold float64
}

// CircuitBreaker implements circuit breaker pattern for queue operations
type CircuitBreaker struct {
	failures    int64
	lastFailure time.Time
	state       int32 // 0=closed, 1=open, 2=half-open
	threshold   int
	timeout     time.Duration
	mutex       sync.RWMutex
}

// WorkerHealth tracks individual worker health
type WorkerHealth struct {
	ID             int
	LastSeen       time.Time
	ProcessedCount int64
	ErrorCount     int64
	AverageTime    time.Duration
	Status         string // "healthy", "slow", "failed"
}

// QueueWorker represents a queue worker
type QueueWorker struct {
	ID     int
	queue  *RobustQueue       //nolint:unused
	health *WorkerHealth      //nolint:unused
	ctx    context.Context    //nolint:unused
	cancel context.CancelFunc //nolint:unused
}

// NewRobustQueue creates a new robust queue with timeout resilience
func NewRobustQueue(config *QueueResilienceConfig) *RobustQueue {
	ctx, cancel := context.WithCancel(context.Background())

	queue := &RobustQueue{
		items:          make(chan QueueItem, config.QueueSize),
		config:         config,
		circuitBreaker: NewCircuitBreaker(config.CircuitBreakerThreshold, config.CircuitBreakerTimeout),
		workerHealth:   make(map[int]*WorkerHealth),
		ctx:            ctx,
		cancel:         cancel,
	}

	// Initialize priority queues if enabled
	if config.PriorityLevels > 1 {
		queue.highPriority = make(chan QueueItem, config.QueueSize/3)
		queue.mediumPriority = make(chan QueueItem, config.QueueSize/3)
		queue.lowPriority = make(chan QueueItem, config.QueueSize/3)
	}

	// Start background routines
	queue.startHealthMonitoring()
	queue.startPriorityAging()
	queue.startSpilloverManager()

	return queue
}

// Enqueue adds an item to the queue with timeout resilience
func (q *RobustQueue) Enqueue(item QueueItem) error {
	// Check circuit breaker
	if !q.circuitBreaker.CanExecute() {
		return errors.New("circuit breaker is open")
	}

	// Create timeout context for queue operation
	ctx, cancel := context.WithTimeout(q.ctx, q.config.QueueOperationTimeout)
	defer cancel()

	// Check backpressure
	currentLoad := float64(atomic.LoadInt64(&q.length)) / float64(q.config.QueueSize)
	if currentLoad > q.config.BackpressureThreshold {
		// Apply backpressure delay
		backpressureDelay := time.Duration(currentLoad * float64(time.Second))
		select {
		case <-time.After(backpressureDelay):
		case <-ctx.Done():
			return ctx.Err()
		}
	}

	// Try to enqueue with priority support
	err := q.enqueueWithPriority(ctx, item)
	if err != nil {
		q.circuitBreaker.RecordFailure()
		return err
	}

	q.circuitBreaker.RecordSuccess()
	atomic.AddInt64(&q.length, 1)
	return nil
}

// enqueueWithPriority handles priority-based enqueueing
func (q *RobustQueue) enqueueWithPriority(ctx context.Context, item QueueItem) error {
	// Set enqueue time
	item.EnqueueTime = time.Now()

	// Choose appropriate queue based on priority
	var targetQueue chan QueueItem
	if q.config.PriorityLevels > 1 {
		switch item.Priority {
		case 3:
			targetQueue = q.highPriority
		case 2:
			targetQueue = q.mediumPriority
		default:
			targetQueue = q.lowPriority
		}
	} else {
		targetQueue = q.items
	}

	// Try to enqueue
	select {
	case targetQueue <- item:
		return nil
	case <-ctx.Done():
		// If primary queue is full, try spillover
		if q.config.SpilloverEnabled {
			return q.spilloverEnqueue(item)
		}
		return ctx.Err()
	}
}

// spilloverEnqueue handles disk spillover when memory queues are full
func (q *RobustQueue) spilloverEnqueue(item QueueItem) error {
	q.spilloverMutex.Lock()
	defer q.spilloverMutex.Unlock()

	// Check spillover size limit
	if int64(len(q.spillover)) >= q.config.SpilloverMaxSize {
		return errors.New("spillover queue is full")
	}

	q.spillover = append(q.spillover, item)
	q.spilloverActive = true
	return nil
}

// Dequeue removes an item from the queue with timeout handling
func (q *RobustQueue) Dequeue(timeout time.Duration) (*QueueItem, error) {
	ctx, cancel := context.WithTimeout(q.ctx, timeout)
	defer cancel()

	// Try priority queues first
	if q.config.PriorityLevels > 1 {
		item, err := q.dequeueWithPriority(ctx)
		if err == nil {
			atomic.AddInt64(&q.length, -1)
			return item, nil
		}
	}

	// Try main queue
	select {
	case item := <-q.items:
		atomic.AddInt64(&q.length, -1)
		return &item, nil
	case <-ctx.Done():
		// Try spillover as last resort
		return q.spilloverDequeue()
	}
}

// dequeueWithPriority handles priority-based dequeuing
func (q *RobustQueue) dequeueWithPriority(ctx context.Context) (*QueueItem, error) {
	// Try high priority first
	select {
	case item := <-q.highPriority:
		return &item, nil
	default:
	}

	// Try medium priority
	select {
	case item := <-q.mediumPriority:
		return &item, nil
	default:
	}

	// Try low priority
	select {
	case item := <-q.lowPriority:
		return &item, nil
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}

// spilloverDequeue retrieves items from disk spillover
func (q *RobustQueue) spilloverDequeue() (*QueueItem, error) {
	q.spilloverMutex.Lock()
	defer q.spilloverMutex.Unlock()

	if len(q.spillover) == 0 {
		return nil, errors.New("no items available")
	}

	item := q.spillover[0]
	q.spillover = q.spillover[1:]

	if len(q.spillover) == 0 {
		q.spilloverActive = false
	}

	return &item, nil
}

// startHealthMonitoring monitors worker health continuously
func (q *RobustQueue) startHealthMonitoring() {
	q.wg.Add(1)
	go func() {
		defer q.wg.Done()
		ticker := time.NewTicker(q.config.WorkerHealthCheckInterval)
		defer ticker.Stop()

		for {
			select {
			case <-ticker.C:
				q.checkWorkerHealth()
			case <-q.ctx.Done():
				return
			}
		}
	}()
}

// checkWorkerHealth evaluates the health of all workers
func (q *RobustQueue) checkWorkerHealth() {
	q.healthMutex.RLock()
	defer q.healthMutex.RUnlock()

	now := time.Now()
	for _, health := range q.workerHealth {
		// Check if worker is responsive
		if now.Sub(health.LastSeen) > q.config.WorkerHealthCheckInterval*2 {
			health.Status = "failed"
			log.Warnf("Worker %d is unresponsive", health.ID)
		} else if health.ErrorCount > health.ProcessedCount/2 {
			health.Status = "slow"
			log.Warnf("Worker %d has high error rate", health.ID)
		} else {
			health.Status = "healthy"
		}
	}
}

// startPriorityAging ages lower priority items to prevent starvation
func (q *RobustQueue) startPriorityAging() {
	if !q.config.PriorityAgingEnabled {
		return
	}

	q.wg.Add(1)
	go func() {
		defer q.wg.Done()
		ticker := time.NewTicker(q.config.PriorityAgingThreshold / 2)
		defer ticker.Stop()

		for {
			select {
			case <-ticker.C:
				q.ageQueueItems()
			case <-q.ctx.Done():
				return
			}
		}
	}()
}

// ageQueueItems promotes old items to higher priority
func (q *RobustQueue) ageQueueItems() {
	now := time.Now()

	// Age medium priority items to high priority
	q.ageSpecificQueue(q.mediumPriority, q.highPriority, now)

	// Age low priority items to medium priority
	q.ageSpecificQueue(q.lowPriority, q.mediumPriority, now)
}

// ageSpecificQueue ages items from source to target queue
func (q *RobustQueue) ageSpecificQueue(source, target chan QueueItem, now time.Time) {
	for {
		select {
		case item := <-source:
			if now.Sub(item.EnqueueTime) > q.config.PriorityAgingThreshold {
				// Age up the item
				item.Priority++
				select {
				case target <- item:
				default:
					// Target queue is full, put it back
					select {
					case source <- item:
					default:
						// Both queues full, move to spillover
						_ = q.spilloverEnqueue(item)
					}
				}
			} else {
				// Put it back, not old enough yet
				select {
				case source <- item:
				default:
					_ = q.spilloverEnqueue(item)
				}
			}
		default:
			return // No more items to age
		}
	}
}

// startSpilloverManager manages the spillover queue
func (q *RobustQueue) startSpilloverManager() {
	q.wg.Add(1)
	go func() {
		defer q.wg.Done()
		ticker := time.NewTicker(time.Second * 30)
		defer ticker.Stop()

		for {
			select {
			case <-ticker.C:
				q.manageSpillover()
			case <-q.ctx.Done():
				return
			}
		}
	}()
}

// manageSpillover tries to move items from spillover back to memory queues
func (q *RobustQueue) manageSpillover() {
	if !q.spilloverActive {
		return
	}

	q.spilloverMutex.Lock()
	defer q.spilloverMutex.Unlock()

	moved := 0
	for i := 0; i < len(q.spillover) && moved < 10; i++ {
		item := q.spillover[i]

		// Try to move back to appropriate queue
		var targetQueue chan QueueItem
		if q.config.PriorityLevels > 1 {
			switch item.Priority {
			case 3:
				targetQueue = q.highPriority
			case 2:
				targetQueue = q.mediumPriority
			default:
				targetQueue = q.lowPriority
			}
		} else {
			targetQueue = q.items
		}

		select {
		case targetQueue <- item:
			// Successfully moved back
			q.spillover = append(q.spillover[:i], q.spillover[i+1:]...)
			i-- // Adjust index after removal
			moved++
		default:
			// Queue still full, try later
		}
	}

	if len(q.spillover) == 0 {
		q.spilloverActive = false
	}

	if moved > 0 {
		log.Debugf("Moved %d items from spillover back to memory queues", moved)
	}
}

// NewCircuitBreaker creates a new circuit breaker
func NewCircuitBreaker(threshold int, timeout time.Duration) *CircuitBreaker {
	return &CircuitBreaker{
		threshold: threshold,
		timeout:   timeout,
	}
}

// CanExecute checks if the circuit breaker allows execution
func (cb *CircuitBreaker) CanExecute() bool {
	cb.mutex.RLock()
	defer cb.mutex.RUnlock()

	state := atomic.LoadInt32(&cb.state)
	if state == 0 { // Closed
		return true
	}

	if state == 1 { // Open
		if time.Since(cb.lastFailure) > cb.timeout {
			// Try to transition to half-open
			if atomic.CompareAndSwapInt32(&cb.state, 1, 2) {
				return true
			}
		}
		return false
	}

	// Half-open state
	return true
}

// RecordSuccess records a successful operation
func (cb *CircuitBreaker) RecordSuccess() {
	cb.mutex.Lock()
	defer cb.mutex.Unlock()

	atomic.StoreInt64(&cb.failures, 0)
	atomic.StoreInt32(&cb.state, 0) // Close circuit
}

// RecordFailure records a failed operation
func (cb *CircuitBreaker) RecordFailure() {
	cb.mutex.Lock()
	defer cb.mutex.Unlock()

	failures := atomic.AddInt64(&cb.failures, 1)
	cb.lastFailure = time.Now()

	if failures >= int64(cb.threshold) {
		atomic.StoreInt32(&cb.state, 1) // Open circuit
	}
}

// GetStats returns queue statistics
func (q *RobustQueue) GetStats() map[string]interface{} {
	return map[string]interface{}{
		"length":           atomic.LoadInt64(&q.length),
		"processed":        atomic.LoadInt64(&q.processed),
		"failed":           atomic.LoadInt64(&q.failed),
		"spillover_active": q.spilloverActive,
		"spillover_size":   len(q.spillover),
		"circuit_state":    atomic.LoadInt32(&q.circuitBreaker.state),
		"circuit_failures": atomic.LoadInt64(&q.circuitBreaker.failures),
	}
}

// Shutdown gracefully shuts down the queue
func (q *RobustQueue) Shutdown(timeout time.Duration) error {
	log.Info("Starting queue shutdown...")

	// Cancel context to stop background routines
	q.cancel()

	// Wait for background routines to finish
	done := make(chan struct{})
	go func() {
		q.wg.Wait()
		close(done)
	}()

	select {
	case <-done:
		log.Info("Queue shutdown completed successfully")
		return nil
	case <-time.After(timeout):
		log.Warn("Queue shutdown timed out")
		return errors.New("shutdown timeout")
	}
}
