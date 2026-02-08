// Package session manages persistent sessions for network resilience.
package session

import (
	"context"
	"encoding/json"
	"sync"
	"time"

	"github.com/go-redis/redis/v8"
	"github.com/patrickmn/go-cache"
	"github.com/sirupsen/logrus"

	"git.uuxo.net/uuxo/hmac-file-server/internal/auth"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// Store manages persistent sessions for network resilience.
type Store struct {
	storage       map[string]*auth.NetworkResilientSession
	mutex         sync.RWMutex
	cleanupTicker *time.Ticker
	redisClient   *redis.Client
	memoryCache   *cache.Cache
	enabled       bool
}

// Global session store
var GlobalStore *Store

// GetSession retrieves a session by ID.
func (s *Store) GetSession(sessionID string) *auth.NetworkResilientSession {
	if !s.enabled || sessionID == "" {
		return nil
	}

	s.mutex.RLock()
	defer s.mutex.RUnlock()

	// Try Redis first if available
	if s.redisClient != nil {
		ctx := context.Background()
		sessionData, err := s.redisClient.Get(ctx, "session:"+sessionID).Result()
		if err == nil {
			var session auth.NetworkResilientSession
			if json.Unmarshal([]byte(sessionData), &session) == nil {
				log.Debugf("Session retrieved from Redis: %s", sessionID)
				return &session
			}
		}
	}

	// Fallback to memory cache
	if s.memoryCache != nil {
		if sessionData, found := s.memoryCache.Get(sessionID); found {
			if session, ok := sessionData.(*auth.NetworkResilientSession); ok {
				log.Debugf("Session retrieved from memory: %s", sessionID)
				return session
			}
		}
	}

	// Fallback to in-memory map
	if session, exists := s.storage[sessionID]; exists {
		if time.Since(session.LastSeen) < 72*time.Hour {
			log.Debugf("Session retrieved from storage: %s", sessionID)
			return session
		}
	}

	return nil
}

// StoreSession stores a session.
func (s *Store) StoreSession(sessionID string, session *auth.NetworkResilientSession) {
	if !s.enabled || sessionID == "" || session == nil {
		return
	}

	s.mutex.Lock()
	defer s.mutex.Unlock()

	session.LastSeen = time.Now()

	// Store in Redis if available
	if s.redisClient != nil {
		ctx := context.Background()
		sessionData, err := json.Marshal(session)
		if err == nil {
			s.redisClient.Set(ctx, "session:"+sessionID, sessionData, 72*time.Hour)
			log.Debugf("Session stored in Redis: %s", sessionID)
		}
	}

	// Store in memory cache
	if s.memoryCache != nil {
		s.memoryCache.Set(sessionID, session, 72*time.Hour)
		log.Debugf("Session stored in memory: %s", sessionID)
	}

	// Store in local map as final fallback
	s.storage[sessionID] = session
	log.Debugf("Session stored in local storage: %s", sessionID)
}

// DeleteSession removes a session.
func (s *Store) DeleteSession(sessionID string) {
	if !s.enabled || sessionID == "" {
		return
	}

	s.mutex.Lock()
	defer s.mutex.Unlock()

	if s.redisClient != nil {
		ctx := context.Background()
		s.redisClient.Del(ctx, "session:"+sessionID)
	}

	if s.memoryCache != nil {
		s.memoryCache.Delete(sessionID)
	}

	delete(s.storage, sessionID)
	log.Debugf("Session deleted: %s", sessionID)
}

func (s *Store) cleanupRoutine() {
	if !s.enabled {
		return
	}

	for range s.cleanupTicker.C {
		s.mutex.Lock()
		for sessionID, session := range s.storage {
			if time.Since(session.LastSeen) > 72*time.Hour {
				delete(s.storage, sessionID)
				log.Debugf("Cleaned up expired session: %s", sessionID)
			}
		}
		s.mutex.Unlock()
	}
}

// Initialize creates and starts the global session store.
func Initialize(redisURL string, enabled bool) {
	if !enabled {
		log.Infof("Session store disabled in configuration")
		GlobalStore = &Store{enabled: false}
		return
	}

	GlobalStore = &Store{
		storage:       make(map[string]*auth.NetworkResilientSession),
		cleanupTicker: time.NewTicker(30 * time.Minute),
		enabled:       true,
	}

	GlobalStore.memoryCache = cache.New(72*time.Hour, 1*time.Hour)

	if redisURL != "" {
		opt, err := redis.ParseURL(redisURL)
		if err == nil {
			GlobalStore.redisClient = redis.NewClient(opt)

			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()

			if err := GlobalStore.redisClient.Ping(ctx).Err(); err == nil {
				log.Infof("Session store: Redis backend initialized (%s)", redisURL)
			} else {
				log.Warnf("Session store: Redis connection failed, using memory backend: %v", err)
				GlobalStore.redisClient = nil
			}
		} else {
			log.Warnf("Session store: Invalid Redis URL, using memory backend: %v", err)
		}
	}

	if GlobalStore.redisClient == nil {
		log.Infof("Session store: Memory backend initialized")
	}

	go GlobalStore.cleanupRoutine()
}
