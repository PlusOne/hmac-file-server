// hmac_key_rotation.go — HMAC key rotation with grace period support.
//
// Provides automatic rotation of the HMAC signing secret. New uploads are signed
// with the current key, while downloads/validations also accept the previous key
// during a configurable grace period after rotation.

package main

import (
	"crypto/hmac"
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"sync"
	"time"
)

// HMACKeyStore manages current and previous HMAC keys with rotation.
type HMACKeyStore struct {
	Current     string        `json:"current"`  // hex-encoded current key
	Previous    string        `json:"previous"` // hex-encoded previous key
	RotatedAt   time.Time     `json:"rotated_at"`
	GracePeriod time.Duration `json:"-"`
	storagePath string
	mu          sync.RWMutex
}

// persistedKeyStore is the JSON-serializable form.
type persistedKeyStore struct {
	Current   string    `json:"current"`
	Previous  string    `json:"previous"`
	RotatedAt time.Time `json:"rotated_at"`
}

var (
	hmacKeyStore     *HMACKeyStore
	hmacKeyStoreOnce sync.Once
)

// InitHMACKeyRotation initializes the HMAC key rotation system.
// If rotation is disabled, the static secret from SecurityConfig is used.
func InitHMACKeyRotation(cfg *KeyRotationConfig, staticSecret string) error {
	var initErr error
	hmacKeyStoreOnce.Do(func() {
		if cfg == nil || !cfg.Enabled {
			// Use static secret — no rotation
			hmacKeyStore = &HMACKeyStore{
				Current:   staticSecret,
				RotatedAt: time.Now(),
			}
			log.Info("HMAC key rotation disabled, using static secret")
			return
		}

		gracePeriod := 24 * time.Hour
		if cfg.GracePeriod != "" {
			if d, err := parseTTL(cfg.GracePeriod); err == nil {
				gracePeriod = d
			}
		}

		rotationInterval := 24 * time.Hour
		if cfg.RotationInterval != "" {
			if d, err := parseTTL(cfg.RotationInterval); err == nil {
				rotationInterval = d
			}
		}

		storagePath := cfg.KeyStoragePath
		if storagePath == "" {
			storagePath = "/etc/hmac-file-server/keys.json"
		}

		hmacKeyStore = &HMACKeyStore{
			GracePeriod: gracePeriod,
			storagePath: storagePath,
		}

		// Try to load persisted keys
		if err := hmacKeyStore.loadFromDisk(); err != nil {
			log.Infof("No persisted HMAC keys found (%v), initializing with static secret", err)
			hmacKeyStore.Current = staticSecret
			hmacKeyStore.RotatedAt = time.Now()
			hmacKeyStore.persistToDisk()
		}

		// Check if rotation is due
		if time.Since(hmacKeyStore.RotatedAt) > rotationInterval {
			log.Info("HMAC key rotation interval exceeded, rotating now")
			hmacKeyStore.Rotate()
		}

		// Start background rotation
		go hmacKeyStore.rotationLoop(rotationInterval)

		log.Infof("HMAC key rotation initialized: interval=%v, grace=%v, storage=%s",
			rotationInterval, gracePeriod, storagePath)
	})

	return initErr
}

// GetHMACKeyStore returns the global HMAC key store.
func GetHMACKeyStore() *HMACKeyStore {
	return hmacKeyStore
}

// GetCurrentSecret returns the current signing secret.
func (ks *HMACKeyStore) GetCurrentSecret() string {
	ks.mu.RLock()
	defer ks.mu.RUnlock()
	return ks.Current
}

// Rotate performs a key rotation: current becomes previous, a new key is generated.
func (ks *HMACKeyStore) Rotate() {
	ks.mu.Lock()
	defer ks.mu.Unlock()

	ks.Previous = ks.Current
	ks.Current = generateRandomKey()
	ks.RotatedAt = time.Now()

	log.Infof("🔑 HMAC key rotated at %s. Previous key valid for %v grace period.",
		ks.RotatedAt.Format(time.RFC3339), ks.GracePeriod)

	ks.persistToDisk()
}

// ValidateHMAC validates an HMAC signature against both current and previous keys.
// Returns true if the signature matches either key (within grace period for previous).
func (ks *HMACKeyStore) ValidateHMAC(signature []byte, data []byte) bool {
	ks.mu.RLock()
	defer ks.mu.RUnlock()

	// Try current key first
	if hmacEqual(signature, ks.Current, data) {
		return true
	}

	// Try previous key if within grace period
	if ks.Previous != "" && time.Since(ks.RotatedAt) < ks.GracePeriod {
		if hmacEqual(signature, ks.Previous, data) {
			log.Debug("HMAC validated against previous key (within grace period)")
			return true
		}
	}

	return false
}

// ValidateSecret validates data against a specific secret string using HMAC-SHA256.
// Tries the given secret first, then falls back to key store if rotation is enabled.
func (ks *HMACKeyStore) ValidateSecret(signature []byte, data []byte, secret string) bool {
	// Try the provided secret
	if hmacEqual(signature, secret, data) {
		return true
	}

	// If key rotation is active, try the key store's keys
	if ks.Current != secret {
		if hmacEqual(signature, ks.Current, data) {
			return true
		}
	}

	if ks.Previous != "" && ks.Previous != secret && time.Since(ks.RotatedAt) < ks.GracePeriod {
		if hmacEqual(signature, ks.Previous, data) {
			log.Debug("HMAC validated against previous rotated key")
			return true
		}
	}

	return false
}

// hmacEqual computes HMAC-SHA256 of data with the given key and compares to expected.
func hmacEqual(expected []byte, key string, data []byte) bool {
	mac := hmac.New(sha256.New, []byte(key))
	mac.Write(data)
	computed := mac.Sum(nil)
	return hmac.Equal(expected, computed)
}

// generateRandomKey generates a cryptographically random 32-byte hex-encoded key.
func generateRandomKey() string {
	key := make([]byte, 32)
	if _, err := rand.Read(key); err != nil {
		log.Errorf("Failed to generate random key: %v", err)
		// Fallback to timestamp-based key (not ideal but better than nothing)
		return fmt.Sprintf("%x", sha256.Sum256([]byte(time.Now().String())))
	}
	return hex.EncodeToString(key)
}

// loadFromDisk loads persisted keys from the JSON file.
func (ks *HMACKeyStore) loadFromDisk() error {
	if ks.storagePath == "" {
		return fmt.Errorf("no storage path configured")
	}

	data, err := os.ReadFile(ks.storagePath)
	if err != nil {
		return fmt.Errorf("failed to read key file: %w", err)
	}

	var persisted persistedKeyStore
	if err := json.Unmarshal(data, &persisted); err != nil {
		return fmt.Errorf("failed to parse key file: %w", err)
	}

	ks.Current = persisted.Current
	ks.Previous = persisted.Previous
	ks.RotatedAt = persisted.RotatedAt

	log.Infof("Loaded persisted HMAC keys (rotated at %s)", ks.RotatedAt.Format(time.RFC3339))
	return nil
}

// persistToDisk saves current keys to the JSON file.
func (ks *HMACKeyStore) persistToDisk() {
	if ks.storagePath == "" {
		return
	}

	persisted := persistedKeyStore{
		Current:   ks.Current,
		Previous:  ks.Previous,
		RotatedAt: ks.RotatedAt,
	}

	data, err := json.MarshalIndent(persisted, "", "  ")
	if err != nil {
		log.Errorf("Failed to marshal HMAC keys: %v", err)
		return
	}

	// Ensure directory exists
	dir := filepath.Dir(ks.storagePath)
	if err := os.MkdirAll(dir, 0700); err != nil {
		log.Errorf("Failed to create key storage directory %s: %v", dir, err)
		return
	}

	// Write with restricted permissions (owner-only read/write)
	if err := os.WriteFile(ks.storagePath, data, 0600); err != nil {
		log.Errorf("Failed to persist HMAC keys to %s: %v", ks.storagePath, err)
		return
	}

	log.Debugf("HMAC keys persisted to %s", ks.storagePath)
}

// rotationLoop runs periodic key rotation.
func (ks *HMACKeyStore) rotationLoop(interval time.Duration) {
	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for range ticker.C {
		log.Info("Scheduled HMAC key rotation triggered")
		ks.Rotate()
	}
}

// getActiveSecret returns the current HMAC secret to use for operations.
// If key rotation is enabled, returns the key store's current key.
// Otherwise, returns the static secret from config.
func getActiveSecret() string {
	ks := GetHMACKeyStore()
	if ks != nil {
		return ks.GetCurrentSecret()
	}
	confMutex.RLock()
	defer confMutex.RUnlock()
	return conf.Security.Secret
}

// getPreviousSecret returns the previous HMAC secret (for rotation grace period).
// Returns empty string if key rotation is not enabled or no previous key exists.
func getPreviousSecret() string {
	ks := GetHMACKeyStore()
	if ks == nil {
		return ""
	}
	ks.mu.RLock()
	defer ks.mu.RUnlock()
	if ks.Previous != "" && time.Since(ks.RotatedAt) < ks.GracePeriod {
		return ks.Previous
	}
	return ""
}

// validateHMACWithRotation validates HMAC using the current key and falls back
// to the previous key during the grace period. Works with any validation function
// that takes a secret string parameter.
func validateHMACWithRotation(r *http.Request, validateFn func(*http.Request, string) error) error {
	currentSecret := getActiveSecret()
	err := validateFn(r, currentSecret)
	if err == nil {
		return nil
	}

	// Try previous key if within grace period
	prevSecret := getPreviousSecret()
	if prevSecret != "" && prevSecret != currentSecret {
		prevErr := validateFn(r, prevSecret)
		if prevErr == nil {
			log.Info("🔑 HMAC validated against previous rotated key (grace period)")
			return nil
		}
	}

	// Also try the static config secret as ultimate fallback
	confMutex.RLock()
	staticSecret := conf.Security.Secret
	confMutex.RUnlock()
	if staticSecret != currentSecret && staticSecret != prevSecret {
		staticErr := validateFn(r, staticSecret)
		if staticErr == nil {
			log.Info("🔑 HMAC validated against static config secret (fallback)")
			return nil
		}
	}

	return err
}

// validateBearerWithRotation validates Bearer token using key rotation.
func validateBearerWithRotation(r *http.Request) (*BearerTokenClaims, error) {
	currentSecret := getActiveSecret()
	claims, err := validateBearerTokenWithSession(r, currentSecret)
	if err == nil {
		return claims, nil
	}

	// Try previous key
	prevSecret := getPreviousSecret()
	if prevSecret != "" && prevSecret != currentSecret {
		claims, prevErr := validateBearerTokenWithSession(r, prevSecret)
		if prevErr == nil {
			log.Info("🔑 Bearer token validated against previous rotated key")
			return claims, nil
		}
	}

	// Fallback to static secret
	confMutex.RLock()
	staticSecret := conf.Security.Secret
	confMutex.RUnlock()
	if staticSecret != currentSecret && staticSecret != prevSecret {
		claims, staticErr := validateBearerTokenWithSession(r, staticSecret)
		if staticErr == nil {
			log.Info("🔑 Bearer token validated against static config secret")
			return claims, nil
		}
	}

	return nil, err
}
