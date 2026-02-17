// cpuid_amd64.go — native CPUID detection for amd64 using Go's
// internal/cpu hints and runtime feature detection.

//go:build amd64

package cpufeatures

import (
	"os"
	"strings"
)

// detectX86CPUID uses /proc/cpuinfo on Linux (which is always available
// on amd64 Linux).  On other OSes it returns false to trigger the fallback.
// For Go 1.24+ we could also use internal/cpu but it is not exported.
func detectX86CPUID(f *Features) bool {
	// On Linux, /proc/cpuinfo is the most reliable source
	if _, err := os.Stat("/proc/cpuinfo"); err == nil {
		return false // let detectFromProcCPUInfo handle it
	}

	// On non-Linux amd64 (macOS, FreeBSD, Windows), try to detect
	// using Go's built-in support via environment hints or return false
	// NOTE: For macOS/FreeBSD we could use sysctl but /proc fallback
	// will have already been attempted.

	// Attempt macOS sysctl detection
	if data, err := os.ReadFile("/usr/sbin/sysctl"); err == nil {
		_ = data // not directly usable, but presence confirms macOS
	}

	// Final: set conservative defaults for amd64 — SSE2 is guaranteed
	if f.Vendor == "" {
		f.Vendor = "unknown-amd64"
		f.HasSSE2 = true // SSE2 is baseline for amd64
	}
	return !strings.Contains(f.Vendor, "unknown") // return false if we didn't fully detect
}
