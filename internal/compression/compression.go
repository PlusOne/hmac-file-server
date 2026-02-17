// Package compression provides CPU-aware compression algorithm selection
// and configuration.  It uses the cpufeatures package to detect hardware
// ISA extensions and automatically chooses the best compression parameters.
//
// Key relationships between ISA extensions and compression:
//
//   - BMI2 (PEXT/PDEP):  zstd's entropy coders (HUF_decompress, FSE_decompress)
//     use PEXT for fast bit extraction.  Without BMI2, zstd falls back to
//     shift-and-mask sequences that are 2-4x slower.
//
//   - SSE4.2 / AVX2:  XXHash (zstd's checksum) and data scanning/matching
//     loops use SSE4.2 CRC32c and AVX2 SIMD for parallel comparisons.
//
//   - AES-NI:  Not directly used by compression, but critical for TLS and
//     HMAC operations running in parallel with I/O.  Hardware AES frees
//     CPU cycles for compression.
//
//   - AVX-512:  Some zstd builds and newer gzip implementations exploit
//     512-bit vectors for even faster scanning and checksumming.
package compression

import (
	"fmt"

	"git.uuxo.net/uuxo/hmac-file-server/internal/cpufeatures"
)

// Algorithm represents a compression algorithm identifier.
type Algorithm string

const (
	AlgoNone Algorithm = "none"
	AlgoGzip Algorithm = "gzip"
	AlgoZstd Algorithm = "zstd"
)

// Profile holds the auto-selected compression settings for the current
// hardware.
type Profile struct {
	// Algorithm is the recommended compression algorithm.
	Algorithm Algorithm

	// Level is the recommended compression level (algorithm-specific).
	// For gzip: 1-9 (6 = default).  For zstd: 1-22 (3 = default).
	Level int

	// Tier is a human-readable label for the CPU capability level.
	Tier string

	// Reason explains why this profile was selected.
	Reason string

	// Features is a reference to the detected CPU features.
	Features *cpufeatures.Features
}

// AutoSelect detects CPU features and returns the optimal compression
// profile for the current hardware.
func AutoSelect() *Profile {
	features := cpufeatures.Detect()
	return SelectForFeatures(features)
}

// SelectForFeatures returns a compression profile based on the given CPU
// features.  This is separated from AutoSelect for testing purposes.
func SelectForFeatures(features *cpufeatures.Features) *Profile {
	tier := features.CompressionTier()

	switch tier {
	case "optimal":
		// BMI2 + AVX2: zstd can run at full speed.
		// Use level 3 (zstd default) — excellent ratio with fast encoding.
		return &Profile{
			Algorithm: AlgoZstd,
			Level:     3,
			Tier:      tier,
			Reason: fmt.Sprintf(
				"BMI2+AVX2 detected — zstd entropy coding (HUF/FSE) uses hardware PEXT/PDEP, "+
					"XXHash checksums use AVX2 SIMD. CPU: %s", features.BrandName),
			Features: features,
		}

	case "good":
		// BMI2 + SSE4.2: zstd PEXT works but checksum parallelism is limited.
		// Use level 1-3 for a good balance.
		return &Profile{
			Algorithm: AlgoZstd,
			Level:     1,
			Tier:      tier,
			Reason: fmt.Sprintf(
				"BMI2+SSE4.2 detected — zstd entropy coding accelerated via PEXT, "+
					"using lower level for optimal throughput. CPU: %s", features.BrandName),
			Features: features,
		}

	case "baseline":
		// SSE2 only: zstd falls back to software bit manipulation (slow PEXT emulation).
		// gzip is more predictable at this tier.
		return &Profile{
			Algorithm: AlgoGzip,
			Level:     6,
			Tier:      tier,
			Reason: fmt.Sprintf(
				"Only SSE2 available — BMI2 missing, zstd entropy coding would use slow "+
					"software fallback. gzip level 6 provides better throughput. CPU: %s", features.BrandName),
			Features: features,
		}

	default: // "minimal"
		// No relevant SIMD at all.  Use gzip at a lower level.
		return &Profile{
			Algorithm: AlgoGzip,
			Level:     4,
			Tier:      tier,
			Reason: fmt.Sprintf(
				"No SIMD extensions detected — using gzip level 4 for acceptable "+
					"throughput without hardware acceleration. Arch: %s", features.Arch),
			Features: features,
		}
	}
}

// String returns a human-readable description of the compression profile.
func (p *Profile) String() string {
	return fmt.Sprintf("algorithm=%s level=%d tier=%s reason=(%s)",
		p.Algorithm, p.Level, p.Tier, p.Reason)
}

// ISAImpactTable returns a formatted string documenting the relationship
// between ISA extensions and compression/crypto performance.  Suitable
// for inclusion in startup logs or diagnostic output.
func ISAImpactTable(features *cpufeatures.Features) string {
	type row struct {
		Extension string
		Present   bool
		Impact    string
		IntelName string
		AMDName   string
	}

	rows := []row{
		{
			Extension: "AES-NI",
			Present:   features.HasAESNI,
			Impact:    "HMAC-SHA256 & TLS: 10x faster encrypt/decrypt, side-channel protection",
			IntelName: "Intel AES-NI",
			AMDName:   "AMD AES",
		},
		{
			Extension: "SSE4.2",
			Present:   features.HasSSE42,
			Impact:    "CRC32c checksums, string comparisons — used by XXHash in zstd",
			IntelName: "Intel SSE4.2",
			AMDName:   "AMD SSE4.2",
		},
		{
			Extension: "AVX2",
			Present:   features.HasAVX2,
			Impact:    "256-bit SIMD: parallel data scanning, checksum computation",
			IntelName: "Intel AVX2",
			AMDName:   "AMD AVX2",
		},
		{
			Extension: "AVX-512",
			Present:   features.HasAVX512,
			Impact:    "512-bit SIMD: maximum throughput for checksum and matching",
			IntelName: "Intel AVX-512",
			AMDName:   "AMD AVX-512 (Zen 4+)",
		},
		{
			Extension: "BMI2",
			Present:   features.HasBMI2,
			Impact:    "PEXT/PDEP: critical for zstd HUF/FSE entropy coding (2-4x speedup)",
			IntelName: "Intel BMI2",
			AMDName:   "AMD BMI2 (Zen 3+)",
		},
		{
			Extension: "SHA-NI",
			Present:   features.HasSHANI,
			Impact:    "Hardware SHA-256: accelerates HMAC signature verification",
			IntelName: "Intel SHA Ext",
			AMDName:   "AMD SHA (Zen+)",
		},
		{
			Extension: "CLMUL",
			Present:   features.HasCLMUL,
			Impact:    "Carry-less multiply: GCM mode AES, CRC computation",
			IntelName: "Intel PCLMULQDQ",
			AMDName:   "AMD CLMUL",
		},
	}

	result := "CPU ISA Extension Impact Analysis:\n"
	result += fmt.Sprintf("%-10s %-9s %-15s %-22s %s\n",
		"Extension", "Detected", "Intel Name", "AMD Name", "Performance Impact")
	result += "---------------------------------------------------------------------------------------------------\n"

	for _, r := range rows {
		status := "NO"
		if r.Present {
			status = "YES"
		}
		result += fmt.Sprintf("%-10s %-9s %-15s %-22s %s\n",
			r.Extension, status, r.IntelName, r.AMDName, r.Impact)
	}

	return result
}
