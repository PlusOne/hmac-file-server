// Package cpufeatures detects CPU instruction set extensions (ISA) relevant
// for cryptographic operations, compression performance, and general SIMD
// throughput.  It exposes a single Features struct that can be queried at
// startup and used to guide algorithm selection (e.g. zstd vs gzip) and to
// surface hardware capability information in logs and metrics.
//
// Detected feature families:
//   - AES-NI   – hardware-accelerated AES encrypt/decrypt
//   - SSE      – SSE2 through SSE4.2
//   - AVX      – AVX, AVX2, AVX-512 (F subset)
//   - BMI      – BMI1, BMI2 (bit manipulation, used by zstd/FSE)
//   - SHA      – SHA-NI (hardware SHA-256)
//   - CLMUL    – carry-less multiplication (used by CRC32, GCM)
//   - POPCNT   – population count
package cpufeatures

import (
	"fmt"
	"os"
	"runtime"
	"strings"
)

// Features holds the detected CPU ISA extension flags.
type Features struct {
	// --- AES / Crypto ---
	HasAESNI  bool // AES New Instructions (Intel AES-NI / AMD AES)
	HasSHANI  bool // SHA-256 hardware acceleration
	HasCLMUL  bool // Carry-less multiply (PCLMULQDQ)
	HasRDRAND bool // Hardware random number generator

	// --- SSE family ---
	HasSSE2  bool // Streaming SIMD Extensions 2
	HasSSE3  bool // SSE3
	HasSSSE3 bool // Supplemental SSE3
	HasSSE41 bool // SSE4.1
	HasSSE42 bool // SSE4.2 (includes CRC32 instruction)

	// --- AVX family ---
	HasAVX    bool // Advanced Vector Extensions (256-bit)
	HasAVX2   bool // AVX2 (256-bit integer SIMD)
	HasAVX512 bool // AVX-512 Foundation

	// --- Bit manipulation ---
	HasBMI1   bool // Bit Manipulation Instruction Set 1
	HasBMI2   bool // BMI2 (PEXT/PDEP – critical for zstd entropy coding)
	HasPOPCNT bool // Population count

	// --- Metadata ---
	Vendor    string // "GenuineIntel", "AuthenticAMD", etc.
	BrandName string // Full CPU brand string
	Family    int
	Model     int
	Stepping  int

	// GOARCH for non-x86 detection
	Arch string
}

// featureNames maps the flag to a human-readable short name used in logs.
var featureNames = []struct {
	name string
	get  func(*Features) bool
}{
	{"AES-NI", func(f *Features) bool { return f.HasAESNI }},
	{"SHA-NI", func(f *Features) bool { return f.HasSHANI }},
	{"CLMUL", func(f *Features) bool { return f.HasCLMUL }},
	{"RDRAND", func(f *Features) bool { return f.HasRDRAND }},
	{"SSE2", func(f *Features) bool { return f.HasSSE2 }},
	{"SSE3", func(f *Features) bool { return f.HasSSE3 }},
	{"SSSE3", func(f *Features) bool { return f.HasSSSE3 }},
	{"SSE4.1", func(f *Features) bool { return f.HasSSE41 }},
	{"SSE4.2", func(f *Features) bool { return f.HasSSE42 }},
	{"AVX", func(f *Features) bool { return f.HasAVX }},
	{"AVX2", func(f *Features) bool { return f.HasAVX2 }},
	{"AVX-512", func(f *Features) bool { return f.HasAVX512 }},
	{"BMI1", func(f *Features) bool { return f.HasBMI1 }},
	{"BMI2", func(f *Features) bool { return f.HasBMI2 }},
	{"POPCNT", func(f *Features) bool { return f.HasPOPCNT }},
}

// cached stores the result of Detect so it is only computed once.
var cached *Features

// Detect reads CPUID information (on x86/x86-64) or falls back to /proc/cpuinfo
// or runtime hints on other architectures.  The result is cached for the
// lifetime of the process.
func Detect() *Features {
	if cached != nil {
		return cached
	}
	f := &Features{Arch: runtime.GOARCH}
	switch runtime.GOARCH {
	case "amd64", "386":
		detectX86(f)
	case "arm64":
		detectARM64(f)
	default:
		// On unknown arches we can't detect ISA extensions.
	}
	cached = f
	return f
}

// SupportedExtensions returns a sorted slice of human-readable names for all
// detected ISA extensions.
func (f *Features) SupportedExtensions() []string {
	var out []string
	for _, fn := range featureNames {
		if fn.get(f) {
			out = append(out, fn.name)
		}
	}
	return out
}

// Summary returns a one-line string suitable for log output, e.g.
// "AES-NI SSE4.2 AVX2 BMI2 POPCNT (GenuineIntel)".
func (f *Features) Summary() string {
	exts := f.SupportedExtensions()
	if len(exts) == 0 {
		return fmt.Sprintf("no ISA extensions detected (%s/%s)", f.Arch, f.Vendor)
	}
	vendor := f.Vendor
	if vendor == "" {
		vendor = f.Arch
	}
	return fmt.Sprintf("%s (%s)", strings.Join(exts, " "), vendor)
}

// CryptoAccelerated returns true when hardware AES acceleration is available.
func (f *Features) CryptoAccelerated() bool {
	return f.HasAESNI
}

// ZstdOptimal returns true when the CPU has the instruction sets that allow
// zstd to run at near-optimal speed: BMI2 for FSE/Huffman entropy coding,
// SSE4.2 or AVX2 for XXHash checksums and data scanning.
func (f *Features) ZstdOptimal() bool {
	return f.HasBMI2 && (f.HasSSE42 || f.HasAVX2)
}

// CompressionTier returns a tier label reflecting the CPU's suitability for
// heavy compression workloads:
//
//	"optimal"   – BMI2 + AVX2 (zstd at full speed)
//	"good"      – BMI2 + SSE4.2 (zstd runs well)
//	"baseline"  – SSE2 only (gzip recommended)
//	"minimal"   – no relevant SIMD (software-only)
func (f *Features) CompressionTier() string {
	switch {
	case f.HasBMI2 && f.HasAVX2:
		return "optimal"
	case f.HasBMI2 && f.HasSSE42:
		return "good"
	case f.HasSSE2:
		return "baseline"
	default:
		return "minimal"
	}
}

// RecommendedCompression returns the compression algorithm recommended for the
// current hardware:
//
//	"zstd"  – when BMI2 and SSE4.2/AVX2 are present
//	"gzip"  – otherwise
func (f *Features) RecommendedCompression() string {
	if f.ZstdOptimal() {
		return "zstd"
	}
	return "gzip"
}

// ––– x86 / x86-64 detection via CPUID ––––––––––––––––––––––––––––––––––––––

// cpuid executes the CPUID instruction.  Implemented in cpuid_amd64.s for
// amd64 or via a fallback that reads /proc/cpuinfo.
func detectX86(f *Features) {
	// Try native CPUID first (only available on amd64 build)
	if detectX86CPUID(f) {
		return
	}
	// Fallback: parse /proc/cpuinfo (works in VMs and containers)
	detectFromProcCPUInfo(f)
}

// detectFromProcCPUInfo parses Linux /proc/cpuinfo for feature flags.
func detectFromProcCPUInfo(f *Features) {
	data, err := os.ReadFile("/proc/cpuinfo")
	if err != nil {
		return
	}
	content := string(data)

	// Parse vendor_id
	for _, line := range strings.Split(content, "\n") {
		if strings.HasPrefix(line, "vendor_id") {
			parts := strings.SplitN(line, ":", 2)
			if len(parts) == 2 {
				f.Vendor = strings.TrimSpace(parts[1])
			}
			break
		}
	}

	// Parse model name
	for _, line := range strings.Split(content, "\n") {
		if strings.HasPrefix(line, "model name") {
			parts := strings.SplitN(line, ":", 2)
			if len(parts) == 2 {
				f.BrandName = strings.TrimSpace(parts[1])
			}
			break
		}
	}

	// Parse flags
	for _, line := range strings.Split(content, "\n") {
		if strings.HasPrefix(line, "flags") {
			parts := strings.SplitN(line, ":", 2)
			if len(parts) != 2 {
				continue
			}
			flags := " " + strings.TrimSpace(parts[1]) + " "
			hasFlag := func(name string) bool {
				return strings.Contains(flags, " "+name+" ")
			}

			f.HasAESNI = hasFlag("aes")
			f.HasSHANI = hasFlag("sha_ni")
			f.HasCLMUL = hasFlag("pclmulqdq")
			f.HasRDRAND = hasFlag("rdrand")
			f.HasSSE2 = hasFlag("sse2")
			f.HasSSE3 = hasFlag("sse3") || hasFlag("pni")
			f.HasSSSE3 = hasFlag("ssse3")
			f.HasSSE41 = hasFlag("sse4_1")
			f.HasSSE42 = hasFlag("sse4_2")
			f.HasAVX = hasFlag("avx")
			f.HasAVX2 = hasFlag("avx2")
			f.HasAVX512 = hasFlag("avx512f")
			f.HasBMI1 = hasFlag("bmi1")
			f.HasBMI2 = hasFlag("bmi2")
			f.HasPOPCNT = hasFlag("popcnt")
			break
		}
	}
}

// detectARM64 checks ARM64 feature flags from /proc/cpuinfo or HWCAP.
func detectARM64(f *Features) {
	f.Vendor = "ARM"

	data, err := os.ReadFile("/proc/cpuinfo")
	if err != nil {
		// On ARM64, AES and SHA are almost universally available (ARMv8 Crypto Extensions)
		f.HasAESNI = true
		f.HasSHANI = true
		return
	}
	content := string(data)

	// Parse CPU implementer / model name
	for _, line := range strings.Split(content, "\n") {
		if strings.HasPrefix(line, "CPU implementer") || strings.HasPrefix(line, "model name") {
			parts := strings.SplitN(line, ":", 2)
			if len(parts) == 2 {
				f.BrandName = strings.TrimSpace(parts[1])
			}
			break
		}
	}

	// Parse Features line
	for _, line := range strings.Split(content, "\n") {
		if strings.HasPrefix(line, "Features") {
			parts := strings.SplitN(line, ":", 2)
			if len(parts) != 2 {
				continue
			}
			flags := " " + strings.TrimSpace(parts[1]) + " "
			hasFlag := func(name string) bool {
				return strings.Contains(flags, " "+name+" ")
			}

			f.HasAESNI = hasFlag("aes")
			f.HasSHANI = hasFlag("sha2") || hasFlag("sha256")
			f.HasCLMUL = hasFlag("pmull")
			// ARM64 doesn't have SSE/AVX but has NEON (ASIMD) which is always present
			// We map ASIMD → SSE2 equivalent for compression tier purposes
			if hasFlag("asimd") {
				f.HasSSE2 = true // NEON/ASIMD provides equivalent SIMD capability
			}
			// CRC32 instruction is common on ARM64
			if hasFlag("crc32") {
				f.HasSSE42 = true // Map CRC32 capability
			}
			break
		}
	}
}
