// cpuid_other.go — stub for non-amd64 architectures.

//go:build !amd64

package cpufeatures

// detectX86CPUID is a no-op on non-amd64 targets.
func detectX86CPUID(f *Features) bool {
	return false
}
