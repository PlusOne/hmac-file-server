package cpufeatures

import (
	"testing"
)

func TestDetect(t *testing.T) {
	f := Detect()
	if f == nil {
		t.Fatal("Detect() returned nil")
	}
	t.Logf("Arch:        %s", f.Arch)
	t.Logf("Vendor:      %s", f.Vendor)
	t.Logf("Brand:       %s", f.BrandName)
	t.Logf("Summary:     %s", f.Summary())
	t.Logf("Compression: tier=%s, recommended=%s", f.CompressionTier(), f.RecommendedCompression())
	t.Logf("Extensions:  %v", f.SupportedExtensions())

	// On amd64/arm64, at minimum SSE2 or AES should be detected
	switch f.Arch {
	case "amd64":
		if !f.HasSSE2 {
			t.Error("SSE2 should be present on amd64")
		}
	case "arm64":
		// ARM64 typically has AES crypto extensions
		t.Logf("ARM64 AES: %v", f.HasAESNI)
	}
}

func TestCompressionTier(t *testing.T) {
	tests := []struct {
		name     string
		features Features
		wantTier string
	}{
		{
			name:     "optimal - BMI2 + AVX2",
			features: Features{HasBMI2: true, HasAVX2: true, HasSSE2: true},
			wantTier: "optimal",
		},
		{
			name:     "good - BMI2 + SSE4.2",
			features: Features{HasBMI2: true, HasSSE42: true, HasSSE2: true},
			wantTier: "good",
		},
		{
			name:     "baseline - SSE2 only",
			features: Features{HasSSE2: true},
			wantTier: "baseline",
		},
		{
			name:     "minimal - nothing",
			features: Features{},
			wantTier: "minimal",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.features.CompressionTier()
			if got != tt.wantTier {
				t.Errorf("CompressionTier() = %q, want %q", got, tt.wantTier)
			}
		})
	}
}

func TestRecommendedCompression(t *testing.T) {
	optimal := Features{HasBMI2: true, HasAVX2: true, HasSSE42: true}
	if got := optimal.RecommendedCompression(); got != "zstd" {
		t.Errorf("expected zstd, got %s", got)
	}

	baseline := Features{HasSSE2: true}
	if got := baseline.RecommendedCompression(); got != "gzip" {
		t.Errorf("expected gzip, got %s", got)
	}
}

func TestSummary(t *testing.T) {
	f := &Features{
		HasAESNI: true,
		HasSSE42: true,
		HasAVX2:  true,
		HasBMI2:  true,
		Vendor:   "GenuineIntel",
	}
	s := f.Summary()
	if s == "" {
		t.Error("Summary() should not be empty")
	}
	t.Logf("Summary: %s", s)

	// Verify it contains the expected extensions
	for _, ext := range []string{"AES-NI", "SSE4.2", "AVX2", "BMI2"} {
		if !contains(s, ext) {
			t.Errorf("Summary should contain %s, got: %s", ext, s)
		}
	}
}

func contains(s, sub string) bool {
	return len(s) >= len(sub) && searchString(s, sub)
}

func searchString(s, sub string) bool {
	for i := 0; i <= len(s)-len(sub); i++ {
		if s[i:i+len(sub)] == sub {
			return true
		}
	}
	return false
}

func TestZstdOptimal(t *testing.T) {
	cases := []struct {
		name string
		f    Features
		want bool
	}{
		{"BMI2+AVX2", Features{HasBMI2: true, HasAVX2: true}, true},
		{"BMI2+SSE42", Features{HasBMI2: true, HasSSE42: true}, true},
		{"BMI2 only", Features{HasBMI2: true}, false},
		{"AVX2 only", Features{HasAVX2: true}, false},
		{"nothing", Features{}, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.f.ZstdOptimal(); got != tc.want {
				t.Errorf("ZstdOptimal() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestCryptoAccelerated(t *testing.T) {
	f := &Features{HasAESNI: true}
	if !f.CryptoAccelerated() {
		t.Error("CryptoAccelerated should be true when HasAESNI is set")
	}

	f2 := &Features{}
	if f2.CryptoAccelerated() {
		t.Error("CryptoAccelerated should be false when HasAESNI is not set")
	}
}
