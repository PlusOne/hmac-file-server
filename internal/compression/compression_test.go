package compression

import (
	"strings"
	"testing"

	"git.uuxo.net/uuxo/hmac-file-server/internal/cpufeatures"
)

func TestAutoSelect(t *testing.T) {
	p := AutoSelect()
	if p == nil {
		t.Fatal("AutoSelect() returned nil")
	}
	t.Logf("Profile: %s", p)
	if p.Algorithm != AlgoGzip && p.Algorithm != AlgoZstd {
		t.Errorf("unexpected algorithm: %s", p.Algorithm)
	}
}

func TestSelectForFeatures(t *testing.T) {
	tests := []struct {
		name     string
		features cpufeatures.Features
		wantAlgo Algorithm
		wantTier string
	}{
		{
			name:     "optimal hardware",
			features: cpufeatures.Features{HasBMI2: true, HasAVX2: true, HasSSE42: true, HasSSE2: true},
			wantAlgo: AlgoZstd,
			wantTier: "optimal",
		},
		{
			name:     "good hardware",
			features: cpufeatures.Features{HasBMI2: true, HasSSE42: true, HasSSE2: true},
			wantAlgo: AlgoZstd,
			wantTier: "good",
		},
		{
			name:     "baseline hardware",
			features: cpufeatures.Features{HasSSE2: true},
			wantAlgo: AlgoGzip,
			wantTier: "baseline",
		},
		{
			name:     "minimal hardware",
			features: cpufeatures.Features{},
			wantAlgo: AlgoGzip,
			wantTier: "minimal",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := SelectForFeatures(&tt.features)
			if p.Algorithm != tt.wantAlgo {
				t.Errorf("Algorithm = %s, want %s", p.Algorithm, tt.wantAlgo)
			}
			if p.Tier != tt.wantTier {
				t.Errorf("Tier = %s, want %s", p.Tier, tt.wantTier)
			}
			if p.Level < 1 {
				t.Error("Level should be >= 1")
			}
			if p.Reason == "" {
				t.Error("Reason should not be empty")
			}
		})
	}
}

func TestISAImpactTable(t *testing.T) {
	f := &cpufeatures.Features{
		HasAESNI:  true,
		HasSSE42:  true,
		HasAVX2:   true,
		HasBMI2:   true,
		HasSHANI:  false,
		HasAVX512: false,
	}
	table := ISAImpactTable(f)
	if table == "" {
		t.Error("ISAImpactTable should not return empty string")
	}
	t.Logf("\n%s", table)

	if !strings.Contains(table, "AES-NI") {
		t.Error("table should contain AES-NI")
	}
	if !strings.Contains(table, "BMI2") {
		t.Error("table should contain BMI2")
	}
	if !strings.Contains(table, "YES") {
		t.Error("table should contain YES for detected features")
	}
	if !strings.Contains(table, "NO") {
		t.Error("table should contain NO for missing features")
	}
}

func TestProfileString(t *testing.T) {
	p := &Profile{
		Algorithm: AlgoZstd,
		Level:     3,
		Tier:      "optimal",
		Reason:    "test reason",
	}
	s := p.String()
	if !strings.Contains(s, "zstd") || !strings.Contains(s, "optimal") {
		t.Errorf("String() should contain algorithm and tier: %s", s)
	}
}
