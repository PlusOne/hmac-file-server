package main

import (
	"os"
	"os/exec"
	"strings"
	"testing"
)

// TestGenConfigFlag runs the server with --genconfig and checks output for expected config keys
func TestGenConfigFlag(t *testing.T) {
	cmd := exec.Command("go", "run", "../cmd/server/main.go", "--genconfig")
	output, err := cmd.CombinedOutput()
	if err != nil && !strings.Contains(string(output), "[server]") {
		t.Fatalf("Failed to run with --genconfig: %v\nOutput: %s", err, output)
	}
	if !strings.Contains(string(output), "[server]") || !strings.Contains(string(output), "bind_ip") {
		t.Errorf("Example config missing expected keys. Output: %s", output)
	}
}

// TestIPv4IPv6Flag runs the server with forceprotocol=ipv4 and ipv6 and checks for startup errors
func TestIPv4IPv6Flag(t *testing.T) {
	for _, proto := range []string{"ipv4", "ipv6", "auto"} {
		cmd := exec.Command("go", "run", "../cmd/server/main.go", "--config", "../cmd/server/config.toml")
		cmd.Env = append(os.Environ(), "FORCEPROTOCOL="+proto)
		// Set Go module cache environment variables if not already set
		if os.Getenv("GOMODCACHE") == "" {
			cmd.Env = append(cmd.Env, "GOMODCACHE="+os.Getenv("HOME")+"/go/pkg/mod")
		}
		if os.Getenv("GOPATH") == "" {
			cmd.Env = append(cmd.Env, "GOPATH="+os.Getenv("HOME")+"/go")
		}
		output, err := cmd.CombinedOutput()
		if err != nil && !strings.Contains(string(output), "Configuration loaded successfully") {
			t.Errorf("Server failed to start with forceprotocol=%s: %v\nOutput: %s", proto, err, output)
		}
	}
}
