// Package network provides network protocol initialization and resilience.
package network

import (
	"fmt"
	"net"
	"syscall"
	"time"

	"github.com/sirupsen/logrus"
)

var log = logrus.New()

// SetLogger replaces the package-level logger.
func SetLogger(l *logrus.Logger) { log = l }

// InitializeNetworkProtocol sets up the network dialer based on the force protocol setting.
func InitializeNetworkProtocol(forceProtocol string) (*net.Dialer, error) {
	if forceProtocol == "" {
		forceProtocol = "auto"
	}

	switch forceProtocol {
	case "ipv4":
		return &net.Dialer{
			Timeout:   5 * time.Second,
			DualStack: false,
			Control: func(network, address string, c syscall.RawConn) error {
				if network == "tcp6" {
					return fmt.Errorf("IPv6 is disabled by forceprotocol setting")
				}
				return nil
			},
		}, nil
	case "ipv6":
		return &net.Dialer{
			Timeout:   5 * time.Second,
			DualStack: false,
			Control: func(network, address string, c syscall.RawConn) error {
				if network == "tcp4" {
					return fmt.Errorf("IPv4 is disabled by forceprotocol setting")
				}
				return nil
			},
		}, nil
	case "auto":
		return &net.Dialer{
			Timeout:   5 * time.Second,
			DualStack: true,
		}, nil
	default:
		return nil, fmt.Errorf("invalid forceprotocol value: %s", forceProtocol)
	}
}
