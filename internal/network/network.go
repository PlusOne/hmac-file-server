// internal/network/network.go

package network

import (
	"context"
	"net"
	"strings"
	"time"

	"your-project/internal/config"
	"your-project/internal/logging"
)

type NetworkEvent struct {
	Type    string
	Details string
}

var (
	NetworkEvents chan NetworkEvent
)

// MonitorNetwork überwacht Netzwerkänderungen
func MonitorNetwork(ctx context.Context) {
	NetworkEvents = make(chan NetworkEvent, 100)
	currentIP := getCurrentIPAddress()

	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			logging.Log.Info("Stopping network monitor.")
			close(NetworkEvents)
			return
		case <-ticker.C:
			newIP := getCurrentIPAddress()
			if newIP != currentIP && newIP != "" {
				currentIP = newIP
				select {
				case NetworkEvents <- NetworkEvent{Type: "IP_CHANGE", Details: currentIP}:
					logging.Log.WithField("new_ip", currentIP).Info("Queued IP_CHANGE event")
				default:
					logging.Log.Warn("Network event channel is full. Dropping IP_CHANGE event.")
				}
			}
		}
	}
}

// HandleNetworkEvents verarbeitet Netzwerkereignisse
func HandleNetworkEvents(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			logging.Log.Info("Stopping network event handler.")
			return
		case event, ok := <-NetworkEvents:
			if !ok {
				logging.Log.Info("Network events channel closed.")
				return
			}
			switch event.Type {
			case "IP_CHANGE":
				logging.Log.WithField("new_ip", event.Details).Info("Network change detected")
				// Beispiel: Update Prometheus gauge oder trigger alerts
				// activeConnections.Set(float64(getActiveConnections()))
			}
			// Weitere Ereignistypen können hier behandelt werden
		}
	}
}

// getCurrentIPAddress ermittelt die aktuelle IP-Adresse
func getCurrentIPAddress() string {
	interfaces, err := net.Interfaces()
	if err != nil {
		logging.Log.WithError(err).Error("Failed to get network interfaces")
		return ""
	}

	for _, iface := range interfaces {
		if iface.Flags&net.FlagUp == 0 || iface.Flags&net.FlagLoopback != 0 {
			continue // Skip interfaces that are down or loopback
		}
		addrs, err := iface.Addrs()
		if err != nil {
			logging.Log.WithError(err).Errorf("Failed to get addresses for interface %s", iface.Name)
			continue
		}
		for _, addr := range addrs {
			if ipnet, ok := addr.(*net.IPNet); ok && ipnet.IP.IsGlobalUnicast() && ipnet.IP.To4() != nil {
				return ipnet.IP.String()
			}
		}
	}
	return ""
}
