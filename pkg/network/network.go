
package network

import (
	"context"
	"net"
	"time"

	"github.com/sirupsen/logrus"
)

type NetworkEvent struct {
	Type    string
	Details string
}

var networkEvents chan NetworkEvent

func SetupNetworkMonitoring(ctx context.Context, events chan NetworkEvent) {
	networkEvents = events
	go monitorNetwork(ctx)
	go handleNetworkEvents(ctx)
}

func monitorNetwork(ctx context.Context) {
	currentIP := getCurrentIPAddress()

	for {
		select {
		case <-ctx.Done():
			logrus.Info("Stopping network monitor.")
			return
		case <-time.After(10 * time.Second):
			newIP := getCurrentIPAddress()
			if newIP != currentIP && newIP != "" {
				currentIP = newIP
				select {
				case networkEvents <- NetworkEvent{Type: "IP_CHANGE", Details: currentIP}:
					logrus.WithField("new_ip", currentIP).Info("Queued IP_CHANGE event")
				default:
					logrus.Warn("Network event channel full. Dropping IP_CHANGE event.")
				}
			}
		}
	}
}

func handleNetworkEvents(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			logrus.Info("Stopping network event handler.")
			return
		case event, ok := <-networkEvents:
			if !ok {
				logrus.Info("Network events channel closed.")
				return
			}
			switch event.Type {
			case "IP_CHANGE":
				logrus.WithField("new_ip", event.Details).Info("Network change detected")
			}
		}
	}
}

func getCurrentIPAddress() string {
	interfaces, err := net.Interfaces()
	if err != nil {
		logrus.WithError(err).Error("Failed to get network interfaces")
		return ""
	}

	for _, iface := range interfaces {
		if iface.Flags&net.FlagUp == 0 || iface.Flags&net.FlagLoopback != 0 {
			continue
		}
		addrs, err := iface.Addrs()
		if err != nil {
			logrus.WithError(err).Errorf("Failed to get addresses for interface %s", iface.Name)
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