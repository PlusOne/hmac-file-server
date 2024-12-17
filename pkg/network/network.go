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

func SetupNetworkMonitoring() {
	go monitorNetwork(context.Background())
	go handleNetworkEvents(context.Background())
}

func monitorNetwork(ctx context.Context) {
	currentIP := getCurrentIPAddress()

	for {
		select {
		case <-ctx.Done():
			return
		default:
			// Monitor network changes
			newIP := getCurrentIPAddress()
			if newIP != currentIP {
				logrus.Infof("IP address changed from %s to %s", currentIP, newIP)
				currentIP = newIP
				// Send network event
			}
			time.Sleep(30 * time.Second)
		}
	}
}

func handleNetworkEvents(ctx context.Context) {
	// Handle network events
	for {
		select {
		case <-ctx.Done():
			return
		default:
			// Process network events
			time.Sleep(1 * time.Minute)
		}
	}
}

func getCurrentIPAddress() string {
	interfaces, err := net.Interfaces()
	if err != nil {
		logrus.Error("Error fetching network interfaces:", err)
		return ""
	}

	for _, iface := range interfaces {
		addrs, err := iface.Addrs()
		if err != nil {
			logrus.Error("Error fetching addresses:", err)
			continue
		}
		for _, addr := range addrs {
			var ip net.IP
			switch v := addr.(type) {
			case *net.IPNet:
				ip = v.IP
			case *net.IPAddr:
				ip = v.IP
			}
			if ip == nil || ip.IsLoopback() {
				continue
			}
			ip = ip.To4()
			if ip == nil {
				continue
			}
			return ip.String()
		}
	}
	return ""
}