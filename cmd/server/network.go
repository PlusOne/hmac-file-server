package main

import (
    "net"
    "log"
)

func getCurrentIPAddress() string {
    addrs, err := net.InterfaceAddrs()
    if err != nil {
        log.Fatalf("Failed to get network interfaces: %v", err)
    }
    for _, addr := range addrs {
        if ipNet, ok := addr.(*net.IPNet); ok && !ipNet.IP.IsLoopback() {
            if ipNet.IP.To4() != nil {
                return ipNet.IP.String()
            }
        }
    }
    return "127.0.0.1"
}
