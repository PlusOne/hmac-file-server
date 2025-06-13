#!/bin/bash

# HMAC File Server v3.2 - Installation Verification Script
# Run this script on your production server to verify the installation

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${BLUE}🔍 HMAC File Server v3.2 - Installation Verification${NC}"
echo "======================================================"
echo ""

# Check if running as root
if [[ $EUID -ne 0 ]]; then
   echo -e "${RED}❌ This script must be run as root (use sudo)${NC}"
   exit 1
fi

ERRORS=0
WARNINGS=0

# Function to report status
report_status() {
    local status=$1
    local message=$2
    local details=$3
    
    case $status in
        "OK")
            echo -e "${GREEN}✅ $message${NC}"
            [[ -n "$details" ]] && echo -e "   ${CYAN}$details${NC}"
            ;;
        "WARNING")
            echo -e "${YELLOW}⚠️  $message${NC}"
            [[ -n "$details" ]] && echo -e "   ${YELLOW}$details${NC}"
            ((WARNINGS++))
            ;;
        "ERROR")
            echo -e "${RED}❌ $message${NC}"
            [[ -n "$details" ]] && echo -e "   ${RED}$details${NC}"
            ((ERRORS++))
            ;;
        "INFO")
            echo -e "${CYAN}ℹ️  $message${NC}"
            [[ -n "$details" ]] && echo -e "   $details"
            ;;
    esac
}

# 1. Check SystemD Service Status
echo -e "${YELLOW}🔧 Checking SystemD Service...${NC}"
if systemctl is-active --quiet hmac-file-server; then
    service_status=$(systemctl status hmac-file-server --no-pager -l | head -10)
    uptime=$(systemctl show hmac-file-server --property=ActiveEnterTimestamp --value)
    report_status "OK" "HMAC File Server service is running" "Active since: $uptime"
else
    service_status=$(systemctl status hmac-file-server --no-pager -l | head -10)
    report_status "ERROR" "HMAC File Server service is not running" "$service_status"
fi

if systemctl is-enabled --quiet hmac-file-server; then
    report_status "OK" "Service is enabled (will start on boot)"
else
    report_status "WARNING" "Service is not enabled for auto-start"
fi

echo ""

# 2. Check Installation Files
echo -e "${YELLOW}📁 Checking Installation Files...${NC}"

# Binary
if [[ -f "/opt/hmac-file-server/hmac-file-server" ]]; then
    binary_info=$(ls -lh /opt/hmac-file-server/hmac-file-server)
    report_status "OK" "Binary installed" "$binary_info"
    
    # Check if binary has version flag (indicates correct build)
    if /opt/hmac-file-server/hmac-file-server --version >/dev/null 2>&1; then
        version=$(/opt/hmac-file-server/hmac-file-server --version 2>/dev/null || echo "Unknown")
        report_status "OK" "Binary supports --version flag" "Version: $version"
    else
        report_status "WARNING" "Binary doesn't support --version flag (may be old build)"
    fi
else
    report_status "ERROR" "Binary not found at /opt/hmac-file-server/hmac-file-server"
fi

# Configuration
if [[ -f "/etc/hmac-file-server/config.toml" ]]; then
    config_info=$(ls -lh /etc/hmac-file-server/config.toml)
    report_status "OK" "Configuration file exists" "$config_info"
else
    report_status "ERROR" "Configuration file not found at /etc/hmac-file-server/config.toml"
fi

# Data directories
data_dirs=("/var/lib/hmac-file-server" "/var/log/hmac-file-server")
for dir in "${data_dirs[@]}"; do
    if [[ -d "$dir" ]]; then
        dir_info=$(ls -lhd "$dir")
        report_status "OK" "Directory exists: $dir" "$dir_info"
    else
        report_status "WARNING" "Directory missing: $dir"
    fi
done

echo ""

# 3. Check Configuration Validation
echo -e "${YELLOW}⚙️  Checking Configuration Validation...${NC}"
if [[ -f "/opt/hmac-file-server/hmac-file-server" ]]; then
    echo -e "${CYAN}Running configuration validation...${NC}"
    
    # Run validation with timeout
    if timeout 30s /opt/hmac-file-server/hmac-file-server -config /etc/hmac-file-server/config.toml --validate-config >/tmp/hmac_validation.log 2>&1; then
        report_status "OK" "Configuration validation passed"
        
        # Check for warnings in validation output
        if grep -q "WARNING\|WARN" /tmp/hmac_validation.log; then
            warning_count=$(grep -c "WARNING\|WARN" /tmp/hmac_validation.log)
            report_status "WARNING" "Configuration validation has $warning_count warnings" "Check logs for details"
        fi
    else
        validation_error=$(tail -5 /tmp/hmac_validation.log)
        report_status "ERROR" "Configuration validation failed" "$validation_error"
    fi
    
    rm -f /tmp/hmac_validation.log
fi

echo ""

# 4. Check Network Connectivity
echo -e "${YELLOW}🌐 Checking Network Connectivity...${NC}"

# Extract ports from config
if [[ -f "/etc/hmac-file-server/config.toml" ]]; then
    server_port=$(grep -E "^listenport\s*=" /etc/hmac-file-server/config.toml | cut -d'"' -f2 | tr -d '"' || echo "8080")
    metrics_port=$(grep -E "^metricsport\s*=" /etc/hmac-file-server/config.toml | cut -d'"' -f2 | tr -d '"' || echo "9090")
    
    # Check if ports are listening
    if netstat -tln 2>/dev/null | grep -q ":$server_port "; then
        report_status "OK" "Server port $server_port is listening"
    else
        report_status "ERROR" "Server port $server_port is not listening"
    fi
    
    if netstat -tln 2>/dev/null | grep -q ":$metrics_port "; then
        report_status "OK" "Metrics port $metrics_port is listening"
    else
        report_status "WARNING" "Metrics port $metrics_port is not listening"
    fi
    
    # Test HTTP connectivity
    if curl -s --connect-timeout 5 "http://localhost:$server_port" >/dev/null 2>&1; then
        report_status "OK" "HTTP server responding on port $server_port"
    elif curl -s --connect-timeout 5 "http://localhost:$server_port" 2>&1 | grep -q "404\|401\|403"; then
        report_status "OK" "HTTP server responding (expected auth required)"
    else
        report_status "WARNING" "HTTP server not responding on port $server_port"
    fi
fi

echo ""

# 5. Check System Resources
echo -e "${YELLOW}💾 Checking System Resources...${NC}"

# Memory usage
memory_usage=$(ps -o pid,ppid,cmd,%mem,%cpu --sort=-%mem -C hmac-file-server | tail -n +2)
if [[ -n "$memory_usage" ]]; then
    report_status "OK" "Process running and using resources" "$memory_usage"
else
    report_status "WARNING" "No process information available"
fi

# Disk space
storage_path=$(grep -E "^storagepath\s*=" /etc/hmac-file-server/config.toml 2>/dev/null | cut -d'"' -f2 | tr -d '"' || echo "/var/lib/hmac-file-server")
if [[ -d "$storage_path" ]]; then
    disk_usage=$(df -h "$storage_path" | tail -1)
    report_status "INFO" "Storage directory disk usage" "$disk_usage"
fi

echo ""

# 6. Check Logs
echo -e "${YELLOW}📋 Checking Recent Logs...${NC}"

# SystemD logs
recent_logs=$(journalctl -u hmac-file-server --since "5 minutes ago" --no-pager -q)
if [[ -n "$recent_logs" ]]; then
    report_status "INFO" "Recent SystemD logs available"
    echo -e "${CYAN}Last 5 log entries:${NC}"
    echo "$recent_logs" | tail -5
else
    report_status "INFO" "No recent SystemD logs (service may be stable)"
fi

echo ""

# 7. Final Summary
echo -e "${BLUE}📊 Verification Summary${NC}"
echo "========================"

if [[ $ERRORS -eq 0 && $WARNINGS -eq 0 ]]; then
    echo -e "${GREEN}🎉 PERFECT! HMAC File Server installation is working correctly!${NC}"
    echo -e "${GREEN}   No errors or warnings found.${NC}"
elif [[ $ERRORS -eq 0 ]]; then
    echo -e "${YELLOW}✅ GOOD! HMAC File Server is working with $WARNINGS warning(s).${NC}"
    echo -e "${YELLOW}   Review warnings above for optimization opportunities.${NC}"
else
    echo -e "${RED}❌ ISSUES FOUND! $ERRORS error(s) and $WARNINGS warning(s) detected.${NC}"
    echo -e "${RED}   Please address the errors above before using in production.${NC}"
fi

echo ""
echo -e "${CYAN}💡 Additional Checks You Can Perform:${NC}"
echo "   • Test file upload: curl -X POST -F \"file=@testfile.txt\" http://localhost:$server_port/"
echo "   • Check metrics: curl http://localhost:$metrics_port/metrics"
echo "   • Review full logs: journalctl -u hmac-file-server -f"
echo "   • Test configuration: /opt/hmac-file-server/hmac-file-server --validate-config"

exit $ERRORS
