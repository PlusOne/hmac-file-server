#!/bin/bash
# Live debugging script for HMAC File Server upload issues
# Monitors logs in real-time and provides detailed diagnostics

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Function to check service status
check_services() {
    log_info "=== SERVICE STATUS CHECK ==="
    
    echo "HMAC File Server:"
    systemctl is-active hmac-file-server && echo "✅ Running" || echo "❌ Not running"
    
    echo "Nginx:"
    systemctl is-active nginx && echo "✅ Running" || echo "❌ Not running"
    
    echo ""
}

# Function to show current configuration
show_config() {
    log_info "=== CONFIGURATION SUMMARY ==="
    
    echo "HMAC File Server Config:"
    echo "- Max Upload Size: $(grep max_upload_size /opt/hmac-file-server/config.toml | cut -d'"' -f2)"
    echo "- Chunk Size: $(grep chunksize /opt/hmac-file-server/config.toml | head -1 | cut -d'"' -f2)"
    echo "- Chunked Uploads: $(grep chunkeduploadsenabled /opt/hmac-file-server/config.toml | cut -d'=' -f2 | tr -d ' ')"
    echo "- Network Events: $(grep networkevents /opt/hmac-file-server/config.toml | cut -d'=' -f2 | tr -d ' ')"
    echo "- Listen Address: $(grep listen_address /opt/hmac-file-server/config.toml | cut -d'"' -f2)"
    
    echo ""
    echo "Nginx Config:"
    echo "- Client Max Body Size: $(nginx -T 2>/dev/null | grep client_max_body_size | head -1 | awk '{print $2}' | tr -d ';')"
    echo "- Proxy Buffering: $(nginx -T 2>/dev/null | grep proxy_request_buffering | head -1 | awk '{print $2}' | tr -d ';')"
    echo "- Proxy Timeouts: $(nginx -T 2>/dev/null | grep proxy_read_timeout | head -1 | awk '{print $2}' | tr -d ';')"
    
    echo ""
}

# Function to monitor logs in real-time
monitor_logs() {
    log_info "=== STARTING LIVE LOG MONITORING ==="
    log_warning "Press Ctrl+C to stop monitoring"
    echo ""
    
    # Create named pipes for log monitoring
    mkfifo /tmp/hmac_logs /tmp/nginx_logs 2>/dev/null || true
    
    # Start log monitoring in background
    journalctl -u hmac-file-server -f --no-pager > /tmp/hmac_logs &
    HMAC_PID=$!
    
    tail -f /var/log/nginx/access.log > /tmp/nginx_logs &
    NGINX_PID=$!
    
    # Monitor both logs with timestamps
    {
        while read line; do
            echo -e "${BLUE}[HMAC]${NC} $line"
        done < /tmp/hmac_logs &
        
        while read line; do
            if [[ "$line" =~ (PUT|POST) ]] && [[ "$line" =~ (40[0-9]|50[0-9]) ]]; then
                echo -e "${RED}[NGINX-ERROR]${NC} $line"
            elif [[ "$line" =~ (PUT|POST) ]]; then
                echo -e "${GREEN}[NGINX-OK]${NC} $line"
            else
                echo -e "${YELLOW}[NGINX]${NC} $line"
            fi
        done < /tmp/nginx_logs &
        
        wait
    }
    
    # Cleanup on exit
    trap 'kill $HMAC_PID $NGINX_PID 2>/dev/null; rm -f /tmp/hmac_logs /tmp/nginx_logs' EXIT
}

# Function to test file upload
test_upload() {
    local test_file="$1"
    local test_size="${2:-1MB}"
    
    if [ -z "$test_file" ]; then
        test_file="/tmp/test_upload_${test_size}.bin"
        log_info "Creating test file: $test_file ($test_size)"
        
        case "$test_size" in
            "1MB") dd if=/dev/urandom of="$test_file" bs=1M count=1 >/dev/null 2>&1 ;;
            "10MB") dd if=/dev/urandom of="$test_file" bs=1M count=10 >/dev/null 2>&1 ;;
            "100MB") dd if=/dev/urandom of="$test_file" bs=1M count=100 >/dev/null 2>&1 ;;
            "1GB") dd if=/dev/urandom of="$test_file" bs=1M count=1024 >/dev/null 2>&1 ;;
        esac
        
        log_success "Test file created: $(ls -lh $test_file | awk '{print $5}')"
    fi
    
    # Get current timestamp for log filtering
    log_info "=== TESTING UPLOAD: $test_file ==="
    
    # Test with curl - simulate XMPP client behavior
    local url="https://share.uuxo.net/test_path/test_file_$(date +%s).bin"
    
    log_info "Testing upload to: $url"
    
    curl -X PUT \
         -H "Content-Type: application/octet-stream" \
         -H "User-Agent: TestClient/1.0" \
         --data-binary "@$test_file" \
         "$url" \
         -v \
         -w "Response: %{http_code}, Size: %{size_upload}, Time: %{time_total}s\n" \
         2>&1 | tee /tmp/curl_test.log
    
    echo ""
    log_info "Upload test completed. Check logs above for details."
}

# Function to analyze recent errors
analyze_errors() {
    log_info "=== ERROR ANALYSIS ==="
    
    echo "Recent 400 errors from Nginx:"
    tail -100 /var/log/nginx/access.log | grep " 400 " | tail -5
    
    echo ""
    echo "Recent HMAC file server errors:"
    tail -100 /opt/hmac-file-server/data/logs/hmac-file-server.log | grep -i error | tail -5
    
    echo ""
    echo "File extension configuration:"
    grep -A 20 "allowedextensions" /opt/hmac-file-server/config.toml | head -10
    
    echo ""
}

# Function to check file permissions and disk space
check_system() {
    log_info "=== SYSTEM CHECK ==="
    
    echo "Disk space:"
    df -h /opt/hmac-file-server/data/uploads
    
    echo ""
    echo "Upload directory permissions:"
    ls -la /opt/hmac-file-server/data/uploads/
    
    echo ""
    echo "Process information:"
    ps aux | grep hmac-file-server | grep -v grep
    
    echo ""
    echo "Network connections:"
    netstat -tlnp | grep :8080
    
    echo ""
}

# Main menu
main_menu() {
    echo -e "${BLUE}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║${NC}         HMAC File Server Live Debugging Tool              ${BLUE}║${NC}"
    echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo "1) Check service status"
    echo "2) Show configuration summary"
    echo "3) Start live log monitoring"
    echo "4) Test file upload (1MB)"
    echo "5) Test file upload (10MB)"
    echo "6) Test file upload (100MB)"
    echo "7) Analyze recent errors"
    echo "8) Check system resources"
    echo "9) Full diagnostic run"
    echo "0) Exit"
    echo ""
    read -p "Choose an option [0-9]: " choice
    
    case $choice in
        1) check_services ;;
        2) show_config ;;
        3) monitor_logs ;;
        4) test_upload "" "1MB" ;;
        5) test_upload "" "10MB" ;;
        6) test_upload "" "100MB" ;;
        7) analyze_errors ;;
        8) check_system ;;
        9) 
            check_services
            show_config
            check_system
            analyze_errors
            ;;
        0) exit 0 ;;
        *) log_error "Invalid option. Please choose 0-9." ;;
    esac
    
    echo ""
    read -p "Press Enter to continue..."
    main_menu
}

# Handle command line arguments
case "${1:-}" in
    "monitor") monitor_logs ;;
    "test") test_upload "$2" "$3" ;;
    "analyze") analyze_errors ;;
    "status") check_services ;;
    "config") show_config ;;
    "system") check_system ;;
    *) main_menu ;;
esac
