#!/bin/bash
# HMAC File Server Configuration Consistency Checker
# Ensures all deployment methods use proper configuration structure

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

# Configuration templates to check
CONFIG_LOCATIONS=(
    "/opt/hmac-file-server/config.toml"                    # SystemD
    "./hmac-docker/config/config.toml"                     # Docker
    "/opt/podman/hmac-file-server/config/config.toml"      # Podman
    "/etc/hmac-file-server/config.toml"                   # Debian
    "./config-default.toml"                                # Default template
    "./config-simple.toml"                                 # Simple template
    "./config-simplified-production.toml"                  # Production template
)

# Required sections and fields
REQUIRED_SECTIONS=("server" "security" "uploads" "logging")
REQUIRED_FIELDS=(
    "server.listen_address"
    "server.storage_path"
    "security.secret"
    "uploads.networkevents"
)

NETWORK_RESILIENCE_FIELDS=(
    "network_resilience.enabled"
    "network_resilience.quality_monitoring"
    "network_resilience.upload_resilience"
)

check_config_file() {
    local config_file="$1"
    local config_name="$2"
    local errors=0
    local warnings=0
    
    log_info "Checking $config_name: $config_file"
    
    if [ ! -f "$config_file" ]; then
        log_warning "Configuration file not found (may not be installed)"
        return 0
    fi
    
    # Check for common field naming issues
    if grep -q "storagepath\s*=" "$config_file" 2>/dev/null; then
        log_error "Found 'storagepath' - should be 'storage_path'"
        ((errors++))
    fi
    
    if grep -q "listenport\s*=" "$config_file" 2>/dev/null; then
        log_error "Found 'listenport' - should be 'listen_address'"
        ((errors++))
    fi
    
    if grep -q "metricsenabled\s*=" "$config_file" 2>/dev/null; then
        log_error "Found 'metricsenabled' - should be 'metrics_enabled'"
        ((errors++))
    fi
    
    # Check required sections
    for section in "${REQUIRED_SECTIONS[@]}"; do
        if ! grep -q "^\[$section\]" "$config_file" 2>/dev/null; then
            log_error "Missing required section: [$section]"
            ((errors++))
        fi
    done
    
    # Check required fields
    for field in "${REQUIRED_FIELDS[@]}"; do
        field_name=$(echo "$field" | cut -d'.' -f2)
        if ! grep -q "^$field_name\s*=" "$config_file" 2>/dev/null; then
            log_warning "Missing or commented field: $field_name"
            ((warnings++))
        fi
    done
    
    # Check network resilience
    local has_network_resilience=false
    if grep -q "^\[network_resilience\]" "$config_file" 2>/dev/null; then
        has_network_resilience=true
        log_success "Network resilience section found"
        
        for field in "${NETWORK_RESILIENCE_FIELDS[@]}"; do
            field_name=$(echo "$field" | cut -d'.' -f2)
            if ! grep -q "^$field_name\s*=" "$config_file" 2>/dev/null; then
                log_warning "Missing network resilience field: $field_name"
                ((warnings++))
            fi
        done
    else
        log_warning "Network resilience section missing"
        ((warnings++))
    fi
    
    # Check networkevents setting
    if grep -q "networkevents\s*=\s*true" "$config_file" 2>/dev/null; then
        if [ "$has_network_resilience" = false ]; then
            log_error "networkevents=true but no [network_resilience] section"
            ((errors++))
        fi
    fi
    
    # Validate configuration with binary if available
    if [ -f "./test-hmac-file-server" ]; then
        log_info "Validating configuration syntax..."
        if ./test-hmac-file-server -config "$config_file" --validate-config >/dev/null 2>&1; then
            log_success "Configuration validation passed"
        else
            log_warning "Configuration has validation warnings"
            ((warnings++))
        fi
    fi
    
    # Summary for this config
    if [ $errors -eq 0 ] && [ $warnings -eq 0 ]; then
        log_success "$config_name: Perfect configuration"
    elif [ $errors -eq 0 ]; then
        log_warning "$config_name: $warnings warnings"
    else
        log_error "$config_name: $errors errors, $warnings warnings"
    fi
    
    echo ""
    return $errors
}

# Auto-fix function
fix_config_file() {
    local config_file="$1"
    local config_name="$2"
    
    if [ ! -f "$config_file" ]; then
        log_warning "Configuration file not found: $config_file"
        return 0
    fi
    
    log_info "Auto-fixing $config_name..."
    
    # Create backup
    cp "$config_file" "$config_file.backup.$(date +%Y%m%d_%H%M%S)"
    
    # Fix common field naming issues
    sed -i 's/storagepath\s*=/storage_path =/g' "$config_file"
    sed -i 's/listenport\s*=/listen_address =/g' "$config_file"
    sed -i 's/metricsenabled\s*=/metrics_enabled =/g' "$config_file"
    sed -i 's/metricsport\s*=/metrics_port =/g' "$config_file"
    sed -i 's/pidfilepath\s*=/pid_file =/g' "$config_file"
    
    # Ensure networkevents is enabled if network_resilience section exists
    if grep -q "^\[network_resilience\]" "$config_file" 2>/dev/null; then
        if ! grep -q "networkevents\s*=" "$config_file" 2>/dev/null; then
            # Add networkevents = true to uploads section
            sed -i '/^\[uploads\]/a networkevents = true' "$config_file"
        else
            # Enable existing networkevents
            sed -i 's/networkevents\s*=\s*false/networkevents = true/g' "$config_file"
        fi
    fi
    
    log_success "Auto-fix completed for $config_name"
}

# Generate standardized configuration
generate_standard_config() {
    local config_file="$1"
    local deployment_type="$2"
    
    log_info "Generating standardized configuration for $deployment_type..."
    
    # Create directory if needed
    mkdir -p "$(dirname "$config_file")"
    
    cat > "$config_file" << EOF
# HMAC File Server 3.2 "Tremora del Terra" Configuration
# Generated for: $deployment_type deployment
# Generated on: $(date)

[server]
listen_address = "8080"
storage_path = "/opt/hmac-file-server/data/uploads"
metrics_enabled = true
metrics_port = "9090"
pid_file = "/opt/hmac-file-server/data/hmac-file-server.pid"
max_upload_size = "10GB"
deduplication_enabled = true
min_free_bytes = "1GB"
file_naming = "original"
enable_dynamic_workers = true

[security]
secret = "CHANGE-THIS-SECRET-KEY-MINIMUM-32-CHARACTERS"
enablejwt = false

[uploads]
allowedextensions = [".txt", ".pdf", ".jpg", ".jpeg", ".png", ".gif", ".webp", ".zip", ".tar", ".gz", ".7z", ".mp4", ".webm", ".ogg", ".mp3", ".wav", ".flac", ".doc", ".docx", ".xls", ".xlsx", ".ppt", ".pptx", ".odt", ".ods", ".odp"]
maxfilesize = "100MB"
chunkeduploadsenabled = true
chunksize = "10MB"
networkevents = true

# Network Resilience for Enhanced Mobile Support
[network_resilience]
enabled = true
fast_detection = false                  # Standard detection for server deployment
quality_monitoring = true              # Enable quality monitoring
predictive_switching = false           # Conservative switching for servers
mobile_optimizations = false           # Standard thresholds for server environment
upload_resilience = true               # Resume uploads across network changes
detection_interval = "5s"              # Standard detection interval
quality_check_interval = "10s"         # Regular quality monitoring
network_change_threshold = 3           # Switches required to trigger network change
interface_stability_time = "30s"       # Server-appropriate stability time
upload_pause_timeout = "5m"           # Standard upload pause timeout
upload_retry_timeout = "10m"          # Standard retry timeout
rtt_warning_threshold = "200ms"        # Server network warning threshold
rtt_critical_threshold = "1000ms"      # Server network critical threshold
packet_loss_warning_threshold = 2.0    # 2% packet loss warning
packet_loss_critical_threshold = 10.0  # 10% packet loss critical

[downloads]
chunkeddownloadsenabled = true
chunksize = "10MB"

[logging]
level = "INFO"
file = "/opt/hmac-file-server/data/logs/hmac-file-server.log"
max_size = 100
max_backups = 3
max_age = 30
compress = true

[workers]
numworkers = 10
uploadqueuesize = 1000
autoscaling = true

[timeouts]
readtimeout = "30s"
writetimeout = "30s"
idletimeout = "120s"
shutdown = "30s"

[clamav]
enabled = false

[redis]
enabled = false
EOF

    log_success "Standard configuration generated: $config_file"
}

# Main function
main() {
    echo -e "${BLUE}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║${NC}     HMAC File Server Configuration Consistency Checker    ${BLUE}║${NC}"
    echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    local total_errors=0
    local fix_mode=false
    local generate_mode=false
    
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --fix)
                fix_mode=true
                shift
                ;;
            --generate)
                generate_mode=true
                shift
                ;;
            --help)
                echo "Configuration Consistency Checker"
                echo ""
                echo "Usage: $0 [options]"
                echo ""
                echo "Options:"
                echo "  --fix       Auto-fix common configuration issues"
                echo "  --generate  Generate standardized configurations"
                echo "  --help      Show this help"
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                exit 1
                ;;
        esac
    done
    
    if [ "$generate_mode" = true ]; then
        log_info "Generating standardized configurations for all deployment methods..."
        generate_standard_config "./templates/config-systemd.toml" "SystemD"
        generate_standard_config "./templates/config-docker.toml" "Docker"
        generate_standard_config "./templates/config-podman.toml" "Podman"
        generate_standard_config "./templates/config-debian.toml" "Debian"
        log_success "All standard configurations generated in ./templates/"
        exit 0
    fi
    
    # Check all configuration locations
    for i in "${!CONFIG_LOCATIONS[@]}"; do
        config_file="${CONFIG_LOCATIONS[$i]}"
        
        # Determine config name
        case "$config_file" in
            *"/opt/hmac-file-server/"*) config_name="SystemD" ;;
            *"hmac-docker"*) config_name="Docker" ;;
            *"podman"*) config_name="Podman" ;;
            *"/etc/hmac-file-server/"*) config_name="Debian" ;;
            *"config-default.toml") config_name="Default Template" ;;
            *"config-simple.toml") config_name="Simple Template" ;;
            *"config-simplified-production.toml") config_name="Production Template" ;;
            *) config_name="Unknown" ;;
        esac
        
        if [ "$fix_mode" = true ]; then
            fix_config_file "$config_file" "$config_name"
        fi
        
        if check_config_file "$config_file" "$config_name"; then
            # No errors
            :
        else
            ((total_errors++))
        fi
    done
    
    # Summary
    echo "════════════════════════════════════════════════════════════"
    if [ $total_errors -eq 0 ]; then
        log_success "All configurations are consistent and valid!"
    else
        log_error "Found configuration issues in $total_errors files"
        echo ""
        log_info "Run with --fix to automatically correct common issues"
        log_info "Run with --generate to create standardized configuration templates"
        exit 1
    fi
}

main "$@"
