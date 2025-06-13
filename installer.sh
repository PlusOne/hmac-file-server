#!/bin/bash

# HMAC File Server Installer Script
# Version: 3.2
# Compatible with systemd Linux distributions

set -e

# Trap to handle script errors
trap 'handle_error $? $LINENO' ERR

# Error handling function
handle_error() {
    local exit_code=$1
    local line_number=$2
    echo -e "${RED}Error occurred in script at line $line_number with exit code $exit_code${NC}"
    echo -e "${YELLOW}Installation failed. Please check the error message above.${NC}"
    exit $exit_code
}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default values
DEFAULT_USER="hmac-server"
DEFAULT_INSTALL_DIR="/opt/hmac-file-server"
DEFAULT_CONFIG_DIR="/etc/hmac-file-server"
DEFAULT_DATA_DIR="/var/lib/hmac-file-server"
DEFAULT_LOG_DIR="/var/log/hmac-file-server"
DEFAULT_PORT="8080"
DEFAULT_METRICS_PORT="9090"

# Help function
show_help() {
    echo -e "${BLUE}HMAC File Server 3.2 Installer${NC}"
    echo ""
    echo "Usage: $0 [OPTION]"
    echo ""
    echo "Options:"
    echo "  --help       Show this help message"
    echo "  --uninstall  Uninstall HMAC File Server completely"
    echo ""
    echo "Environment Variables (optional):"
    echo "  HMAC_SECRET     Pre-set HMAC secret (minimum 32 characters)"
    echo "  JWT_SECRET      Pre-set JWT secret (minimum 32 characters)"
    echo ""
    echo "Example:"
    echo "  HMAC_SECRET='your-super-secret-hmac-key-here-32chars' sudo -E $0"
    echo ""
    echo "This installer will:"
    echo "  • Install Go 1.24 (if not present)"
    echo "  • Create system user and directories"
    echo "  • Build and install HMAC File Server"
    echo "  • Configure systemd service"
    echo "  • Install Redis and/or ClamAV (optional)"
    echo ""
    echo "For XMPP operators: This installer is optimized for easy integration"
    echo "with Prosody, Ejabberd, and other XMPP servers."
    echo ""
}

# Check for help flag first (before root check)
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    show_help
    exit 0
fi

# Professional installer header with branding
echo ""
echo -e "${BLUE}    __                               _____ __                                         ${NC}"
echo -e "${BLUE}   / /_  ____ ___  ____ ______      / __(_) /__        ________  ______   _____  _____${NC}"
echo -e "${BLUE}  / __ \\/ __ \`__ \\/ __ \`/ ___/_____/ /_/ / / _ \\______/ ___/ _ \\/ ___/ | / / _ \\/ ___/${NC}"
echo -e "${BLUE} / / / / / / / / / /_/ / /__/_____/ __/ / /  __/_____(__  )  __/ /   | |/ /  __/ /    ${NC}"
echo -e "${BLUE}/_/ /_/_/ /_/ /_/\\__,_/\\___/     /_/ /_/_/\\___/     /____/\\___/_/    |___/\\___/_/     ${NC}"
echo ""
echo -e "${BLUE}                     █ HMAC File Server 3.2 Installer █${NC}"
echo -e "${BLUE}                           Professional XMPP Integration${NC}"
echo ""
echo -e "${YELLOW}────────────────────────────────────────────────────────────────────────────────${NC}"
echo -e "${GREEN} Secure File Uploads & Downloads     JWT & HMAC Authentication${NC}"
echo -e "${GREEN} Prometheus Metrics Integration      ClamAV Virus Scanning${NC}"  
echo -e "${GREEN} Redis Cache & Session Management    Chunked Upload/Download Support${NC}"
echo -e "${YELLOW}────────────────────────────────────────────────────────────────────────────────${NC}"
echo ""

# Check if running as root
if [[ $EUID -ne 0 ]]; then
   echo -e "${RED}This script must be run as root (use sudo)${NC}"
   exit 1
fi

# Check for systemd
if ! command -v systemctl &> /dev/null; then
    echo -e "${RED}Error: systemctl not found. This installer requires a systemd-based Linux distribution.${NC}"
    exit 1
fi

# Pre-installation checks
pre_installation_checks() {
    echo -e "${YELLOW}Running pre-installation checks...${NC}"
    
    # Check if service already exists
    if systemctl is-enabled hmac-file-server.service &>/dev/null; then
        echo -e "${YELLOW}Warning: HMAC File Server service already exists${NC}"
        read -p "Do you want to continue and overwrite the existing installation? (y/N): " OVERWRITE
        if [[ ! $OVERWRITE =~ ^[Yy]$ ]]; then
            echo -e "${YELLOW}Installation cancelled${NC}"
            exit 0
        fi
        
        # Stop existing service
        echo -e "${YELLOW}Stopping existing service...${NC}"
        systemctl stop hmac-file-server.service || true
    fi
    
    # Check available disk space (minimum 1GB)
    AVAILABLE_SPACE=$(df / | awk 'NR==2 {print $4}')
    if [[ $AVAILABLE_SPACE -lt 1048576 ]]; then
        echo -e "${RED}Error: Insufficient disk space. At least 1GB required${NC}"
        exit 1
    fi
    
    # Check if we're in the correct directory (should contain go.mod)
    if [[ ! -f "go.mod" ]]; then
        echo -e "${RED}Error: go.mod not found. Please run this installer from the HMAC File Server source directory${NC}"
        exit 1
    fi
    
    echo -e "${GREEN}Pre-installation checks passed${NC}"
    echo ""
}

# Check for Go installation
check_go() {
    if ! command -v go &> /dev/null; then
        echo -e "${YELLOW}Go is not installed. Installing Go 1.24...${NC}"
        
        # Detect architecture
        ARCH=$(uname -m)
        case $ARCH in
            x86_64) GO_ARCH="amd64" ;;
            aarch64|arm64) GO_ARCH="arm64" ;;
            armv7l) GO_ARCH="armv6l" ;;
            *) echo -e "${RED}Unsupported architecture: $ARCH${NC}"; exit 1 ;;
        esac
        
        # Download and install Go
        cd /tmp
        wget -q "https://go.dev/dl/go1.24.linux-${GO_ARCH}.tar.gz"
        tar -C /usr/local -xzf "go1.24.linux-${GO_ARCH}.tar.gz"
        
        # Add Go to PATH
        echo 'export PATH=$PATH:/usr/local/go/bin' >> /etc/profile
        export PATH=$PATH:/usr/local/go/bin
        
        echo -e "${GREEN}Go 1.24 installed successfully${NC}"
    else
        GO_VERSION=$(go version | awk '{print $3}' | sed 's/go//')
        echo -e "${GREEN}Go $GO_VERSION is already installed${NC}"
    fi
}

# User input function
get_user_input() {
    echo -e "${BLUE}Configuration Setup${NC}"
    echo "Please provide the following information (or press Enter for defaults):"
    echo ""
    
    # System user
    read -p "System user for HMAC File Server [$DEFAULT_USER]: " HMAC_USER
    HMAC_USER=${HMAC_USER:-$DEFAULT_USER}
    
    # Installation directory
    read -p "Installation directory [$DEFAULT_INSTALL_DIR]: " INSTALL_DIR
    INSTALL_DIR=${INSTALL_DIR:-$DEFAULT_INSTALL_DIR}
    
    # Configuration directory
    read -p "Configuration directory [$DEFAULT_CONFIG_DIR]: " CONFIG_DIR
    CONFIG_DIR=${CONFIG_DIR:-$DEFAULT_CONFIG_DIR}
    
    # Data directory
    read -p "Data directory (uploads) [$DEFAULT_DATA_DIR]: " DATA_DIR
    DATA_DIR=${DATA_DIR:-$DEFAULT_DATA_DIR}
    
    # Server port
    read -p "Server port [$DEFAULT_PORT]: " SERVER_PORT
    SERVER_PORT=${SERVER_PORT:-$DEFAULT_PORT}
    
    # Metrics port
    read -p "Metrics port [$DEFAULT_METRICS_PORT]: " METRICS_PORT
    METRICS_PORT=${METRICS_PORT:-$DEFAULT_METRICS_PORT}
    
    # HMAC secret
    if [[ -n "$HMAC_SECRET" ]]; then
        # Use environment variable if provided
        if [[ ${#HMAC_SECRET} -ge 32 ]]; then
            echo -e "${GREEN}Using HMAC secret from environment variable${NC}"
        else
            echo -e "${RED}Error: HMAC_SECRET environment variable must be at least 32 characters long${NC}"
            echo -e "${YELLOW}Current length: ${#HMAC_SECRET}${NC}"
            exit 1
        fi
    else
        # Interactive input with auto-generation option
        echo ""
        echo -e "${BLUE}HMAC Secret Configuration${NC}"
        echo "Choose how to set the HMAC secret:"
        echo "  1) Generate automatically (recommended)"
        echo "  2) Enter manually"
        echo ""
        
        while true; do
            read -p "Choice [1]: " hmac_choice
            hmac_choice=${hmac_choice:-1}
            
            case $hmac_choice in
                1)
                    echo -e "${YELLOW}Generating secure HMAC secret...${NC}"
                    HMAC_SECRET=$(generate_random_key 48)
                    echo -e "${GREEN}Generated 48-character HMAC secret${NC}"
                    echo -e "${BLUE}Secret preview: ${HMAC_SECRET:0:8}...${HMAC_SECRET: -8}${NC}"
                    break
                    ;;
                2)
                    while true; do
                        echo -n "HMAC secret (minimum 32 characters): "
                        # Use bash built-in silent read if available
                        if read -s -r HMAC_SECRET 2>/dev/null; then
                            echo ""
                        else
                            # Fallback: use regular read with warning
                            echo ""
                            echo -e "${YELLOW}Note: Input will be visible (your terminal doesn't support hidden input)${NC}"
                            echo -n "HMAC secret (minimum 32 characters): "
                            read -r HMAC_SECRET
                        fi
                        
                        if [[ ${#HMAC_SECRET} -ge 32 ]]; then
                            echo -e "${GREEN}HMAC secret accepted (${#HMAC_SECRET} characters)${NC}"
                            break 2
                        else
                            echo -e "${RED}HMAC secret must be at least 32 characters long (you entered ${#HMAC_SECRET} characters)${NC}"
                            echo -e "${YELLOW}Tip: Choose option 1 for automatic generation${NC}"
                        fi
                    done
                    ;;
                *)
                    echo -e "${RED}Please enter 1 or 2${NC}"
                    ;;
            esac
        done
    fi
    
    # JWT settings
    echo ""
    read -p "Enable JWT authentication? (y/N): " ENABLE_JWT
    if [[ $ENABLE_JWT =~ ^[Yy]$ ]]; then
        ENABLE_JWT="true"
        
        # JWT secret
        if [[ -n "$JWT_SECRET" ]]; then
            # Use environment variable if provided
            if [[ ${#JWT_SECRET} -ge 32 ]]; then
                echo -e "${GREEN}Using JWT secret from environment variable${NC}"
            else
                echo -e "${RED}Error: JWT_SECRET environment variable must be at least 32 characters long${NC}"
                echo -e "${YELLOW}Current length: ${#JWT_SECRET}${NC}"
                exit 1
            fi
        else
            # Interactive input with auto-generation option
            echo ""
            echo -e "${BLUE}JWT Secret Configuration${NC}"
            echo "Choose how to set the JWT secret:"
            echo "  1) Generate automatically (recommended)"
            echo "  2) Enter manually"
            echo ""
            
            while true; do
                read -p "Choice [1]: " jwt_choice
                jwt_choice=${jwt_choice:-1}
                
                case $jwt_choice in
                    1)
                        echo -e "${YELLOW}Generating secure JWT secret...${NC}"
                        JWT_SECRET=$(generate_random_key 48)
                        echo -e "${GREEN}Generated 48-character JWT secret${NC}"
                        echo -e "${BLUE}Secret preview: ${JWT_SECRET:0:8}...${JWT_SECRET: -8}${NC}"
                        break
                        ;;
                    2)
                        while true; do
                            echo -n "JWT secret (minimum 32 characters): "
                            # Use bash built-in silent read if available
                            if read -s -r JWT_SECRET 2>/dev/null; then
                                echo ""
                            else
                                # Fallback: use regular read with warning
                                echo ""
                                echo -e "${YELLOW}Note: Input will be visible (your terminal doesn't support hidden input)${NC}"
                                echo -n "JWT secret (minimum 32 characters): "
                                read -r JWT_SECRET
                            fi
                            
                            if [[ ${#JWT_SECRET} -ge 32 ]]; then
                                echo -e "${GREEN}JWT secret accepted (${#JWT_SECRET} characters)${NC}"
                                break 2
                            else
                                echo -e "${RED}JWT secret must be at least 32 characters long (you entered ${#JWT_SECRET} characters)${NC}"
                                echo -e "${YELLOW}Tip: Choose option 1 for automatic generation${NC}"
                            fi
                        done
                        ;;
                    *)
                        echo -e "${RED}Please enter 1 or 2${NC}"
                        ;;
                esac
            done
        fi
        
        # JWT expiration
        read -p "JWT token expiration [24h]: " JWT_EXPIRATION
        JWT_EXPIRATION=${JWT_EXPIRATION:-"24h"}
        
        # JWT algorithm
        read -p "JWT algorithm (HS256/HS384/HS512) [HS256]: " JWT_ALGORITHM
        JWT_ALGORITHM=${JWT_ALGORITHM:-"HS256"}
    else
        ENABLE_JWT="false"
        JWT_SECRET=""
        JWT_EXPIRATION="24h"
        JWT_ALGORITHM="HS256"
    fi
    
    # Redis settings
    echo ""
    read -p "Enable Redis integration? (y/N): " ENABLE_REDIS
    if [[ $ENABLE_REDIS =~ ^[Yy]$ ]]; then
        ENABLE_REDIS="true"
        read -p "Redis host [localhost]: " REDIS_HOST
        REDIS_HOST=${REDIS_HOST:-"localhost"}
        read -p "Redis port [6379]: " REDIS_PORT
        REDIS_PORT=${REDIS_PORT:-"6379"}
        read -p "Redis database [0]: " REDIS_DB
        REDIS_DB=${REDIS_DB:-"0"}
        read -s -p "Redis password (optional): " REDIS_PASSWORD
        echo ""
    else
        ENABLE_REDIS="false"
    fi
    
    # ClamAV settings
    echo ""
    read -p "Enable ClamAV virus scanning? (y/N): " ENABLE_CLAMAV
    if [[ $ENABLE_CLAMAV =~ ^[Yy]$ ]]; then
        ENABLE_CLAMAV="true"
        CLAMAV_CONFIG="socket = \"/var/run/clamav/clamd.ctl\""  # Default, will be updated during installation
    else
        ENABLE_CLAMAV="false"
        CLAMAV_CONFIG=""
    fi
    
    # SSL/TLS settings
    echo ""
    read -p "Enable SSL/TLS? (y/N): " ENABLE_TLS
    if [[ $ENABLE_TLS =~ ^[Yy]$ ]]; then
        ENABLE_TLS="true"
        read -p "SSL certificate path: " SSL_CERT
        read -p "SSL private key path: " SSL_KEY
    else
        ENABLE_TLS="false"
    fi
    
    # Show configuration summary
    # Professional configuration summary
    echo ""
    echo -e "${BLUE}Configuration Summary${NC}"
    echo -e "${YELLOW}────────────────────────────────────────────────────────────────${NC}"
    echo -e "${YELLOW}System User:${NC}     $HMAC_USER"
    echo -e "${YELLOW}Install Dir:${NC}     $INSTALL_DIR"
    echo -e "${YELLOW}Config Dir:${NC}      $CONFIG_DIR"
    echo -e "${YELLOW}Data Dir:${NC}        $DATA_DIR"
    echo -e "${YELLOW}Server Port:${NC}     $SERVER_PORT"
    echo -e "${YELLOW}Metrics Port:${NC}    $METRICS_PORT"
    echo -e "${YELLOW}JWT Auth:${NC}        $([[ "$ENABLE_JWT" == "true" ]] && echo "Enabled" || echo "Disabled")"
    echo -e "${YELLOW}Redis:${NC}           $([[ "$ENABLE_REDIS" == "true" ]] && echo "Enabled ($REDIS_HOST:$REDIS_PORT)" || echo "Disabled")"
    echo -e "${YELLOW}ClamAV:${NC}          $([[ "$ENABLE_CLAMAV" == "true" ]] && echo "Enabled" || echo "Disabled")"
    echo -e "${YELLOW}SSL/TLS:${NC}         $([[ "$ENABLE_TLS" == "true" ]] && echo "Enabled" || echo "Disabled")"
    echo -e "${YELLOW}────────────────────────────────────────────────────────────────${NC}"
    echo ""
    read -p "Continue with installation? (y/N): " CONFIRM_INSTALL
    if [[ ! $CONFIRM_INSTALL =~ ^[Yy]$ ]]; then
        echo -e "${YELLOW}Installation cancelled by user${NC}"
        exit 0
    fi
}

# Create system user
create_user() {
    if ! id "$HMAC_USER" &>/dev/null; then
        echo -e "${YELLOW}Creating system user: $HMAC_USER${NC}"
        useradd --system --home-dir "$INSTALL_DIR" --shell /bin/false --comment "HMAC File Server" "$HMAC_USER"
    else
        echo -e "${GREEN}User $HMAC_USER already exists${NC}"
    fi
}

# Create directories
create_directories() {
    echo -e "${YELLOW}Creating directories...${NC}"
    
    mkdir -p "$INSTALL_DIR"
    mkdir -p "$CONFIG_DIR"
    mkdir -p "$DATA_DIR/uploads"
    mkdir -p "$DATA_DIR/deduplication"
    mkdir -p "$DATA_DIR/runtime"
    mkdir -p "$DEFAULT_LOG_DIR"
    
    # Set ownership
    chown -R "$HMAC_USER:$HMAC_USER" "$INSTALL_DIR"
    chown -R "$HMAC_USER:$HMAC_USER" "$DATA_DIR"
    chown -R "$HMAC_USER:$HMAC_USER" "$DEFAULT_LOG_DIR"
    
    # Set permissions
    chmod 755 "$INSTALL_DIR"
    chmod 755 "$DATA_DIR"
    chmod 750 "$DEFAULT_LOG_DIR"
}

# Build HMAC File Server
build_server() {
    echo -e "${YELLOW}Building HMAC File Server...${NC}"
    
    # Build the server
    cd "$(dirname "$0")"
    go build -o "$INSTALL_DIR/hmac-file-server" cmd/server/main.go cmd/server/helpers.go cmd/server/config_validator.go cmd/server/config_test_scenarios.go
    
    # Set ownership and permissions
    chown "$HMAC_USER:$HMAC_USER" "$INSTALL_DIR/hmac-file-server"
    chmod 755 "$INSTALL_DIR/hmac-file-server"
    
    echo -e "${GREEN}HMAC File Server built successfully${NC}"
}

# Generate configuration file
generate_config() {
    echo -e "${YELLOW}Generating configuration file...${NC}"
    
    cat > "$CONFIG_DIR/config.toml" << EOF
# HMAC File Server Configuration
# Generated by installer on $(date)

[server]
bind_ip = "0.0.0.0"
listenport = "$SERVER_PORT"
unixsocket = false
storagepath = "$DATA_DIR/uploads"
metricsenabled = true
metricsport = "$METRICS_PORT"
deduplicationenabled = true
deduplicationpath = "$DATA_DIR/deduplication"
filenaming = "HMAC"
force_protocol = "auto"
pidfilepath = "$DATA_DIR/runtime/hmac-file-server.pid"
EOF

    if [[ $ENABLE_TLS == "true" ]]; then
        cat >> "$CONFIG_DIR/config.toml" << EOF
sslenabled = true
sslcert = "$SSL_CERT"
sslkey = "$SSL_KEY"
EOF
    else
        cat >> "$CONFIG_DIR/config.toml" << EOF
sslenabled = false
EOF
    fi

    cat >> "$CONFIG_DIR/config.toml" << EOF

[security]
secret = "$HMAC_SECRET"
enablejwt = $ENABLE_JWT
EOF

    if [[ $ENABLE_JWT == "true" ]]; then
        cat >> "$CONFIG_DIR/config.toml" << EOF
jwtsecret = "$JWT_SECRET"
jwtalgorithm = "$JWT_ALGORITHM"
jwtexpiration = "$JWT_EXPIRATION"
EOF
    fi

    cat >> "$CONFIG_DIR/config.toml" << EOF

[uploads]
allowedextensions = [".txt", ".pdf", ".jpg", ".jpeg", ".png", ".gif", ".webp", ".zip", ".tar", ".gz", ".7z", ".mp4", ".webm", ".ogg", ".mp3", ".wav", ".flac", ".doc", ".docx", ".xls", ".xlsx", ".ppt", ".pptx", ".odt", ".ods", ".odp"]
maxfilesize = "100MB"
chunkeduploadsenabled = true
chunksize = "10MB"
ttlenabled = false
ttl = "168h"

[downloads]
chunkeddownloadsenabled = true
chunksize = "10MB"

[logging]
level = "INFO"
file = "$DEFAULT_LOG_DIR/hmac-file-server.log"
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
EOF

    if [[ $ENABLE_CLAMAV == "true" ]]; then
        cat >> "$CONFIG_DIR/config.toml" << EOF

[clamav]
enabled = true
${CLAMAV_CONFIG}
timeout = "30s"
EOF
    else
        cat >> "$CONFIG_DIR/config.toml" << EOF

[clamav]
enabled = false
EOF
    fi

    if [[ $ENABLE_REDIS == "true" ]]; then
        cat >> "$CONFIG_DIR/config.toml" << EOF

[redis]
enabled = true
host = "$REDIS_HOST"
port = $REDIS_PORT
database = $REDIS_DB
password = "$REDIS_PASSWORD"
timeout = "5s"
EOF
    else
        cat >> "$CONFIG_DIR/config.toml" << EOF

[redis]
enabled = false
EOF
    fi

    # Set ownership and permissions
    chown "$HMAC_USER:$HMAC_USER" "$CONFIG_DIR/config.toml"
    chmod 640 "$CONFIG_DIR/config.toml"
    
    echo -e "${GREEN}Configuration file created: $CONFIG_DIR/config.toml${NC}"
}

# Create systemd service
create_systemd_service() {
    echo -e "${YELLOW}Creating systemd service...${NC}"
    
    cat > /etc/systemd/system/hmac-file-server.service << EOF
[Unit]
Description=HMAC File Server 3.2
Documentation=https://github.com/PlusOne/hmac-file-server
After=network.target
Wants=network-online.target
EOF

    if [[ $ENABLE_REDIS == "true" ]]; then
        echo "After=redis.service" >> /etc/systemd/system/hmac-file-server.service
    fi

    if [[ $ENABLE_CLAMAV == "true" ]]; then
        echo "After=clamav-daemon.service" >> /etc/systemd/system/hmac-file-server.service
    fi

    cat >> /etc/systemd/system/hmac-file-server.service << EOF

[Service]
Type=simple
User=$HMAC_USER
Group=$HMAC_USER
ExecStart=$INSTALL_DIR/hmac-file-server -config $CONFIG_DIR/config.toml
ExecReload=/bin/kill -SIGHUP \$MAINPID
WorkingDirectory=$INSTALL_DIR
Restart=always
RestartSec=10
StandardOutput=journal
StandardError=journal
SyslogIdentifier=hmac-file-server

# Security settings
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=$DATA_DIR $DEFAULT_LOG_DIR
CapabilityBoundingSet=CAP_NET_BIND_SERVICE
AmbientCapabilities=CAP_NET_BIND_SERVICE

# Resource limits
LimitNOFILE=65536
LimitNPROC=4096

[Install]
WantedBy=multi-user.target
EOF

    # Reload systemd and enable service
    systemctl daemon-reload
    systemctl enable hmac-file-server.service
    
    echo -e "${GREEN}Systemd service created and enabled${NC}"
}

# Install dependencies
install_dependencies() {
    echo -e "${YELLOW}Installing dependencies...${NC}"
    
    # Detect package manager and install dependencies
    if command -v apt-get &> /dev/null; then
        apt-get update
        if [[ $ENABLE_REDIS == "true" ]]; then
            apt-get install -y redis-server
            systemctl enable redis-server
        fi
        if [[ $ENABLE_CLAMAV == "true" ]]; then
            apt-get install -y clamav clamav-daemon
            systemctl enable clamav-daemon
            # Update virus definitions
            freshclam || true
            
            # Detect ClamAV configuration and configure accordingly
            echo -e "${YELLOW}Configuring ClamAV connection...${NC}"
            
            # Check if ClamAV daemon is running and detect socket/port
            if systemctl is-active --quiet clamav-daemon; then
                echo "  ClamAV daemon is running"
                
                # Check for Unix socket (preferred)
                if [[ -S "/var/run/clamav/clamd.ctl" ]]; then
                    echo "  Unix socket detected: /var/run/clamav/clamd.ctl"
                    CLAMAV_CONFIG="socket = \"/var/run/clamav/clamd.ctl\""
                elif [[ -S "/run/clamav/clamd.ctl" ]]; then
                    echo "  Unix socket detected: /run/clamav/clamd.ctl"
                    CLAMAV_CONFIG="socket = \"/run/clamav/clamd.ctl\""
                elif [[ -S "/tmp/clamd" ]]; then
                    echo "  Unix socket detected: /tmp/clamd"
                    CLAMAV_CONFIG="socket = \"/tmp/clamd\""
                # Check for TCP port
                elif netstat -ln | grep -q ":3310"; then
                    echo "  TCP port detected: 127.0.0.1:3310"
                    CLAMAV_CONFIG="address = \"127.0.0.1:3310\""
                else
                    echo "  ClamAV socket/port not detected, using default Unix socket"
                    CLAMAV_CONFIG="socket = \"/var/run/clamav/clamd.ctl\""
                fi
            else
                echo "  ClamAV daemon not running, using default configuration"
                CLAMAV_CONFIG="socket = \"/var/run/clamav/clamd.ctl\""
                
                # Try to start the daemon
                echo "  Attempting to start ClamAV daemon..."
                systemctl start clamav-daemon || echo "  Failed to start ClamAV daemon"
            fi
        fi
    elif command -v yum &> /dev/null; then
        if [[ $ENABLE_REDIS == "true" ]]; then
            yum install -y redis
            systemctl enable redis
        fi
        if [[ $ENABLE_CLAMAV == "true" ]]; then
            yum install -y clamav clamav-update clamd
            systemctl enable clamd
            freshclam || true
        fi
    elif command -v dnf &> /dev/null; then
        if [[ $ENABLE_REDIS == "true" ]]; then
            dnf install -y redis
            systemctl enable redis
        fi
        if [[ $ENABLE_CLAMAV == "true" ]]; then
            dnf install -y clamav clamav-update clamd
            systemctl enable clamd
            freshclam || true
        fi
    else
        echo -e "${YELLOW}Unknown package manager. Please install Redis and/or ClamAV manually if needed.${NC}"
    fi
}

# Generate secure random key
generate_random_key() {
    local length=${1:-48}  # Default 48 characters for extra security
    local key=""
    
    # Try different methods in order of preference
    if command -v openssl &> /dev/null; then
        # Method 1: OpenSSL (most common and secure)
        key=$(openssl rand -base64 $((length * 3 / 4 + 1)) | tr -d "=+/\n" | cut -c1-$length)
    elif command -v head &> /dev/null && [[ -r /dev/urandom ]]; then
        # Method 2: /dev/urandom with head (Linux/Unix)
        key=$(head -c $((length * 3 / 4 + 1)) /dev/urandom | base64 | tr -d "=+/\n" | cut -c1-$length)
    elif command -v dd &> /dev/null && [[ -r /dev/urandom ]]; then
        # Method 3: dd with /dev/urandom
        key=$(dd if=/dev/urandom bs=$((length * 3 / 4 + 1)) count=1 2>/dev/null | base64 | tr -d "=+/\n" | cut -c1-$length)
    elif command -v date &> /dev/null; then
        # Method 4: Fallback using date and process info (less secure but works)
        local timestamp=$(date +%s%N)
        local random_data="${timestamp}${RANDOM}${$}$(hostname)"
        key=$(echo -n "$random_data" | sha256sum | cut -c1-$length)
    else
        # Method 5: Last resort - basic fallback
        echo -e "${YELLOW}Warning: Using basic key generation (consider installing openssl)${NC}" >&2
        key="hmac-file-server-$(date +%s)-$(hostname | cut -c1-16)"
        key=$(echo -n "$key" | sha256sum | cut -c1-$length)
    fi
    
    # Ensure exact length
    key=$(echo -n "$key" | cut -c1-$length)
    
    # If still too short, pad with additional random data
    while [[ ${#key} -lt $length ]]; do
        local padding=$(date +%s | sha256sum | cut -c1-$((length - ${#key})))
        key="${key}${padding}"
        key=$(echo -n "$key" | cut -c1-$length)
    done
    
    echo "$key"
}

# Main installation function
main() {
    echo -e "${BLUE}Starting HMAC File Server installation...${NC}"
    echo ""
    
    # Run pre-installation checks
    pre_installation_checks
    
    # Get user input
    get_user_input
    
    echo ""
    echo -e "${BLUE}Installation Summary:${NC}"
    echo "User: $HMAC_USER"
    echo "Install Directory: $INSTALL_DIR"
    echo "Config Directory: $CONFIG_DIR"
    echo "Data Directory: $DATA_DIR"
    echo "Server Port: $SERVER_PORT"
    echo "Metrics Port: $METRICS_PORT"
    echo "JWT Enabled: $ENABLE_JWT"
    echo "Redis Enabled: $ENABLE_REDIS"
    echo "ClamAV Enabled: $ENABLE_CLAMAV"
    echo "TLS Enabled: $ENABLE_TLS"
    echo ""
    
    read -p "Continue with installation? (y/N): " CONFIRM
    if [[ ! $CONFIRM =~ ^[Yy]$ ]]; then
        echo -e "${YELLOW}Installation cancelled.${NC}"
        exit 0
    fi
    
    echo ""
    echo -e "${BLUE}Installing...${NC}"
    
    # Installation steps
    check_go
    create_user
    create_directories
    install_dependencies
    build_server
    generate_config
    create_systemd_service
    
    # Ask if user wants to start the service now
    echo ""
    read -p "Start HMAC File Server service now? (Y/n): " START_SERVICE
    START_SERVICE=${START_SERVICE:-Y}
    
    if [[ $START_SERVICE =~ ^[Yy]$ ]]; then
        echo -e "${YELLOW}Starting HMAC File Server service...${NC}"
        systemctl start hmac-file-server.service
        
        # Wait a moment and check status
        sleep 3
        if systemctl is-active --quiet hmac-file-server.service; then
            echo -e "${GREEN}Service started successfully${NC}"
        else
            echo -e "${RED}Service failed to start. Check logs with: journalctl -u hmac-file-server.service${NC}"
        fi
    fi
    
    print_completion_info
}

# Function to print completion information
print_completion_info() {
    echo ""
    echo -e "${GREEN}                     Installation Complete!${NC}"
    echo -e "${GREEN}────────────────────────────────────────────────────────────────${NC}"
    echo -e "${GREEN}         HMAC File Server 3.2 Successfully Deployed!        ${NC}"
    echo -e "${GREEN}────────────────────────────────────────────────────────────────${NC}"
    echo ""
    echo -e "${BLUE}Service Information:${NC}"
    echo -e "  Status: ${YELLOW}sudo systemctl status hmac-file-server${NC}"
    echo -e "  Logs:   ${YELLOW}sudo journalctl -u hmac-file-server -f${NC}"
    echo -e "  Config: ${YELLOW}sudo nano $CONFIG_DIR/config.toml${NC}"
    echo -e "  Reload: ${YELLOW}sudo systemctl reload hmac-file-server${NC}"
    echo ""
    echo -e "${BLUE}Service Endpoints:${NC}"
    if [[ $ENABLE_TLS == "true" ]]; then
        echo -e "  Server:  ${YELLOW}https://$(hostname -I | awk '{print $1}'):$SERVER_PORT${NC}"
    else
        echo -e "  Server:  ${YELLOW}http://$(hostname -I | awk '{print $1}'):$SERVER_PORT${NC}"
    fi
    echo -e "  Metrics: ${YELLOW}http://$(hostname -I | awk '{print $1}'):$METRICS_PORT/metrics${NC}"
    echo ""
    echo -e "${BLUE}File Locations:${NC}"
    echo -e "  Binary:     ${YELLOW}$INSTALL_DIR/hmac-file-server${NC}"
    echo -e "  Config:     ${YELLOW}$CONFIG_DIR/config.toml${NC}"
    echo -e "  Uploads:    ${YELLOW}$DATA_DIR/uploads${NC}"
    echo -e "  Logs:       ${YELLOW}$DEFAULT_LOG_DIR/hmac-file-server.log${NC}"
    echo ""
    echo -e "${BLUE}Quick Commands:${NC}"
    echo -e "  Start:   ${YELLOW}sudo systemctl start hmac-file-server${NC}"
    echo -e "  Stop:    ${YELLOW}sudo systemctl stop hmac-file-server${NC}"
    echo -e "  Restart: ${YELLOW}sudo systemctl restart hmac-file-server${NC}"
    echo -e "  Status:  ${YELLOW}sudo systemctl status hmac-file-server${NC}"
    echo ""
    echo -e "${BLUE}Next Steps for XMPP Integration:${NC}"
    echo -e "1. ${YELLOW}Configure firewall${NC} to allow ports $SERVER_PORT (server) and $METRICS_PORT (metrics)"
    echo -e "2. Configure your reverse proxy (nginx/apache) with SSL"
    echo -e "3. Update your Prosody/Ejabberd configuration:"
    echo -e "   ${YELLOW}http_file_share = \"http://localhost:$SERVER_PORT\"${NC}"
    echo -e "4. Set up monitoring and log rotation"
    echo -e "5. Test file uploads with your XMPP client"
    echo ""
    echo -e "${BLUE}Documentation & Support:${NC}"
    echo -e "  README: https://github.com/PlusOne/hmac-file-server/blob/main/README.MD"
    echo -e "  Wiki:   https://github.com/PlusOne/hmac-file-server/blob/main/WIKI.MD"
    echo -e "  Issues: https://github.com/PlusOne/hmac-file-server/issues"
    echo ""
    echo -e "${GREEN}────────────────────────────────────────────────────────────────${NC}"
    echo -e "${GREEN}    Thank you for choosing HMAC File Server for your XMPP setup! ${NC}"
    echo -e "${GREEN}────────────────────────────────────────────────────────────────${NC}"
}

# Helper function to safely preserve a directory
preserve_directory() {
    local source_dir="$1"
    local backup_path="$2"
    
    if [[ -d "$source_dir" ]]; then
        local parent_dir=$(dirname "$backup_path")
        mkdir -p "$parent_dir"
        
        if mv "$source_dir" "$backup_path" 2>/dev/null; then
            echo "  ✓ Preserved: $source_dir → $backup_path"
        else
            # Fallback to copy if move fails
            if cp -r "$source_dir" "$backup_path" 2>/dev/null; then
                echo "  ✓ Copied: $source_dir → $backup_path"
                rm -rf "$source_dir"
                echo "  ✓ Removed original: $source_dir"
            else
                echo "  ⚠ Failed to preserve: $source_dir"
            fi
        fi
    else
        echo "  ⚠ Directory not found: $source_dir"
    fi
}

# Custom data selection for option 4
custom_data_selection() {
    echo ""
    echo -e "${BLUE}Custom Data Selection:${NC}"
    echo "Choose which data directories to preserve:"
    echo ""
    
    CUSTOM_PRESERVE_UPLOADS=""
    CUSTOM_PRESERVE_DEDUP=""
    CUSTOM_PRESERVE_LOGS=""
    
    # Ask about uploads
    if [[ -d "$UPLOAD_DIR" ]]; then
        FILE_COUNT=$(find "$UPLOAD_DIR" -type f 2>/dev/null | wc -l)
        DIR_SIZE=$(du -sh "$UPLOAD_DIR" 2>/dev/null | cut -f1)
        echo -e "${GREEN}Upload Directory: ${UPLOAD_DIR}${NC} (Files: $FILE_COUNT, Size: $DIR_SIZE)"
        read -p "Preserve upload directory? (y/N): " PRESERVE_UPLOADS
        if [[ $PRESERVE_UPLOADS =~ ^[Yy]$ ]]; then
            CUSTOM_PRESERVE_UPLOADS="yes"
            echo "  Will preserve uploads"
        else
            echo "  Will delete uploads"
        fi
    else
        echo -e "${YELLOW}Upload Directory: Not found${NC}"
    fi
    
    echo ""
    
    # Ask about deduplication
    if [[ -d "$DEDUP_DIR" ]]; then
        FILE_COUNT=$(find "$DEDUP_DIR" -type f 2>/dev/null | wc -l)
        DIR_SIZE=$(du -sh "$DEDUP_DIR" 2>/dev/null | cut -f1)
        echo -e "${GREEN}Deduplication Directory: ${DEDUP_DIR}${NC} (Files: $FILE_COUNT, Size: $DIR_SIZE)"
        read -p "Preserve deduplication directory? (y/N): " PRESERVE_DEDUP
        if [[ $PRESERVE_DEDUP =~ ^[Yy]$ ]]; then
            CUSTOM_PRESERVE_DEDUP="yes"
            echo "  Will preserve deduplication data"
        else
            echo "  Will delete deduplication data"
        fi
    else
        echo -e "${YELLOW}Deduplication Directory: Not found${NC}"
    fi
    
    echo ""
    
    # Ask about logs
    if [[ -d "$LOG_DIR" ]]; then
        FILE_COUNT=$(find "$LOG_DIR" -type f 2>/dev/null | wc -l)
        DIR_SIZE=$(du -sh "$LOG_DIR" 2>/dev/null | cut -f1)
        echo -e "${GREEN}Log Directory: ${LOG_DIR}${NC} (Files: $FILE_COUNT, Size: $DIR_SIZE)"
        read -p "Preserve log directory? (y/N): " PRESERVE_LOGS
        if [[ $PRESERVE_LOGS =~ ^[Yy]$ ]]; then
            CUSTOM_PRESERVE_LOGS="yes"
            echo "  Will preserve logs"
        else
            echo "  Will delete logs"
        fi
    else
        echo -e "${YELLOW}Log Directory: Not found${NC}"
    fi
    
    # Store custom selection for later processing
    PRESERVE_DATA="custom"
    
    echo ""
    echo -e "${BLUE}Custom selection complete:${NC}"
    [[ "$CUSTOM_PRESERVE_UPLOADS" == "yes" ]] && echo "  Uploads: Preserve" || echo "  Uploads: Delete"
    [[ "$CUSTOM_PRESERVE_DEDUP" == "yes" ]] && echo "  Deduplication: Preserve" || echo "  Deduplication: Delete"
    [[ "$CUSTOM_PRESERVE_LOGS" == "yes" ]] && echo "  Logs: Preserve" || echo "  Logs: Delete"
    echo ""
}

# Handle custom preservation choices
handle_custom_preservation() {
    # Check if any data needs to be preserved
    if [[ "$CUSTOM_PRESERVE_UPLOADS" == "yes" || "$CUSTOM_PRESERVE_DEDUP" == "yes" || "$CUSTOM_PRESERVE_LOGS" == "yes" ]]; then
        BACKUP_DIR="/var/backups/hmac-file-server-$(date +%Y%m%d-%H%M%S)"
        mkdir -p "$BACKUP_DIR"
        echo "  ✓ Created backup directory: $BACKUP_DIR"
    fi
    
    # Handle uploads
    if [[ "$CUSTOM_PRESERVE_UPLOADS" == "yes" ]]; then
        preserve_directory "$UPLOAD_DIR" "$BACKUP_DIR/uploads"
    elif [[ -d "$UPLOAD_DIR" ]]; then
        rm -rf "$UPLOAD_DIR"
        echo "  ✓ Removed uploads: $UPLOAD_DIR"
    fi
    
    # Handle deduplication
    if [[ "$CUSTOM_PRESERVE_DEDUP" == "yes" ]]; then
        preserve_directory "$DEDUP_DIR" "$BACKUP_DIR/deduplication"
    elif [[ -d "$DEDUP_DIR" ]]; then
        rm -rf "$DEDUP_DIR"
        echo "  ✓ Removed deduplication: $DEDUP_DIR"
    fi
    
    # Handle logs
    if [[ "$CUSTOM_PRESERVE_LOGS" == "yes" ]]; then
        preserve_directory "$LOG_DIR" "$BACKUP_DIR/logs"
    elif [[ -d "$LOG_DIR" ]]; then
        rm -rf "$LOG_DIR"
        echo "  ✓ Removed logs: $LOG_DIR"
    fi
    
    # Remove the main data directory if it's separate and empty
    if [[ -d "$DEFAULT_DATA_DIR" ]]; then
        # Only remove if it's different from preserved directories and if it's empty or only contains subdirs we've handled
        if [[ "$DEFAULT_DATA_DIR" != "$UPLOAD_DIR" && "$DEFAULT_DATA_DIR" != "$DEDUP_DIR" && "$DEFAULT_DATA_DIR" != "$LOG_DIR" ]]; then
            # Check if directory is effectively empty (only contains directories we've already handled)
            remaining_files=$(find "$DEFAULT_DATA_DIR" -type f 2>/dev/null | wc -l)
            if [[ $remaining_files -eq 0 ]]; then
                rm -rf "$DEFAULT_DATA_DIR"
                echo "  ✓ Removed empty data directory: $DEFAULT_DATA_DIR"
            else
                echo "  ⚠ Data directory contains additional files: $DEFAULT_DATA_DIR"
            fi
        fi
    fi
}

# Uninstaller function (can be called with ./installer.sh --uninstall)
uninstall() {
    echo ""
    echo -e "${RED}                     █ HMAC File Server Uninstaller █${NC}"
    echo -e "${RED}────────────────────────────────────────────────────────────────${NC}"
    echo -e "${RED}              Warning: This will remove the server installation!   ${NC}"
    echo -e "${RED}────────────────────────────────────────────────────────────────${NC}"
    echo ""
    
    read -p "Are you sure you want to uninstall HMAC File Server? (y/N): " CONFIRM_UNINSTALL
    if [[ ! $CONFIRM_UNINSTALL =~ ^[Yy]$ ]]; then
        echo -e "${YELLOW}Uninstall cancelled${NC}"
        exit 0
    fi
    
    echo ""
    echo -e "${BLUE}Data Preservation Options:${NC}"
    echo -e "${BLUE}────────────────────────────────────────────────────────────────${NC}"
    echo ""
    echo "The following data directories may contain important files:"
    
    # Check what data directories exist and show their contents
    PRESERVE_DATA=""
    UPLOAD_DIR=""
    DEDUP_DIR=""
    LOG_DIR=""
    
    # Find upload directory from config if it exists
    if [[ -f "$DEFAULT_CONFIG_DIR/config.toml" ]]; then
        UPLOAD_DIR=$(grep -E "^storagepath\s*=" "$DEFAULT_CONFIG_DIR/config.toml" 2>/dev/null | sed 's/.*=\s*"*\([^"]*\)"*.*/\1/' | xargs)
        DEDUP_DIR=$(grep -E "^directory\s*=" "$DEFAULT_CONFIG_DIR/config.toml" 2>/dev/null | sed 's/.*=\s*"*\([^"]*\)"*.*/\1/' | xargs)
    fi
    
    # Fallback to default locations
    [[ -z "$UPLOAD_DIR" ]] && UPLOAD_DIR="$DEFAULT_DATA_DIR/uploads"
    [[ -z "$DEDUP_DIR" ]] && DEDUP_DIR="$DEFAULT_DATA_DIR/deduplication"
    LOG_DIR="$DEFAULT_LOG_DIR"
    
    # Show upload directory status
    if [[ -d "$UPLOAD_DIR" ]]; then
        FILE_COUNT=$(find "$UPLOAD_DIR" -type f 2>/dev/null | wc -l)
        DIR_SIZE=$(du -sh "$UPLOAD_DIR" 2>/dev/null | cut -f1)
        echo -e "${GREEN}  Upload Directory: ${UPLOAD_DIR}${NC}"
        echo -e "     Files: $FILE_COUNT, Size: $DIR_SIZE"
    else
        echo -e "${YELLOW}  Upload Directory: Not found or empty${NC}"
    fi
    
    # Show deduplication directory status
    if [[ -d "$DEDUP_DIR" ]]; then
        FILE_COUNT=$(find "$DEDUP_DIR" -type f 2>/dev/null | wc -l)
        DIR_SIZE=$(du -sh "$DEDUP_DIR" 2>/dev/null | cut -f1)
        echo -e "${GREEN}  Deduplication Directory: ${DEDUP_DIR}${NC}"
        echo -e "     Files: $FILE_COUNT, Size: $DIR_SIZE"
    else
        echo -e "${YELLOW}  Deduplication Directory: Not found or empty${NC}"
    fi
    
    # Show log directory status
    if [[ -d "$LOG_DIR" ]]; then
        FILE_COUNT=$(find "$LOG_DIR" -type f 2>/dev/null | wc -l)
        DIR_SIZE=$(du -sh "$LOG_DIR" 2>/dev/null | cut -f1)
        echo -e "${GREEN}  Log Directory: ${LOG_DIR}${NC}"
        echo -e "     Files: $FILE_COUNT, Size: $DIR_SIZE"
    else
        echo -e "${YELLOW}  Log Directory: Not found or empty${NC}"
    fi
    
    echo ""
    echo -e "${BLUE}Choose data handling option:${NC}"
    echo "  1) Delete all data (complete removal)"
    echo "  2) Preserve upload and deduplication data only"
    echo "  3) Preserve all data (uploads, deduplication, and logs)"
    echo "  4) Custom selection (choose what to preserve)"
    echo "  5) Cancel uninstallation"
    echo ""
    
    while true; do
        read -p "Select option (1-5): " DATA_OPTION
        case $DATA_OPTION in
            1)
                echo -e "${RED}Selected: Delete all data${NC}"
                PRESERVE_DATA="none"
                break
                ;;
            2)
                echo -e "${GREEN}Selected: Preserve uploads and deduplication data${NC}"
                PRESERVE_DATA="uploads_dedup"
                break
                ;;
            3)
                echo -e "${GREEN}Selected: Preserve all data${NC}"
                PRESERVE_DATA="all"
                break
                ;;
            4)
                echo -e "${BLUE}Custom selection:${NC}"
                custom_data_selection
                break
                ;;
            5)
                echo -e "${YELLOW}Uninstall cancelled${NC}"
                exit 0
                ;;
            *)
                echo -e "${RED}Invalid option. Please choose 1-5.${NC}"
                ;;
        esac
    done
    
    # Final confirmation for complete deletion
    if [[ "$PRESERVE_DATA" == "none" ]]; then
        echo ""
        echo -e "${RED}FINAL WARNING: This will permanently delete ALL data!${NC}"
        echo -e "${RED}   This includes all uploaded files, deduplication data, and logs.${NC}"
        echo -e "${RED}   This action cannot be undone!${NC}"
        echo ""
        read -p "Type 'DELETE' to confirm complete data removal: " FINAL_CONFIRM
        if [[ "$FINAL_CONFIRM" != "DELETE" ]]; then
            echo -e "${YELLOW}Uninstall cancelled - confirmation failed${NC}"
            exit 0
        fi
    fi
    
    echo ""
    echo -e "${YELLOW}Starting uninstallation process...${NC}"
    echo ""
    
    echo -e "${YELLOW}Stopping and disabling service...${NC}"
    if systemctl is-active --quiet hmac-file-server.service; then
        systemctl stop hmac-file-server.service || true
        echo "  ✓ Service stopped"
    else
        echo "  ⚠ Service was not running"
    fi
    
    if systemctl is-enabled --quiet hmac-file-server.service 2>/dev/null; then
        systemctl disable hmac-file-server.service || true
        echo "  ✓ Service disabled"
    else
        echo "  ⚠ Service was not enabled"
    fi
    
    if [[ -f /etc/systemd/system/hmac-file-server.service ]]; then
        rm -f /etc/systemd/system/hmac-file-server.service
        echo "  ✓ Service file removed"
    else
        echo "  ⚠ Service file not found"
    fi
    
    systemctl daemon-reload
    echo "  ✓ Systemd reloaded"
    
    echo -e "${YELLOW}Removing installation and configuration...${NC}"
    
    # Always remove installation directory
    if [[ -d "$DEFAULT_INSTALL_DIR" ]]; then
        rm -rf "$DEFAULT_INSTALL_DIR"
        echo "  ✓ Removed installation directory: $DEFAULT_INSTALL_DIR"
    else
        echo "  ⚠ Installation directory not found: $DEFAULT_INSTALL_DIR"
    fi
    
    # Always remove configuration directory
    if [[ -d "$DEFAULT_CONFIG_DIR" ]]; then
        rm -rf "$DEFAULT_CONFIG_DIR"
        echo "  ✓ Removed configuration directory: $DEFAULT_CONFIG_DIR"
    else
        echo "  ⚠ Configuration directory not found: $DEFAULT_CONFIG_DIR"
    fi
    
    # Handle data directories based on user choice
    echo -e "${YELLOW}Processing data directories...${NC}"
    
    case $PRESERVE_DATA in
        "none")
            # Delete everything
            for dir in "$UPLOAD_DIR" "$DEDUP_DIR" "$LOG_DIR" "$DEFAULT_DATA_DIR"; do
                if [[ -d "$dir" ]]; then
                    rm -rf "$dir"
                    echo "  ✓ Removed: $dir"
                fi
            done
            ;;
        "uploads_dedup")
            # Preserve uploads and deduplication, remove logs
            if [[ -d "$LOG_DIR" ]]; then
                rm -rf "$LOG_DIR"
                echo "  ✓ Removed logs: $LOG_DIR"
            fi
            # Move preserved data to a safe location
            BACKUP_DIR="/var/backups/hmac-file-server-$(date +%Y%m%d-%H%M%S)"
            mkdir -p "$BACKUP_DIR"
            preserve_directory "$UPLOAD_DIR" "$BACKUP_DIR/uploads"
            preserve_directory "$DEDUP_DIR" "$BACKUP_DIR/deduplication"
            # Remove original data directory structure but keep preserved data
            if [[ -d "$DEFAULT_DATA_DIR" && "$DEFAULT_DATA_DIR" != "$UPLOAD_DIR" && "$DEFAULT_DATA_DIR" != "$DEDUP_DIR" ]]; then
                rm -rf "$DEFAULT_DATA_DIR"
                echo "  ✓ Removed data directory (preserved content moved to $BACKUP_DIR)"
            fi
            ;;
        "all")
            # Preserve everything
            BACKUP_DIR="/var/backups/hmac-file-server-$(date +%Y%m%d-%H%M%S)"
            mkdir -p "$BACKUP_DIR"
            preserve_directory "$UPLOAD_DIR" "$BACKUP_DIR/uploads"
            preserve_directory "$DEDUP_DIR" "$BACKUP_DIR/deduplication"
            preserve_directory "$LOG_DIR" "$BACKUP_DIR/logs"
            # Remove original data directory structure but keep preserved data
            if [[ -d "$DEFAULT_DATA_DIR" ]]; then
                rm -rf "$DEFAULT_DATA_DIR"
                echo "  ✓ Removed data directory (all content preserved in $BACKUP_DIR)"
            fi
            ;;
        "custom")
            # Handle custom selection
            handle_custom_preservation
            ;;
    esac
    
    echo -e "${YELLOW}Removing system user...${NC}"
    if id "$DEFAULT_USER" &>/dev/null; then
        userdel "$DEFAULT_USER" || true
        echo "  ✓ User $DEFAULT_USER removed"
    else
        echo "  ⚠ User $DEFAULT_USER not found"
    fi
    
    # Remove any remaining binary in common locations
    echo -e "${YELLOW}Cleaning up any remaining files...${NC}"
    for location in "/usr/local/bin/hmac-file-server" "/usr/bin/hmac-file-server"; do
        if [[ -f "$location" ]]; then
            rm -f "$location"
            echo "  ✓ Removed $location"
        fi
    done
    
    echo ""
    if [[ "$PRESERVE_DATA" != "none" ]]; then
        echo -e "${GREEN}HMAC File Server uninstalled successfully with data preservation${NC}"
        if [[ -d "$BACKUP_DIR" ]]; then
            echo -e "${BLUE}Preserved data location: $BACKUP_DIR${NC}"
            echo -e "${BLUE}   You can safely delete this directory if you no longer need the data.${NC}"
        fi
    else
        echo -e "${GREEN}HMAC File Server uninstalled completely${NC}"
        echo -e "${BLUE}All files, services, and user accounts have been removed.${NC}"
    fi
    echo ""
}

# Check for help flag
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    show_help
    exit 0
fi

# Check for uninstall flag
if [[ "$1" == "--uninstall" ]]; then
    uninstall
    exit 0
fi

# Run main function
main "$@"
