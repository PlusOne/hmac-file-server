#!/bin/bash

# HMAC File Server - Ejabberd Module Installation Script
# This script installs and configures mod_http_upload_hmac for seamless XMPP integration

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EJABBERD_MODULES_DIR="/opt/ejabberd/lib/ejabberd-*/ebin"
EJABBERD_CONFIG="/opt/ejabberd/conf/ejabberd.yml"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_header() {
    echo -e "${BLUE}"
    echo "╔══════════════════════════════════════════════════════════════════╗"
    echo "║                HMAC File Server - Ejabberd Integration          ║"
    echo "║                      Module Installation Script                 ║"
    echo "╚══════════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"
}

print_step() {
    echo -e "${GREEN}➤ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ WARNING: $1${NC}"
}

print_error() {
    echo -e "${RED}✗ ERROR: $1${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

check_requirements() {
    print_step "Checking requirements..."
    
    # Check if ejabberd is installed
    if ! command -v ejabberdctl &> /dev/null; then
        print_error "ejabberd is not installed or not in PATH"
        exit 1
    fi
    
    # Check if Erlang compiler is available
    if ! command -v erlc &> /dev/null; then
        print_error "Erlang compiler (erlc) is not installed"
        echo "Please install: sudo apt-get install erlang-dev (Ubuntu/Debian) or equivalent"
        exit 1
    fi
    
    # Check ejabberd version
    EJABBERD_VERSION=$(ejabberdctl status | grep "ejabberd" | head -1 | awk '{print $2}' || echo "unknown")
    print_success "ejabberd version: $EJABBERD_VERSION"
    
    # Find ejabberd modules directory
    EJABBERD_MODULES_DIR=$(find /opt/ejabberd /usr/lib/ejabberd /usr/local/lib/ejabberd -name "ebin" -type d 2>/dev/null | head -1)
    if [ -z "$EJABBERD_MODULES_DIR" ]; then
        print_error "Could not find ejabberd modules directory"
        exit 1
    fi
    print_success "ejabberd modules directory: $EJABBERD_MODULES_DIR"
}

compile_module() {
    print_step "Compiling mod_http_upload_hmac..."
    
    cd "$SCRIPT_DIR"
    
    # Create include directory for ejabberd headers
    EJABBERD_INCLUDE_DIR="/tmp/ejabberd_includes"
    mkdir -p "$EJABBERD_INCLUDE_DIR"
    
    # Find ejabberd include files
    EJABBERD_SRC_DIR=$(find /usr/src /opt -name "ejabberd*" -type d 2>/dev/null | grep -E "(src|include)" | head -1)
    
    if [ -n "$EJABBERD_SRC_DIR" ]; then
        cp -r "$EJABBERD_SRC_DIR"/*.hrl "$EJABBERD_INCLUDE_DIR/" 2>/dev/null || true
    fi
    
    # Compile the module
    erlc -I "$EJABBERD_INCLUDE_DIR" -I /opt/ejabberd/lib/ejabberd-*/include \
         -o . mod_http_upload_hmac.erl
    
    if [ ! -f "mod_http_upload_hmac.beam" ]; then
        print_error "Module compilation failed"
        exit 1
    fi
    
    print_success "Module compiled successfully"
}

install_module() {
    print_step "Installing module to ejabberd..."
    
    # Copy compiled module to ejabberd
    sudo cp mod_http_upload_hmac.beam "$EJABBERD_MODULES_DIR/"
    sudo chown ejabberd:ejabberd "$EJABBERD_MODULES_DIR/mod_http_upload_hmac.beam"
    sudo chmod 644 "$EJABBERD_MODULES_DIR/mod_http_upload_hmac.beam"
    
    print_success "Module installed to $EJABBERD_MODULES_DIR"
}

backup_config() {
    if [ -f "$EJABBERD_CONFIG" ]; then
        BACKUP_FILE="${EJABBERD_CONFIG}.backup.$(date +%Y%m%d_%H%M%S)"
        sudo cp "$EJABBERD_CONFIG" "$BACKUP_FILE"
        print_success "ejabberd.yml backed up to $BACKUP_FILE"
    fi
}

configure_ejabberd() {
    print_step "Configuring ejabberd..."
    
    backup_config
    
    # Generate secure random secret
    HMAC_SECRET=$(openssl rand -hex 32)
    
    # Create module configuration
    cat << EOF > /tmp/mod_http_upload_hmac_config.yml

# HMAC File Server Integration Module
modules:
  mod_http_upload_hmac:
    hmac_server_url: "http://localhost:8080"
    hmac_shared_secret: "$HMAC_SECRET"
    max_size: 104857600  # 100MB
    quota_per_user: 1073741824  # 1GB  
    token_expiry: 3600  # 1 hour
    allowed_extensions: 
      - ".jpg"
      - ".jpeg" 
      - ".png"
      - ".gif"
      - ".webp"
      - ".pdf"
      - ".mp4"
      - ".webm"
      - ".mp3"
      - ".flac"
      - ".ogg"
      - ".txt"
      - ".md"
      - ".doc"
      - ".docx"
      - ".zip"
      - ".tar.gz"
    iqdisc: one_queue

# Optional: Disable default mod_http_upload if present
# mod_http_upload: []

EOF

    print_warning "Manual configuration required!"
    echo -e "${YELLOW}Please add the following to your ejabberd.yml modules section:${NC}"
    echo
    cat /tmp/mod_http_upload_hmac_config.yml
    echo
    echo -e "${YELLOW}Save this HMAC secret for your HMAC File Server configuration:${NC}"
    echo -e "${GREEN}$HMAC_SECRET${NC}"
    echo
}

update_hmac_server() {
    print_step "Updating HMAC File Server configuration..."
    
    # Look for existing config files
    HMAC_CONFIG_FILES=(
        "/etc/hmac-file-server/config.toml"
        "./config.toml"
        "./test-config.toml"
    )
    
    for config_file in "${HMAC_CONFIG_FILES[@]}"; do
        if [ -f "$config_file" ]; then
            print_success "Found HMAC config: $config_file"
            
            # Add ejabberd integration section if not present
            if ! grep -q "ejabberd_integration" "$config_file"; then
                echo "" >> "$config_file"
                echo "# Ejabberd Integration" >> "$config_file"
                echo "[ejabberd_integration]" >> "$config_file"
                echo "enabled = true" >> "$config_file"
                echo "bearer_token_auth = true" >> "$config_file"
                echo "# Use the same secret as in ejabberd.yml" >> "$config_file"
                echo "# shared_secret = \"$HMAC_SECRET\"" >> "$config_file"
                
                print_success "Added ejabberd integration section to $config_file"
            fi
        fi
    done
}

test_installation() {
    print_step "Testing installation..."
    
    # Test module loading
    if sudo ejabberdctl module_check mod_http_upload_hmac; then
        print_success "Module can be loaded successfully"
    else
        print_warning "Module check failed - manual verification required"
    fi
}

show_next_steps() {
    echo
    echo -e "${BLUE}╔══════════════════════════════════════════════════════════════════╗"
    echo -e "║                         NEXT STEPS                              ║"
    echo -e "╚══════════════════════════════════════════════════════════════════╝${NC}"
    echo
    echo -e "${GREEN}1. Update ejabberd.yml:${NC}"
    echo "   - Add the module configuration shown above"
    echo "   - Set the hmac_shared_secret to the generated value"
    echo "   - Comment out or remove existing mod_http_upload"
    echo
    echo -e "${GREEN}2. Update HMAC File Server config:${NC}"
    echo "   - Set the same shared_secret in your config.toml"
    echo "   - Enable bearer_token_auth = true"
    echo
    echo -e "${GREEN}3. Restart services:${NC}"
    echo "   sudo systemctl restart ejabberd"
    echo "   sudo systemctl restart hmac-file-server"
    echo
    echo -e "${GREEN}4. Test XMPP client uploads:${NC}"
    echo "   - Use Conversations, Dino, or Gajim"
    echo "   - No client-side HMAC configuration needed!"
    echo "   - Uploads should work seamlessly"
    echo
    echo -e "${YELLOW}For troubleshooting, check logs:${NC}"
    echo "   journalctl -u ejabberd -f"
    echo "   journalctl -u hmac-file-server -f"
    echo
}

main() {
    print_header
    
    check_requirements
    compile_module
    install_module
    configure_ejabberd
    update_hmac_server
    test_installation
    show_next_steps
    
    print_success "Ejabberd module installation completed!"
    echo -e "${GREEN}Your XMPP clients can now upload files without HMAC configuration!${NC}"
}

# Run main function
main "$@"
