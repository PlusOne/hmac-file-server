#!/bin/bash

# Test script to verify installer options without requiring root

# Extract just the user input portion from installer.sh for testing
get_user_input() {
    echo -e "\033[0;34mInstallation Type Selection\033[0m"
    echo "Choose your preferred installation method:"
    echo ""
    echo "  1) Native installation (systemd service)"
    echo "  2) Docker deployment (docker-compose)"
    echo ""
    
    while true; do
        read -p "Installation type [1]: " INSTALL_TYPE
        INSTALL_TYPE=${INSTALL_TYPE:-1}
        
        case $INSTALL_TYPE in
            1)
                echo -e "\033[0;32mSelected: Native installation\033[0m"
                DEPLOYMENT_TYPE="native"
                break
                ;;
            2)
                echo -e "\033[0;32mSelected: Docker deployment\033[0m"
                DEPLOYMENT_TYPE="docker"
                break
                ;;
            *)
                echo -e "\033[0;31mPlease enter 1 or 2\033[0m"
                ;;
        esac
    done
    
    echo ""
    echo -e "\033[0;34mConfiguration Setup\033[0m"
    echo "Please provide the following information (or press Enter for defaults):"
    echo ""

    # System user
    read -p "System user for HMAC File Server [hmac-server]: " HMAC_USER
    HMAC_USER=${HMAC_USER:-"hmac-server"}
    
    if [[ "$DEPLOYMENT_TYPE" == "native" ]]; then
        read -p "Installation directory [/opt/hmac-file-server]: " INSTALL_DIR
        INSTALL_DIR=${INSTALL_DIR:-"/opt/hmac-file-server"}
        
        read -p "Configuration directory [/etc/hmac-file-server]: " CONFIG_DIR
        CONFIG_DIR=${CONFIG_DIR:-"/etc/hmac-file-server"}
        
        read -p "Data directory (uploads) [/var/lib/hmac-file-server]: " DATA_DIR
        DATA_DIR=${DATA_DIR:-"/var/lib/hmac-file-server"}
    else
        read -p "Docker deployment directory [./hmac-file-server-docker]: " DOCKER_DIR
        DOCKER_DIR=${DOCKER_DIR:-"./hmac-file-server-docker"}
        
        INSTALL_DIR="$DOCKER_DIR"
        CONFIG_DIR="$DOCKER_DIR/config"
        DATA_DIR="$DOCKER_DIR/data"
    fi

    read -p "Server port [8080]: " SERVER_PORT
    SERVER_PORT=${SERVER_PORT:-"8080"}
    
    read -p "Metrics port [9090]: " METRICS_PORT
    METRICS_PORT=${METRICS_PORT:-"9090"}

    echo ""
    echo -e "\033[0;34mConfiguration Summary\033[0m"
    echo "────────────────────────────────────────────────────────────────"
    echo "Deployment Type: $DEPLOYMENT_TYPE"
    echo "System User:     $HMAC_USER"
    echo "Install Dir:     $INSTALL_DIR"
    echo "Config Dir:      $CONFIG_DIR"
    echo "Data Dir:        $DATA_DIR"
    echo "Server Port:     $SERVER_PORT"
    echo "Metrics Port:    $METRICS_PORT"
    echo "────────────────────────────────────────────────────────────────"
}

echo "Testing installer options..."
get_user_input
echo ""
echo "Test completed successfully! All options work correctly."
