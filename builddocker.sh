#!/bin/bash

set -e

# Enhanced Container Build Script - Supports Docker & Podman
# HMAC File Server 3.3.0 - Universal Container Support

IMAGE_NAME="hmac-file-server"
DOCKERFILE_PATH="dockerenv/dockerbuild/Dockerfile"

# Select appropriate compose file based on engine
get_compose_file() {
    local engine="$1"
    if [ "$engine" = "podman" ] && [ -f "dockerenv/podman-compose.yml" ]; then
        echo "dockerenv/podman-compose.yml"
    else
        echo "dockerenv/docker-compose.yml"
    fi
}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to detect available container engines
detect_container_engines() {
    local engines=()
    
    if command -v docker &> /dev/null; then
        engines+=("docker")
    fi
    
    if command -v podman &> /dev/null; then
        engines+=("podman")
    fi
    
    echo "${engines[@]}"
}

# Function to select container engine
select_container_engine() {
    local available_engines=($(detect_container_engines))
    
    if [ ${#available_engines[@]} -eq 0 ]; then
        echo -e "${RED}❌ Error: Neither Docker nor Podman is installed${NC}"
        echo "Please install Docker or Podman to continue"
        exit 1
    fi
    
    # Check for user preference via argument
    if [ "$1" = "docker" ] || [ "$1" = "podman" ]; then
        local requested_engine="$1"
        for engine in "${available_engines[@]}"; do
            if [ "$engine" = "$requested_engine" ]; then
                echo "$requested_engine"
                return 0
            fi
        done
        echo -e "${RED}❌ Error: $requested_engine is not available${NC}"
        exit 1
    fi
    
    # If only one engine available, use it
    if [ ${#available_engines[@]} -eq 1 ]; then
        echo "${available_engines[0]}"
        return 0
    fi
    
    # Multiple engines available, let user choose
    echo -e "${BLUE}🐳 Multiple container engines detected:${NC}"
    for i in "${!available_engines[@]}"; do
        echo "  $((i+1))) ${available_engines[i]}"
    done
    
    while true; do
        read -p "Select container engine (1-${#available_engines[@]}): " choice
        if [[ "$choice" =~ ^[0-9]+$ ]] && [ "$choice" -ge 1 ] && [ "$choice" -le ${#available_engines[@]} ]; then
            echo "${available_engines[$((choice-1))]}"
            return 0
        fi
        echo "Invalid choice. Please enter a number between 1 and ${#available_engines[@]}"
    done
}

# Function to get compose command based on engine
get_compose_command() {
    local engine="$1"
    
    case "$engine" in
        "docker")
            if command -v docker-compose &> /dev/null; then
                echo "docker-compose"
            elif docker compose version &> /dev/null; then
                echo "docker compose"
            else
                echo ""
            fi
            ;;
        "podman")
            if command -v podman-compose &> /dev/null; then
                echo "podman-compose"
            else
                echo ""
            fi
            ;;
        *)
            echo ""
            ;;
    esac
}

# Function to build container image
build_image() {
    local engine="$1"
    
    echo -e "${BLUE}🔨 Building container image with $engine...${NC}"
    echo "Image: $IMAGE_NAME"
    echo "Dockerfile: $DOCKERFILE_PATH"
    
    if [ "$engine" = "podman" ]; then
        # Podman specific build
        podman build -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" .
    else
        # Docker build
        docker build -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" .
    fi
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✅ Image built successfully with $engine${NC}"
    else
        echo -e "${RED}❌ Failed to build image with $engine${NC}"
        exit 1
    fi
}

# Function to start services (optional)
start_services() {
    local engine="$1"
    local compose_file=$(get_compose_file "$engine")
    local compose_cmd=$(get_compose_command "$engine")
    
    if [ -z "$compose_cmd" ]; then
        echo -e "${YELLOW}⚠️  No compose command available for $engine${NC}"
        echo "You can start the container manually:"
        if [ "$engine" = "podman" ]; then
            echo "  podman run -d --name hmac-file-server -p 8081:8080 -v ./dockerenv/config:/etc/hmac-file-server:Z -v ./dockerenv/data/uploads:/opt/hmac-file-server/data/uploads:Z $IMAGE_NAME"
        else
            echo "  docker run -d --name hmac-file-server -p 8081:8080 -v ./dockerenv/config:/etc/hmac-file-server -v ./dockerenv/data/uploads:/opt/hmac-file-server/data/uploads $IMAGE_NAME"
        fi
        return 0
    fi
    
    echo -e "${BLUE}🚀 Starting services with $compose_cmd...${NC}"
    echo "Using compose file: $compose_file"
    
    if [ "$compose_cmd" = "docker compose" ]; then
        docker compose -f "$compose_file" up -d
    else
        $compose_cmd -f "$compose_file" up -d
    fi
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✅ Services started successfully${NC}"
        echo "Server accessible at: http://localhost:8081"
    else
        echo -e "${RED}❌ Failed to start services${NC}"
        exit 1
    fi
}

# Main execution
main() {
    echo -e "${BLUE}🐳 HMAC File Server - Universal Container Builder${NC}"
    echo "Version: 3.3.0 - Docker & Podman Support"
    echo

    # Select container engine
    CONTAINER_ENGINE=$(select_container_engine "$1")
    echo -e "${GREEN}📦 Using container engine: $CONTAINER_ENGINE${NC}"
    echo

    # Build image
    build_image "$CONTAINER_ENGINE"
    echo

    # Ask about starting services
    if [ "$2" != "--build-only" ]; then
        read -p "Start services now? (y/n): " start_choice
        if [[ "$start_choice" =~ ^[Yy] ]]; then
            start_services "$CONTAINER_ENGINE"
        else
            echo -e "${YELLOW}ℹ️  Build complete. Services not started.${NC}"
            echo "To start manually, use:"
            local compose_file=$(get_compose_file "$CONTAINER_ENGINE")
            local compose_cmd=$(get_compose_command "$CONTAINER_ENGINE")
            if [ -n "$compose_cmd" ]; then
                if [ "$compose_cmd" = "docker compose" ]; then
                    echo "  docker compose -f $compose_file up -d"
                else
                    echo "  $compose_cmd -f $compose_file up -d"
                fi
            fi
        fi
    fi

    echo
    echo -e "${GREEN}🎉 Container build process completed successfully!${NC}"
}

# Show usage if help requested
if [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
    echo "HMAC File Server - Universal Container Builder"
    echo "Usage: $0 [engine] [options]"
    echo
    echo "Engines:"
    echo "  docker    - Use Docker engine"
    echo "  podman    - Use Podman engine"
    echo "  (auto)    - Auto-detect and select available engine"
    echo
    echo "Options:"
    echo "  --build-only    - Build image only, don't start services"
    echo "  --help, -h      - Show this help message"
    echo
    echo "Examples:"
    echo "  $0                    # Auto-detect engine and interactive mode"
    echo "  $0 docker            # Use Docker specifically"
    echo "  $0 podman --build-only # Use Podman, build only"
    exit 0
fi

# Run main function
main "$@"
