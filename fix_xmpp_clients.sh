#!/bin/bash
# 🧹 XMPP Client Cache Cleaner for Upload Issues
# Fixes Dino and Gajim upload problems after restart
# Date: August 26, 2025

set -euo pipefail

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}🧹 XMPP CLIENT CACHE CLEANER${NC}"
echo "=============================="
echo "Fixing Dino and Gajim upload issues after restart"
echo ""

# Function to safely stop processes
stop_process() {
    local process_name="$1"
    echo -e "${YELLOW}🛑 Stopping $process_name...${NC}"
    
    if pgrep -f "$process_name" >/dev/null; then
        pkill -f "$process_name"
        sleep 2
        
        # Force kill if still running
        if pgrep -f "$process_name" >/dev/null; then
            pkill -9 -f "$process_name" 2>/dev/null || true
            sleep 1
        fi
        
        if ! pgrep -f "$process_name" >/dev/null; then
            echo -e "${GREEN}✅ $process_name stopped${NC}"
        else
            echo -e "${RED}⚠️  $process_name may still be running${NC}"
        fi
    else
        echo -e "${GREEN}✅ $process_name not running${NC}"
    fi
}

# Function to clear cache directory
clear_cache() {
    local app_name="$1"
    local cache_dir="$2"
    
    if [ -d "$cache_dir" ]; then
        echo -e "${YELLOW}🗑️  Clearing $app_name cache: $cache_dir${NC}"
        rm -rf "$cache_dir" 2>/dev/null || true
        echo -e "${GREEN}✅ $app_name cache cleared${NC}"
    else
        echo -e "${BLUE}ℹ️  $app_name cache not found: $cache_dir${NC}"
    fi
}

# Function to clear upload-related files
clear_upload_files() {
    local app_name="$1"
    local data_dir="$2"
    
    if [ -d "$data_dir" ]; then
        echo -e "${YELLOW}🔍 Clearing $app_name upload-related files...${NC}"
        
        # Find and remove upload/token related files
        local files_removed=0
        for pattern in "*upload*" "*token*" "*session*" "*cache*"; do
            while IFS= read -r -d '' file; do
                rm -f "$file" 2>/dev/null && ((files_removed++)) || true
            done < <(find "$data_dir" -name "$pattern" -type f -print0 2>/dev/null || true)
        done
        
        if [ $files_removed -gt 0 ]; then
            echo -e "${GREEN}✅ Removed $files_removed upload-related files from $app_name${NC}"
        else
            echo -e "${BLUE}ℹ️  No upload-related files found in $app_name${NC}"
        fi
    else
        echo -e "${BLUE}ℹ️  $app_name data directory not found: $data_dir${NC}"
    fi
}

# Function to backup data (optional)
backup_data() {
    local app_name="$1"
    local data_dir="$2"
    local backup_dir="${data_dir}.backup.$(date +%Y%m%d_%H%M%S)"
    
    if [ -d "$data_dir" ]; then
        echo -e "${YELLOW}💾 Creating backup of $app_name data...${NC}"
        if cp -r "$data_dir" "$backup_dir" 2>/dev/null; then
            echo -e "${GREEN}✅ Backup created: $backup_dir${NC}"
        else
            echo -e "${RED}⚠️  Failed to create backup for $app_name${NC}"
        fi
    fi
}

# Main execution
echo -e "${BLUE}Step 1: Stopping XMPP clients${NC}"
echo "-----------------------------"
stop_process "dino"
stop_process "gajim"

echo ""
echo -e "${BLUE}Step 2: Creating backups (optional)${NC}"
echo "-----------------------------------"
if [ "${1:-}" = "--backup" ]; then
    backup_data "Dino" "$HOME/.local/share/dino"
    backup_data "Gajim" "$HOME/.local/share/gajim"
else
    echo -e "${YELLOW}ℹ️  Skipping backups (use --backup flag to create backups)${NC}"
fi

echo ""
echo -e "${BLUE}Step 3: Clearing caches${NC}"
echo "---------------------"
clear_cache "Dino" "$HOME/.cache/dino"
clear_cache "Gajim" "$HOME/.cache/gajim"

echo ""
echo -e "${BLUE}Step 4: Clearing upload-related files${NC}"
echo "------------------------------------"
clear_upload_files "Dino" "$HOME/.local/share/dino"
clear_upload_files "Gajim" "$HOME/.local/share/gajim"

echo ""
echo -e "${BLUE}Step 5: Restarting XMPP clients${NC}"
echo "------------------------------"

# Check if display is available
if [ -z "${DISPLAY:-}" ]; then
    echo -e "${RED}⚠️  No DISPLAY environment variable - cannot start GUI clients${NC}"
    echo "Please manually start Dino and Gajim after setting DISPLAY"
else
    echo -e "${YELLOW}🚀 Starting Dino...${NC}"
    if command -v dino >/dev/null 2>&1; then
        dino &
        echo -e "${GREEN}✅ Dino started${NC}"
    else
        echo -e "${RED}❌ Dino not found in PATH${NC}"
    fi
    
    echo -e "${YELLOW}🚀 Starting Gajim...${NC}"
    if command -v gajim >/dev/null 2>&1; then
        gajim &
        echo -e "${GREEN}✅ Gajim started${NC}"
    else
        echo -e "${RED}❌ Gajim not found in PATH${NC}"
    fi
fi

echo ""
echo -e "${GREEN}🎉 CLEANUP COMPLETE!${NC}"
echo "==================="
echo ""
echo -e "${GREEN}✅ What was done:${NC}"
echo "   • Stopped Dino and Gajim processes"
echo "   • Cleared application caches"
echo "   • Removed upload/token related files"
echo "   • Restarted XMPP clients"
echo ""
echo -e "${BLUE}🧪 Next steps:${NC}"
echo "   1. Wait for clients to fully load"
echo "   2. Try uploading a small file in both clients"
echo "   3. Upload should work with fresh authentication"
echo ""
echo -e "${YELLOW}📋 If upload still fails:${NC}"
echo "   • Check server logs: tail -f /var/log/hmac-file-server-mobile.log"
echo "   • Use enhanced server: ./hmac-file-server-desktop-fixed -config config-mobile-resilient.toml"
echo "   • Check network configuration with: ip addr show"
echo ""
echo "Cache cleanup completed at $(date)"
