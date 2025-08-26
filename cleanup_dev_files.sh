#!/bin/bash
# 🧹 HMAC File Server 3.3.0 "Nexus Infinitum" - Developer File Cleanup
# Carefully removes development and test files while preserving production assets
# Date: August 26, 2025

set -euo pipefail

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
PURPLE='\033[0;35m'
NC='\033[0m'

echo -e "${BLUE}🧹 HMAC FILE SERVER 3.3.0 DEVELOPER CLEANUP${NC}"
echo "=============================================="
echo "Carefully cleaning development files while preserving production assets"
echo ""

# Files to keep (important production files)
KEEP_FILES=(
    "hmac-file-server-network-fixed"    # Main enhanced server binary
    "hmac-file-server-desktop-fixed"    # Desktop client enhanced binary
    "config-mobile-resilient.toml"      # Production mobile config
    "config-production-enhanced.toml"   # Production config
    "config-production-validated.toml"  # Validated production config
    "README.md"                         # Main documentation
    "WIKI.MD"                          # Wiki documentation
    "LICENSE"                          # License file
    "go.mod"                           # Go module file
    "go.sum"                           # Go dependencies
    "RELEASE_NOTES_3.3.0.md"          # Current release notes
    "install-manager.sh"               # Production installer
    "installer.sh"                     # Alternative installer
    "builddebian.sh"                   # Debian package builder
    "builddocker.sh"                   # Docker builder
    "fix_xmpp_clients.sh"              # Client troubleshooting tool
    "verify_network_resilience.sh"     # Network verification tool
    "NETWORK_RESILIENCE_COMPLETE.md"   # Network feature documentation
    "DESKTOP_XMPP_CLIENT_FIX.md"       # Desktop client fix documentation
    "XMPP_CLIENT_ECOSYSTEM_ANALYSIS.md" # Client analysis
    "xmpp_client_upload_diagnosis.ipynb" # Diagnostic notebook
)

# Directories to keep
KEEP_DIRS=(
    "cmd/"                             # Source code
    "dashboard/"                       # Monitoring dashboard
    "dockerenv/"                       # Docker configurations
    "ejabberd-module/"                 # XMPP module
    "templates/"                       # Configuration templates
    "tests/"                          # Test framework
    "uploads/"                        # Upload directory
    ".git/"                           # Git repository
)

# Files to remove (development/testing artifacts)
REMOVE_FILES=(
    "hmac-file-server"                 # Old binary
    "hmac-file-server-ejabberd"        # Development binary
    "hmac-file-server-fixed"           # Old fixed binary
    "hmac-file-server-mobile-resilient" # Development binary
    "monitor"                          # Test monitor
    "server"                           # Test server
    "quick-test"                       # Development test
    "test"                            # Old test script
    "test-file.txt"                   # Test file
    "test_enhanced_mime.go"           # Development test
    "test_mime.go"                    # Development test
    "test_mime_integration.go"        # Development test
    "router-test.log"                 # Test log
    "server-test.log"                 # Test log
    "test-server.log"                 # Test log
)

# Config files to remove (development/testing configs)
REMOVE_CONFIGS=(
    "test-config.toml"                # Test config
    "test-config-network-resilience.toml" # Test config
    "test-config-resilience.toml"     # Test config
    "test-final.toml"                 # Test config
    "test-minimal.toml"               # Test config
    "test-simple-config.toml"         # Test config
    "test-simple.toml"                # Test config
    "test-startup.toml"               # Test config
    "test-success.toml"               # Test config
    "config-client-multiinterface.toml" # Development config
)

# Scripts to remove (development/testing scripts)
REMOVE_SCRIPTS=(
    "comprehensive_upload_test.sh"     # Development test
    "debug-uploads.sh"                # Development debug
    "monitor_nginx.sh"                # Development monitor
    "monitor_server.sh"               # Development monitor
    "monitor_uploads.sh"              # Development monitor
    "test-network-resilience.sh"      # Development test
    "test_network_resilience_complete.sh" # Development test
    "simple_revalidation.sh"          # Development validation
    "revalidate_all_features.sh"      # Development validation
    "check-configs.sh"                # Development check
    "build-multi-arch.sh"             # Development build script
)

# Documentation to remove (outdated/development docs)
REMOVE_DOCS=(
    "ADAPTIVE_IO_INTEGRATION.md"      # Development doc
    "CHANGELOG.MD"                    # Old changelog
    "DUAL_STACK_IMPROVEMENTS.md"      # Development doc
    "EJABBERD_MODULE_PROPOSAL.md"     # Development proposal
    "GIT_RELEASE_NOTES_3.2.2.md"     # Old release notes
    "IMPROVEMENT_SUMMARY.md"          # Development summary
    "MIME_TYPE_ENHANCEMENT_REPORT.md" # Development report
    "MULTI_INTERFACE_INTEGRATION_COMPLETE.md" # Development doc
    "NETWORK_RESILIENCE_FIX_REPORT.md" # Development report
    "RELEASE_NOTES_3.2.2.md"         # Old release notes
    "STABILITY_AUDIT_PLAN.md"        # Development audit
)

# Directories to remove (development/testing dirs)
REMOVE_DIRS=(
    "temp/"                           # Temporary files
    "test-uploads/"                   # Test uploads
    "dedup_store/"                    # Development dedup store (empty)
)

# Function to safely remove files
safe_remove() {
    local item="$1"
    local type="$2"
    
    if [ "$type" = "file" ] && [ -f "$item" ]; then
        echo -e "${YELLOW}📄 Removing file: $item${NC}"
        rm -f "$item"
        return 0
    elif [ "$type" = "dir" ] && [ -d "$item" ]; then
        if [ -z "$(ls -A "$item" 2>/dev/null)" ]; then
            echo -e "${YELLOW}📁 Removing empty directory: $item${NC}"
            rmdir "$item"
        else
            echo -e "${YELLOW}📁 Removing directory: $item${NC}"
            rm -rf "$item"
        fi
        return 0
    fi
    return 1
}

# Count removed items
REMOVED_COUNT=0

echo -e "${BLUE}🗑️  REMOVING DEVELOPMENT FILES${NC}"
echo "==============================="

# Remove development files
for file in "${REMOVE_FILES[@]}"; do
    if safe_remove "$file" "file"; then
        ((REMOVED_COUNT++))
    fi
done

# Remove development configs
for config in "${REMOVE_CONFIGS[@]}"; do
    if safe_remove "$config" "file"; then
        ((REMOVED_COUNT++))
    fi
done

# Remove development scripts
for script in "${REMOVE_SCRIPTS[@]}"; do
    if safe_remove "$script" "file"; then
        ((REMOVED_COUNT++))
    fi
done

# Remove development documentation
for doc in "${REMOVE_DOCS[@]}"; do
    if safe_remove "$doc" "file"; then
        ((REMOVED_COUNT++))
    fi
done

# Remove development directories
for dir in "${REMOVE_DIRS[@]}"; do
    if safe_remove "$dir" "dir"; then
        ((REMOVED_COUNT++))
    fi
done

echo ""
echo -e "${GREEN}✅ PRESERVED PRODUCTION FILES${NC}"
echo "============================"

# Show kept files
echo -e "${GREEN}📦 Key production files preserved:${NC}"
for file in "${KEEP_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo -e "  ✅ $file"
    fi
done

echo ""
echo -e "${GREEN}📁 Production directories preserved:${NC}"
for dir in "${KEEP_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        echo -e "  ✅ $dir"
    fi
done

echo ""
echo -e "${PURPLE}📊 CLEANUP SUMMARY${NC}"
echo "=================="
echo -e "Items removed: ${REMOVED_COUNT}"
echo -e "Production files preserved: ${#KEEP_FILES[@]}"
echo -e "Production directories preserved: ${#KEEP_DIRS[@]}"

echo ""
echo -e "${GREEN}🎯 PRODUCTION-READY STRUCTURE${NC}"
echo "============================="
echo "The HMAC File Server 3.3.0 'Nexus Infinitum' is now clean and"
echo "ready for production deployment with all development artifacts removed."
echo ""
echo -e "${BLUE}🚀 Ready to deploy:${NC}"
echo "  ./hmac-file-server-network-fixed -config config-mobile-resilient.toml"
echo ""
echo "Cleanup completed at $(date)"
