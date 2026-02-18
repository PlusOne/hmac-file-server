#!/bin/bash
# HMAC File Server v3.3 - Debian Package Builder
# Creates .deb packages for AMD64 and ARM64 architectures

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

print_status() {
    echo -e "${GREEN}[BUILD]${NC} $1"
}

print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Project configuration
PROJECT_DIR=$(pwd)
BUILD_DIR=$PROJECT_DIR/build
DEB_DIR=$PROJECT_DIR/debian
PACKAGE_NAME="hmac-file-server"
VERSION="3.4.0"
MAINTAINER="Alex Renz <renz@uuxo.net>"

# Source package for compilation
SOURCE_PKG="./cmd/server/"

print_status "Starting Debian package build for HMAC File Server v$VERSION"
print_info "Building packages for: AMD64, ARM64"

# Check if Go is installed
if ! command -v go &> /dev/null; then
    print_error "Go is not installed or not in PATH"
    exit 1
fi

# Check if dpkg-deb is available
if ! command -v dpkg-deb &> /dev/null; then
    print_error "dpkg-deb is not installed. Please install dpkg-dev package"
    exit 1
fi

# Clean and create required directories
print_info "Setting up build directories..."
rm -rf $BUILD_DIR $DEB_DIR
mkdir -p $BUILD_DIR/{amd64,arm64}
mkdir -p $DEB_DIR/DEBIAN 
mkdir -p $DEB_DIR/usr/local/bin 
mkdir -p $DEB_DIR/etc/hmac-file-server 
mkdir -p $DEB_DIR/var/lib/hmac-file-server/{uploads,deduplication,runtime}
mkdir -p $DEB_DIR/var/log/hmac-file-server
mkdir -p $DEB_DIR/usr/share/doc/hmac-file-server
mkdir -p $DEB_DIR/lib/systemd/system

# Compile Go binaries for both architectures
print_status "Compiling binaries..."
for ARCH in amd64 arm64; do
    print_info "Building for $ARCH..."
    
    # Set cross-compilation environment
    export GOOS=linux
    export GOARCH=$ARCH
    export CGO_ENABLED=0
    
    # Build hmac-file-server
    if go build -ldflags="-w -s" -o $BUILD_DIR/$ARCH/hmac-file-server $SOURCE_PKG; then
        SIZE=$(stat -c%s "$BUILD_DIR/$ARCH/hmac-file-server" | awk '{printf "%.1fMB", $1/1024/1024}')
        print_info "   $ARCH binary built successfully ($SIZE)"
    else
        print_error "Failed to build $ARCH binary"
        exit 1
    fi
done

# Reset environment variables
unset GOOS GOARCH CGO_ENABLED

# Prepare Debian control file template
print_info "Creating package metadata..."
CONTROL_TEMPLATE=$DEB_DIR/DEBIAN/control.template
cat <<EOF > $CONTROL_TEMPLATE
Package: $PACKAGE_NAME
Version: $VERSION
Architecture: ARCH_PLACEHOLDER
Maintainer: $MAINTAINER
Depends: redis-server, clamav, clamav-daemon
Recommends: nginx
Section: net
Priority: optional
Homepage: https://git.uuxo.net/uuxo/hmac-file-server/
Description: HMAC File Server v3.3 - Enterprise XMPP File Sharing
 A lightweight, secure file server designed for XMPP environments with
 enterprise-grade features including:
 .
  * HMAC-based authentication and JWT support
  * Redis integration for session management
  * ClamAV virus scanning for uploaded files
  * Prometheus metrics for monitoring
  * Chunked upload/download support
  * File deduplication capabilities
  * Comprehensive configuration validation
 .
 Perfect for Prosody, Ejabberd, and other XMPP servers requiring
 secure file sharing capabilities with professional deployment features.
EOF

# Prepare systemd service file
print_info "Creating systemd service configuration..."
cat <<EOF > $DEB_DIR/lib/systemd/system/hmac-file-server.service
[Unit]
Description=HMAC File Server 3.3
Documentation=https://git.uuxo.net/uuxo/hmac-file-server/
After=network.target
Wants=network-online.target
After=redis.service
After=clamav-daemon.service

[Service]
Type=simple
User=hmac-file-server
Group=hmac-file-server
ExecStart=/usr/local/bin/hmac-file-server -config /etc/hmac-file-server/config.toml
ExecReload=/bin/kill -SIGHUP \$MAINPID
WorkingDirectory=/var/lib/hmac-file-server
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
ReadWritePaths=/var/lib/hmac-file-server /var/log/hmac-file-server
CapabilityBoundingSet=CAP_NET_BIND_SERVICE
AmbientCapabilities=CAP_NET_BIND_SERVICE

# Resource limits
LimitNOFILE=65536
LimitNPROC=4096

[Install]
WantedBy=multi-user.target
EOF

# Prepare example configuration file
print_info "Creating example configuration..."
cat <<EOF > $DEB_DIR/etc/hmac-file-server/config.toml
# HMAC File Server v3.3 Configuration
# Complete configuration reference: https://git.uuxo.net/uuxo/hmac-file-server/blob/main/WIKI.MD

[server]
bind_ip = "127.0.0.1"
listenport = "8080"
unixsocket = false
storagepath = "/var/lib/hmac-file-server/uploads"
metricsenabled = true
metricsport = "9090"
deduplicationenabled = true
deduplicationpath = "/var/lib/hmac-file-server/deduplication"
filenaming = "HMAC"
force_protocol = "auto"
sslenabled = false
pidfilepath = "/var/lib/hmac-file-server/runtime/hmac-file-server.pid"

[security]
secret = "CHANGE_THIS_SECRET_IN_PRODUCTION_USE_48_CHARS_MIN"
enablejwt = false
jwtsecret = ""
jwtalgorithm = "HS256"
jwtexpiration = "24h"

[uploads]
allowedextensions = [".txt", ".pdf", ".jpg", ".jpeg", ".png", ".gif", ".webp", ".zip", ".tar", ".gz", ".7z", ".mp4", ".webm", ".ogg", ".mp3", ".wav", ".flac", ".doc", ".docx", ".xls", ".xlsx", ".ppt", ".pptx", ".odt", ".ods", ".odp"]
maxfilesize = "100MB"
chunkeduploadsenabled = true
chunksize = "10MB"
resumableuploadsenabled = true
ttlenabled = false
ttl = "168h"
networkevents = true

# Network Resilience Configuration (3.3 Enhanced Features)
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
file = "/var/log/hmac-file-server/hmac-file-server.log"
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
socket = "/var/run/clamav/clamd.ctl"
timeout = "30s"

[redis]
enabled = false
address = "localhost:6379"
database = 0
password = ""
EOF

# Prepare post-installation script
print_info "Creating installation scripts..."
cat <<EOF > $DEB_DIR/DEBIAN/postinst
#!/bin/bash
set -e

# Create hmac-file-server user and group if they do not exist
if ! id -u hmac-file-server >/dev/null 2>&1; then
    useradd --system --no-create-home --shell /usr/sbin/nologin --home-dir /var/lib/hmac-file-server hmac-file-server
    echo "Created system user: hmac-file-server"
fi

# Set proper ownership and permissions
chown -R hmac-file-server:hmac-file-server /var/lib/hmac-file-server
chown -R hmac-file-server:hmac-file-server /var/log/hmac-file-server
chown hmac-file-server:hmac-file-server /etc/hmac-file-server/config.toml

# Set directory permissions
chmod 755 /var/lib/hmac-file-server
chmod 755 /var/lib/hmac-file-server/uploads
chmod 755 /var/lib/hmac-file-server/deduplication
chmod 755 /var/lib/hmac-file-server/runtime
chmod 755 /var/log/hmac-file-server
chmod 640 /etc/hmac-file-server/config.toml

# Reload systemd and enable service
systemctl daemon-reload
systemctl enable hmac-file-server.service

echo ""
echo "Installation complete! Configure /etc/hmac-file-server/config.toml and start:"
echo "sudo systemctl enable --now hmac-file-server"
echo ""
echo "Documentation: https://git.uuxo.net/uuxo/hmac-file-server/"
EOF
chmod 0755 $DEB_DIR/DEBIAN/postinst

# Prepare pre-removal script
cat <<EOF > $DEB_DIR/DEBIAN/prerm
#!/bin/bash
set -e

# Stop the service before removal
if systemctl is-active --quiet hmac-file-server.service; then
    echo "Stopping HMAC File Server service..."
    systemctl stop hmac-file-server.service || true
fi
EOF
chmod 0755 $DEB_DIR/DEBIAN/prerm

# Prepare post-removal script
cat <<EOF > $DEB_DIR/DEBIAN/postrm
#!/bin/bash
set -e

case "\$1" in
    purge)
        # Remove systemd service
        systemctl disable hmac-file-server.service >/dev/null 2>&1 || true
        rm -f /lib/systemd/system/hmac-file-server.service
        systemctl daemon-reload >/dev/null 2>&1 || true

        # Remove user and group
        if id -u hmac-file-server >/dev/null 2>&1; then
            userdel hmac-file-server || true
        fi
        if getent group hmac-file-server >/dev/null 2>&1; then
            groupdel hmac-file-server || true
        fi

        # Remove data directories (ask user)
        echo ""
        echo "HMAC File Server has been removed."
        echo "Data directories remain at:"
        echo "  - /var/lib/hmac-file-server/"
        echo "  - /var/log/hmac-file-server/"
        echo "  - /etc/hmac-file-server/"
        echo ""
        echo "Remove them manually if no longer needed:"
        echo "  sudo rm -rf /var/lib/hmac-file-server"
        echo "  sudo rm -rf /var/log/hmac-file-server"
        echo "  sudo rm -rf /etc/hmac-file-server"
        echo ""
        ;;
    remove)
        # Just disable service
        systemctl disable hmac-file-server.service >/dev/null 2>&1 || true
        systemctl daemon-reload >/dev/null 2>&1 || true
        ;;
esac
EOF
chmod 0755 $DEB_DIR/DEBIAN/postrm

# Prepare documentation
print_info "Including documentation..."
cp README.md $DEB_DIR/usr/share/doc/hmac-file-server/ 2>/dev/null || true
cp WIKI.md $DEB_DIR/usr/share/doc/hmac-file-server/ 2>/dev/null || true
cp LICENSE $DEB_DIR/usr/share/doc/hmac-file-server/ 2>/dev/null || true

# Create .deb packages
print_status "Building Debian packages..."
for ARCH in amd64 arm64; do
    print_info "Creating package for $ARCH..."
    
    # Update control file for the current architecture
    sed "s/ARCH_PLACEHOLDER/$ARCH/" $CONTROL_TEMPLATE > $DEB_DIR/DEBIAN/control

    # Copy binary for current architecture
    cp $BUILD_DIR/$ARCH/hmac-file-server $DEB_DIR/usr/local/bin/

    # Calculate installed size
    INSTALLED_SIZE=$(du -sk $DEB_DIR | cut -f1)
    echo "Installed-Size: $INSTALLED_SIZE" >> $DEB_DIR/DEBIAN/control

    # Ensure proper permissions
    find $DEB_DIR -type d -exec chmod 755 {} \;
    find $DEB_DIR -type f -exec chmod 644 {} \;
    chmod 0755 $DEB_DIR/usr/local/bin/hmac-file-server
    chmod 0755 $DEB_DIR/DEBIAN/postinst
    chmod 0755 $DEB_DIR/DEBIAN/prerm
    chmod 0755 $DEB_DIR/DEBIAN/postrm

    # Build the .deb package
    PACKAGE_FILE="${PACKAGE_NAME}_${VERSION}_${ARCH}.deb"
    if dpkg-deb --build $DEB_DIR $PACKAGE_FILE; then
        SIZE=$(stat -c%s "$PACKAGE_FILE" | awk '{printf "%.1fMB", $1/1024/1024}')
        print_info "   Package created: $PACKAGE_FILE ($SIZE)"
    else
        print_error "Failed to create package for $ARCH"
        exit 1
    fi
    
    # Clean up binary for next build
    rm -f $DEB_DIR/usr/local/bin/hmac-file-server
    rm -f $DEB_DIR/DEBIAN/control
done

# Cleanup temporary directories
print_info "Cleaning up build directories..."
rm -rf $BUILD_DIR $DEB_DIR

# Show results
print_status "Debian package build completed!"
echo ""
print_info "Generated packages:"
for PACKAGE in ${PACKAGE_NAME}_${VERSION}_*.deb; do
    if [[ -f "$PACKAGE" ]]; then
        SIZE=$(stat -c%s "$PACKAGE" | awk '{printf "%.1fMB", $1/1024/1024}')
        print_info "   $PACKAGE ($SIZE)"
    fi
done

echo ""
print_info "Installation commands:"
echo "   sudo dpkg -i ${PACKAGE_NAME}_${VERSION}_amd64.deb"
echo "   sudo dpkg -i ${PACKAGE_NAME}_${VERSION}_arm64.deb"
echo ""
print_info "Package information:"
echo "   dpkg -I ${PACKAGE_NAME}_${VERSION}_amd64.deb"
echo "   dpkg -c ${PACKAGE_NAME}_${VERSION}_amd64.deb"
echo ""
print_warning "Remember to:"
echo "   1. Edit /etc/hmac-file-server/config.toml"
echo "   2. Change the default secret"
echo "   3. Configure Redis/ClamAV if needed"
echo "   4. Start the service: systemctl start hmac-file-server"

exit 0
