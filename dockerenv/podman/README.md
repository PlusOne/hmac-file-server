# HMAC File Server - Podman Configuration Examples

This directory contains Podman-specific deployment files for HMAC File Server 3.2 "Tremora del Terra".

## 🚀 Quick Start

```bash
# Clone repository
git clone https://github.com/PlusOne/hmac-file-server.git
cd hmac-file-server/dockerenv/podman

# Deploy with single command
./deploy-podman.sh

# Check status
./deploy-podman.sh status

# View logs
./deploy-podman.sh logs
```

## 📁 Files Overview

### `Dockerfile.podman`
- **Purpose**: Optimized Dockerfile for Podman deployment
- **Features**: 
  - Security-hardened Alpine-based image
  - Non-root user (UID 1011)
  - Health checks included
  - Static binary compilation
  - Minimal attack surface

### `deploy-podman.sh`
- **Purpose**: Complete deployment automation script
- **Features**:
  - Interactive deployment with colored output
  - Automatic configuration generation with random secrets
  - Security-hardened container settings
  - Pod management for XMPP integration
  - Health monitoring and status reporting

### `hmac-file-server.service`
- **Purpose**: Systemd service unit for service management
- **Usage**: Place in `~/.config/systemd/user/` (rootless) or `/etc/systemd/system/` (system-wide)

## 🛠️ Deployment Commands

### Basic Deployment
```bash
# Full deployment (directories, config, build, start)
./deploy-podman.sh deploy

# Start services only
./deploy-podman.sh start

# Stop all services
./deploy-podman.sh stop

# Restart services
./deploy-podman.sh restart
```

### Management Commands
```bash
# Check status and health
./deploy-podman.sh status

# View real-time logs
./deploy-podman.sh logs

# Show current configuration
./deploy-podman.sh config

# Build image only
./deploy-podman.sh build

# Create networking pod only
./deploy-podman.sh pod

# Complete cleanup (keeps data)
./deploy-podman.sh clean
```

## 🔧 Configuration

### Environment Variables
```bash
# Custom data directory
export APP_DATA="/custom/path/hmac-file-server"

# Custom ports
export LISTEN_PORT="9999"
export METRICS_PORT="9998"

# Deploy with custom settings
./deploy-podman.sh
```

### Generated Configuration
The deployment script generates a production-ready configuration with:
- ✅ **XMPP-compatible file extensions**
- ✅ **Random HMAC and JWT secrets**
- ✅ **Optimized performance settings**
- ✅ **Security hardening enabled**
- ✅ **Comprehensive logging**

## 🔒 Security Features

### Container Security
- **Rootless operation**: Runs as non-root user (UID 1011)
- **Capability dropping**: `--cap-drop=ALL`
- **No new privileges**: `--security-opt no-new-privileges`
- **Read-only filesystem**: `--read-only` with tmpfs for /tmp
- **SELinux labels**: Volume mounts with `:Z` labels

### Network Security
- **Pod isolation**: Containers run in isolated pods
- **Port binding**: Only necessary ports exposed
- **Health monitoring**: Built-in health checks

## 🔄 Systemd Integration

### User Service (Rootless - Recommended)
```bash
# Copy service file
cp hmac-file-server.service ~/.config/systemd/user/

# Enable and start
systemctl --user daemon-reload
systemctl --user enable hmac-file-server.service
systemctl --user start hmac-file-server.service

# Check status
systemctl --user status hmac-file-server.service
```

### System Service (Root)
```bash
# Copy service file
sudo cp hmac-file-server.service /etc/systemd/system/

# Enable and start
sudo systemctl daemon-reload
sudo systemctl enable hmac-file-server.service
sudo systemctl start hmac-file-server.service

# Check status
sudo systemctl status hmac-file-server.service
```

## 🎯 XMPP Integration

### Pod-based XMPP Deployment
```bash
# Create XMPP services pod
podman pod create --name xmpp-services \
  --publish 5222:5222 \
  --publish 5269:5269 \
  --publish 5443:5443 \
  --publish 8888:8888

# Add Prosody XMPP server
podman run -d --pod xmpp-services --name prosody \
  -v ./prosody-config:/etc/prosody:ro \
  -v ./prosody-data:/var/lib/prosody:rw \
  docker.io/prosody/prosody:latest

# Add HMAC File Server
podman run -d --pod xmpp-services --name hmac-file-server \
  -v ./config.toml:/app/config.toml:ro \
  -v ./data:/data:rw \
  localhost/hmac-file-server:latest -config /app/config.toml
```

## 📊 Monitoring and Health

### Health Checks
```bash
# Manual health check
curl -f http://localhost:8888/health

# Container health status
podman healthcheck run hmac-file-server

# Continuous monitoring
watch -n 5 'curl -s http://localhost:8888/health && echo " - $(date)"'
```

### Metrics
```bash
# Prometheus metrics
curl http://localhost:9090/metrics

# Pod statistics
podman pod stats xmpp-pod

# Container logs
podman logs -f hmac-file-server
```

## 🚨 Troubleshooting

### Common Issues

#### Permission Errors
```bash
# Fix SELinux contexts
restorecon -R /opt/podman/hmac-file-server

# Check volume permissions
podman unshare ls -la /opt/podman/hmac-file-server
```

#### Container Won't Start
```bash
# Check image exists
podman images | grep hmac-file-server

# Validate configuration
./deploy-podman.sh config

# Debug with interactive container
podman run -it --rm localhost/hmac-file-server:latest /bin/sh
```

#### Network Issues
```bash
# Check pod networking
podman pod ps
podman port hmac-file-server

# Test connectivity
nc -zv localhost 8888
```

### Log Analysis
```bash
# Container logs
podman logs hmac-file-server

# Application logs
tail -f /opt/podman/hmac-file-server/logs/hmac-file-server.log

# System journal
journalctl --user -u hmac-file-server.service -f
```

## 🎉 Success Verification

After deployment, verify everything works:

1. **Health Check**: `curl -f http://localhost:8888/health`
2. **Metrics**: `curl http://localhost:9090/metrics`
3. **Container Status**: `podman ps`
4. **Pod Status**: `podman pod ps`
5. **Logs**: `./deploy-podman.sh logs`

## 📚 Additional Resources

- [Podman Official Documentation](https://docs.podman.io/)
- [HMAC File Server GitHub](https://github.com/PlusOne/hmac-file-server)
- [XEP-0363 Specification](https://xmpp.org/extensions/xep-0363.html)
- [Container Security Best Practices](https://docs.podman.io/en/latest/markdown/podman-run.1.html#security-options)
