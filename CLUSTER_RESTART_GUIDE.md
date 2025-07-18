# HMAC File Server Cluster Restart Guide

## Overview

When HMAC File Server restarts in a distributed cluster environment, client applications on other VMs may need to be restarted or reconfigured to resume upload functionality. This guide addresses common scenarios and solutions.

## 🚨 **Common Client Restart Scenarios**

### 1. **XMPP Server Integration (XEP-0363)**

**Scenario:**
```
VM1: ejabberd/Prosody XMPP Server
VM2: HMAC File Server (restarted)
```

**Why clients may need restart:**
- XMPP servers cache upload slot endpoints
- HTTP client connection pools become stale
- Upload session tokens may be invalidated
- DNS/service discovery cache issues

**Solutions:**
```bash
# On XMPP Server VM:
systemctl reload ejabberd       # Reload config without full restart
# OR
systemctl restart ejabberd     # Full restart if reload doesn't work

# For Prosody:
systemctl reload prosody
# OR
prosodyctl reload
```

### 2. **Application Servers with HTTP Clients**

**Scenario:**
```
VM1,VM2,VM3: Web Applications
VM4: HMAC File Server (restarted)
```

**Why clients may need restart:**
- HTTP client libraries maintain connection pools
- Upload tokens cached in application memory
- Circuit breakers may be in "open" state
- Load balancer health checks failing

**Solutions:**
```bash
# Restart application servers:
systemctl restart your-app-server

# Or graceful reload if supported:
systemctl reload your-app-server
kill -USR1 $(pgrep -f your-app)
```

### 3. **Load Balancer / Proxy Issues**

**Scenario:**
```
VM1: nginx/haproxy Load Balancer
VM2: HMAC File Server (restarted)
```

**Why restart needed:**
- Upstream connection pooling
- Health check failures
- Backend marking as "down"

**Solutions:**
```bash
# nginx:
nginx -s reload

# haproxy:
systemctl reload haproxy
# OR disable/enable backend:
echo "disable server hmac-backend/server1" | socat stdio /var/run/haproxy.sock
echo "enable server hmac-backend/server1" | socat stdio /var/run/haproxy.sock
```

## 🔧 **Enhanced Configuration for Cluster Resilience**

### Server-Level Settings
```toml
[server]
# Graceful shutdown to allow client reconnections
graceful_shutdown_timeout = "300s"
connection_drain_timeout = "120s"
restart_grace_period = "60s"

# Connection management
max_idle_conns_per_host = 5
idle_conn_timeout = "90s"
client_timeout = "300s"
```

### Upload Session Persistence
```toml
[uploads]
# Enable session persistence across restarts
session_persistence = true
session_recovery_timeout = "300s"
client_reconnect_window = "120s"
upload_slot_ttl = "3600s"
retry_failed_uploads = true
max_upload_retries = 3
```

### Redis-Based Session Sharing
```toml
[redis]
redisenabled = true
redisaddr = "redis-cluster:6379"
# Store upload sessions in Redis for cluster-wide persistence
redishealthcheckinterval = "30s"
```

## 🚀 **Automated Restart Coordination**

### 1. **Service Discovery Integration**

```bash
#!/bin/bash
# restart-coordination.sh
# Notify cluster components of HMAC server restart

HMAC_SERVER_VM="vm2"
XMPP_SERVERS=("vm1" "vm3")
APP_SERVERS=("vm4" "vm5" "vm6")

echo "HMAC File Server restart initiated on $HMAC_SERVER_VM"

# Wait for HMAC server to be ready
while ! curl -s http://$HMAC_SERVER_VM:8080/health > /dev/null; do
    echo "Waiting for HMAC server to be ready..."
    sleep 5
done

echo "HMAC server is ready. Notifying cluster components..."

# Restart XMPP servers
for server in "${XMPP_SERVERS[@]}"; do
    echo "Reloading XMPP server on $server"
    ssh $server "systemctl reload ejabberd || systemctl restart ejabberd"
done

# Restart application servers
for server in "${APP_SERVERS[@]}"; do
    echo "Restarting application server on $server"
    ssh $server "systemctl restart your-app-server"
done

echo "Cluster restart coordination completed"
```

### 2. **Health Check Integration**

```bash
#!/bin/bash
# hmac-health-check.sh
# Advanced health check that validates upload functionality

HMAC_URL="http://localhost:8080"
SECRET="f6g4ldPvQM7O2UTFeBEUUj33VrXypDAcsDt0yqKrLiOr5oQW"

# Test basic connectivity
if ! curl -s -f "$HMAC_URL/health" > /dev/null; then
    echo "CRITICAL: HMAC server not responding"
    exit 2
fi

# Test upload endpoint availability
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" "$HMAC_URL/upload")
if [ "$HTTP_CODE" != "405" ] && [ "$HTTP_CODE" != "401" ]; then
    echo "WARNING: Upload endpoint returning unexpected code: $HTTP_CODE"
    exit 1
fi

echo "OK: HMAC File Server is healthy"
exit 0
```

### 3. **Consul/etcd Integration**

```bash
#!/bin/bash
# consul-hmac-restart.sh
# Integrate with Consul for service discovery

# Deregister service before restart
curl -X PUT "http://consul:8500/v1/agent/service/deregister/hmac-file-server"

# Restart HMAC server
systemctl restart hmac-file-server

# Wait for service to be ready
while ! ./hmac-health-check.sh; do
    sleep 5
done

# Re-register service
curl -X PUT "http://consul:8500/v1/agent/service/register" \
  -d '{
    "ID": "hmac-file-server",
    "Name": "hmac-file-server",
    "Address": "vm2",
    "Port": 8080,
    "Check": {
      "HTTP": "http://vm2:8080/health",
      "Interval": "30s"
    }
  }'

# Trigger dependent service reloads via Consul watches
consul event -name="hmac-restarted" "HMAC File Server restarted on vm2"
```

## 🔍 **Troubleshooting Client Issues**

### Symptoms of Client Restart Needed:
- Upload requests returning "connection refused"
- Timeouts on upload attempts
- "Service temporarily unavailable" errors
- Cached upload slots returning 404/410

### Diagnosis Commands:
```bash
# Check client connection pools
ss -tuln | grep :8080

# Test upload endpoint from client VM
curl -I http://hmac-server:8080/upload

# Check client application logs
journalctl -u your-app-server -f

# Verify DNS resolution
nslookup hmac-server
dig hmac-server
```

### Quick Fixes:
```bash
# Clear client-side DNS cache
systemctl restart systemd-resolved

# Reset client HTTP connections
ss -K dst hmac-server

# Force application reconnection
systemctl restart your-app-server
```

## 🎯 **Best Practices for Production**

### 1. **Rolling Restarts**
- Use multiple HMAC server instances behind load balancer
- Restart one instance at a time
- Monitor client reconnection success

### 2. **Health Check Integration**
- Implement deep health checks that test upload functionality
- Use health check results in load balancer decisions
- Monitor client-side connection success rates

### 3. **Session Persistence**
- Use Redis cluster for session sharing
- Implement upload session recovery
- Provide client reconnection grace periods

### 4. **Monitoring and Alerts**
```bash
# Monitor upload success rates
watch -n 30 'curl -s http://hmac-server:9090/metrics | grep upload_success_total'

# Monitor client connections
watch -n 10 'ss -tuln | grep :8080 | wc -l'

# Monitor Redis session store
redis-cli info keyspace
```

## 📋 **Restart Checklist**

### Before HMAC Server Restart:
- [ ] Identify all client VMs and applications
- [ ] Verify Redis cluster health
- [ ] Check current upload queue status
- [ ] Notify operations team

### During Restart:
- [ ] Execute graceful shutdown
- [ ] Monitor client reconnection attempts
- [ ] Verify upload session recovery
- [ ] Check Redis session persistence

### After Restart:
- [ ] Restart/reload client applications as needed
- [ ] Verify upload functionality from all client VMs
- [ ] Monitor error rates and connection counts
- [ ] Update monitoring dashboards

### Client Applications to Restart:
- [ ] XMPP servers (ejabberd, Prosody)
- [ ] Web application servers
- [ ] API gateway services
- [ ] Load balancers (if upstream issues)
- [ ] Monitoring agents

This comprehensive approach ensures minimal disruption during HMAC File Server restarts in distributed environments.
