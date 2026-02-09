# HMAC File Server Configuration Examples

This directory contains example configuration files for various deployment scenarios.

## Available Configurations

- **config-enhanced-security.toml** - Security-hardened configuration with enhanced authentication
- **config-mobile-resilient.toml** - Optimized for mobile XMPP clients with extended grace periods
- **config-network-switching.toml** - Advanced network resilience for WiFi ↔ LTE switching
- **config-production-enhanced.toml** - Production-ready configuration with performance optimizations
- **config-production-validated.toml** - Validated production configuration for enterprise deployment

## New in v3.4.0 "Cascade"

The following config sections were added in v3.4.0 and can be appended to any existing config:

```toml
# Rate limiting (per JID and/or per IP)
[rate_limit]
enabled = true
requests_per_minute = 60
burst_size = 10
by_jid = true
by_ip = true
whitelisted_ips = ["127.0.0.1"]
whitelisted_jids = []

# Automatic HMAC key rotation
[key_rotation]
enabled = true
rotation_interval = "720h"
grace_period = "48h"
key_storage = "/etc/hmac-file-server/keys.json"

# SQLite metadata tracking
[metadata]
enabled = true
db_path = "/var/lib/hmac-file-server/metadata/files.db"
purge_age = "90d"
```

## Usage

Copy the example that best fits your use case and customize it:

```bash
cp examples/config-production-validated.toml config.toml
# Edit config.toml with your settings
./hmac-file-server -config config.toml
```

## More Templates

See the `templates/` directory for additional configuration templates for specific deployment methods (Docker, Podman, SystemD, Debian).
