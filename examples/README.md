# HMAC File Server Configuration Examples

This directory contains example configuration files for various deployment scenarios.

## Available Configurations

- **config-enhanced-security.toml** - Security-hardened configuration with enhanced authentication
- **config-mobile-resilient.toml** - Optimized for mobile XMPP clients with extended grace periods
- **config-network-switching.toml** - Advanced network resilience for WiFi ↔ LTE switching
- **config-production-enhanced.toml** - Production-ready configuration with performance optimizations
- **config-production-validated.toml** - Validated production configuration for enterprise deployment

## Usage

Copy the example that best fits your use case and customize it:

```bash
cp examples/config-production-validated.toml config.toml
# Edit config.toml with your settings
./hmac-file-server -config config.toml
```

## More Templates

See the `templates/` directory for additional configuration templates for specific deployment methods (Docker, Podman, SystemD, Debian).
