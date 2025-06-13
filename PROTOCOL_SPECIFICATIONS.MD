# HMAC File Server Authentication Protocol Specifications

This document outlines the different authentication protocols supported by the HMAC File Server for secure file uploads and downloads. The server supports multiple authentication methods to ensure backward compatibility while providing enhanced security features.

## Overview

The HMAC File Server supports two primary authentication mechanisms:
1. **HMAC-based Authentication** (Multiple versions: v1, v2, token, v3)
2. **JWT Authentication** (Bearer tokens)

All protocols use SHA256 hashing and require a shared secret key configured on the server.

---

## HMAC Authentication Protocols

### Common Elements
- **Algorithm**: HMAC-SHA256
- **Secret**: Shared secret key configured in `[security]` section
- **Transport**: URL query parameters for HMAC, headers for signatures
- **Encoding**: Hexadecimal encoding for HMAC values

---

### Legacy v1 Protocol (`v` parameter)

**Overview**: The original HMAC authentication protocol.

**URL Format**:
```
PUT /filename.ext?v=HMAC_SIGNATURE
```

**Message Construction**:
```
fileStorePath + "\x20" + contentLength
```

**Example**:
```bash
# For file "test.txt" with 1024 bytes
# Message: "test.txt\x201024"
curl -X PUT "http://localhost:8080/test.txt?v=a1b2c3d4..." --data-binary @test.txt
```

**Implementation Notes**:
- Uses space character (`\x20`) as separator
- Content-Length header must be accurate
- Simplest protocol, minimal metadata validation

---

### Enhanced v2 Protocol (`v2` parameter)

**Overview**: Enhanced version including content type validation.

**URL Format**:
```
PUT /filename.ext?v2=HMAC_SIGNATURE
```

**Message Construction**:
```
fileStorePath + "\x00" + contentLength + "\x00" + contentType
```

**Example**:
```bash
# For file "document.pdf" with 2048 bytes
# Message: "document.pdf\x002048\x00application/pdf"
curl -X PUT "http://localhost:8080/document.pdf?v2=e5f6g7h8..." --data-binary @document.pdf
```

**Implementation Notes**:
- Uses null characters (`\x00`) as separators
- Content-Type automatically detected from file extension
- Fallback to "application/octet-stream" for unknown extensions

---

### Token Protocol (`token` parameter)

**Overview**: Alternative parameter name for v2-style authentication.

**URL Format**:
```
PUT /filename.ext?token=HMAC_SIGNATURE
```

**Message Construction**: Same as v2 protocol
```
fileStorePath + "\x00" + contentLength + "\x00" + contentType
```

**Example**:
```bash
curl -X PUT "http://localhost:8080/image.jpg?token=i9j0k1l2..." --data-binary @image.jpg
```

**Implementation Notes**:
- Identical to v2 protocol but uses `token` parameter
- Useful for clients that prefer different parameter naming

---

### v3 Protocol - mod_http_upload_external Compatible (`v3` parameter)

**Overview**: Specifically designed for Prosody's `mod_http_upload_external` compatibility with expiration support.

**URL Format**:
```
PUT /path/to/file.ext?v3=HMAC_SIGNATURE&expires=UNIX_TIMESTAMP
```

**Message Construction**:
```
METHOD + "\n" + expires + "\n" + requestPath
```

**Example**:
```bash
# Current timestamp: 1717804800
# Message: "PUT\n1717804800\n/upload/myfile.txt"
curl -X PUT "http://localhost:8080/upload/myfile.txt?v3=m3n4o5p6...&expires=1717804800" --data-binary @myfile.txt
```

**Verification Process**:
1. Extract `v3` signature and `expires` timestamp
2. Validate `expires` is in the future
3. Construct message: `"{METHOD}\n{expires}\n{path}"`
4. Calculate HMAC-SHA256 of message
5. Compare with provided signature

**Implementation Notes**:
- Includes expiration timestamp validation
- Prevents replay attacks through time-based validation
- Path-only signing (no query parameters in signed message)
- HTTP method is part of the signed message

---

## JWT Authentication

**Overview**: Token-based authentication using JSON Web Tokens.

**Configuration**:
```toml
[security]
enablejwt = true
jwtsecret = "your-256-bit-secret"
jwtalgorithm = "HS256"
jwtexpiration = "24h"
```

**Header Format**:
```
Authorization: Bearer JWT_TOKEN
```

**Fallback Query Parameter**:
```
GET /file.txt?token=JWT_TOKEN
```

**Example Usage**:
```bash
# Header-based JWT
curl -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIs..." http://localhost:8080/file.txt

# Query parameter fallback
curl "http://localhost:8080/file.txt?token=eyJhbGciOiJIUzI1NiIs..."
```

**JWT Claims**: Standard JWT claims (exp, iat, iss, etc.) as configured.

---

## POST Upload Authentication

### X-Signature Header Method

**Overview**: For multipart form uploads via POST requests.

**Header Format**:
```
X-Signature: HMAC_OF_REQUEST_PATH
```

**Message Construction**:
```
requestPath (e.g., "/upload")
```

**Example**:
```bash
# HMAC of "/upload"
curl -X POST \
  -H "X-Signature: CALCULATED_HMAC" \
  -F 'file=@myfile.txt' \
  http://localhost:8080/upload
```

---

## Authentication Priority and Fallbacks

The server checks authentication in the following order:

1. **JWT Authentication** (if `enablejwt = true`)
   - Authorization header (Bearer token)
   - Query parameter `token`

2. **HMAC Authentication** (if JWT disabled or not found)
   - X-Signature header (for POST uploads)
   - v3 protocol (with expires validation)
   - v2 protocol 
   - token protocol
   - v1 protocol (legacy)

---

## Security Considerations

### HMAC Protocols
- **Secret Management**: Use strong, randomly generated secrets
- **Time Validation**: v3 protocol includes expiration to prevent replay attacks
- **Content Validation**: v2/token protocols include content-type validation
- **Path Sanitization**: All protocols validate and sanitize file paths

### JWT Authentication
- **Token Expiration**: Configure appropriate expiration times
- **Secret Rotation**: Regularly rotate JWT signing keys
- **Algorithm Security**: Default HS256 is secure for most use cases
- **Transport Security**: Always use HTTPS in production

### General Security
- **HTTPS Only**: Use TLS encryption for all production deployments
- **Rate Limiting**: Implement reverse proxy rate limiting
- **File Validation**: Configure allowed file extensions
- **Virus Scanning**: Enable ClamAV integration for malware detection
- **Access Logs**: Monitor authentication failures and suspicious activity

---

## Migration Guide

### From v1 to v2
- Update HMAC calculation to include content type
- Change separator from `\x20` to `\x00`
- No breaking changes in URL structure

### From HMAC to JWT
- Set `enablejwt = true` in configuration
- Generate JWT tokens server-side or use external auth provider
- HMAC authentication remains available as fallback

### Adding v3 Support
- Implement expiration timestamp generation
- Update HMAC calculation to include HTTP method and expiration
- Useful for mod_http_upload_external compatibility

---

## Example Implementations

### Client-Side HMAC Generation (Python)
```python
import hmac
import hashlib
import time

def generate_v3_signature(secret, method, expires, path):
    message = f"{method}\n{expires}\n{path}"
    signature = hmac.new(
        secret.encode(), 
        message.encode(), 
        hashlib.sha256
    ).hexdigest()
    return signature

# Example usage
secret = "your-hmac-secret"
expires = int(time.time()) + 3600  # 1 hour from now
signature = generate_v3_signature(secret, "PUT", expires, "/upload/file.txt")
```

### Server-Side Validation (Reference)
See the main.go file for complete implementation details of all validation functions:
- `validateHMAC()`: Legacy v1, v2, and token protocols
- `validateV3HMAC()`: v3 protocol with expiration
- `validateJWTFromRequest()`: JWT validation

---

This specification ensures consistent implementation across clients and provides multiple authentication options for different use cases and security requirements.
