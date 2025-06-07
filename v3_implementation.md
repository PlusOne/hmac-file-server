# HMAC File Server v3 Protocol Implementation

This document describes the v3 protocol for secure HTTP file upload and download, as implemented in the HMAC File Server. It is intended for developers, integrators, and XMPP server administrators who want to use or implement v3-compatible clients or services.

---

## Overview

The v3 protocol extends the v2 protocol by adding support for uploader identity, timestamp-based replay protection, and a more robust signed string format. It is fully backward compatible: the server supports v1, v2, and v3 in parallel, always using the highest version present in the request.

---

## Signed String Construction (v3)

The signed string for v3 is constructed as follows:

```
signed_string = file_path + '\x01' + content_length + '\x01' + content_type + '\x01' + uploader + '\x01' + timestamp
```

- **file_path**: The relative path of the file (as in v2).
- **content_length**: The value of the HTTP `Content-Length` header.
- **content_type**: The value of the HTTP `Content-Type` header (or inferred from file extension).
- **uploader**: The uploader's identity (e.g., XMPP JID), provided as the `X-Uploader` header or `uploader` query parameter.
- **timestamp**: The UNIX timestamp (seconds since epoch), provided as the `X-Timestamp` header or `ts` query parameter.
- **Separator**: Fields are joined using the byte value `\x01` (ASCII SOH).

---

## HMAC Calculation

- The HMAC is calculated using SHA256 and the shared secret key.
- The resulting HMAC is hex-encoded and provided as the `v3` query parameter.

---

## Example

Suppose:
- file_path: `foo/bar.jpg`
- content_length: `1048576`
- content_type: `image/jpeg`
- uploader: `alice@example.org`
- timestamp: `1717689600`

The signed string is:

```
foo/bar.jpg\x011048576\x01image/jpeg\x01alice@example.org\x011717689600
```

The HMAC is:

```
hmac_sha256(signed_string, secret)
```

---

## Timestamp Window

The server checks that the provided timestamp is within ±5 minutes (300 seconds) of the current server time. Requests outside this window are rejected to prevent replay attacks.

---

## Request Example

```
PUT /upload/foo/bar.jpg?v3=...&uploader=alice@example.org&ts=1717689600
Content-Length: 1048576
Content-Type: image/jpeg
X-Uploader: alice@example.org
X-Timestamp: 1717689600
```

- The server will use the highest version parameter present (`v3` > `v2` > `v`).
- The uploader and timestamp can be provided as headers or query parameters.

---

## Backward Compatibility

- v1 and v2 clients are still supported.
- The server always uses the highest version present in the request.
- No core functionality is lost for older clients.

---

## Security Notes

- Always use HTTPS for uploads and downloads.
- The secret key must be kept private and strong.
- The timestamp window helps prevent replay attacks.
- The uploader field enables per-user quotas and auditing.

---

## See Also
- [Prosody mod_http_upload_external](https://modules.prosody.im/mod_http_upload_external.html)
- [HMAC File Server Wiki](wiki.md)
