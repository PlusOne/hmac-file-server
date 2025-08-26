# Ejabberd HMAC File Server Integration Module Proposal

## Problem Analysis

### Current Issues
- **Authentication Complexity**: XMPP clients need manual HMAC secret configuration
- **Re-authentication Failures**: Clients lose connection during network switches  
- **Secret Management**: Shared secrets must be distributed to all clients
- **404 Upload Errors**: Direct HTTP upload authentication failures
- **Configuration Burden**: Each client needs individual HMAC setup

## Proposed Solution: `mod_http_upload_hmac`

### Architecture Overview
```
XMPP Client → Ejabberd → mod_http_upload_hmac → HMAC File Server
     ↓           ↓              ↓                    ↓
  XEP-0363    Auth Check    Generate Token       Store File
  Request     & Quotas      & Upload URL         & Validate
```

### Module Features

#### 1. Seamless Authentication
```erlang
% User authentication via existing XMPP session
authenticate_user(User, Server) ->
    case ejabberd_auth:check_password(User, Server, undefined) of
        true -> {ok, generate_upload_token(User, Server)};
        false -> {error, unauthorized}
    end.
```

#### 2. Dynamic Token Generation
```erlang
% Generate time-limited upload tokens
generate_upload_token(User, Filename, Size) ->
    Timestamp = unix_timestamp(),
    Payload = iolist_to_binary([User, $\0, Filename, $\0, integer_to_binary(Size)]),
    Token = crypto:mac(hmac, sha256, get_hmac_secret(), Payload),
    {ok, base64:encode(Token), Timestamp + 3600}. % 1 hour expiry
```

#### 3. XEP-0363 Response Generation
```erlang
% Generate XEP-0363 compliant slot response
generate_slot_response(User, Filename, Size, ContentType) ->
    {ok, Token, Expiry} = generate_upload_token(User, Filename, Size),
    UUID = uuid:generate(),
    PutURL = iolist_to_binary([get_upload_base_url(), "/", UUID, "/", Filename,
                               "?token=", Token, "&user=", User]),
    GetURL = iolist_to_binary([get_download_base_url(), "/", UUID, "/", Filename]),
    
    #xmlel{name = <<"slot">>,
           attrs = [{<<"xmlns">>, ?NS_HTTP_UPLOAD}],
           children = [
               #xmlel{name = <<"put">>,
                      attrs = [{<<"url">>, PutURL}],
                      children = [
                          #xmlel{name = <<"header">>,
                                 attrs = [{<<"name">>, <<"Authorization">>}],
                                 children = [{xmlcdata, <<"Bearer ", Token/binary>>}]}
                      ]},
               #xmlel{name = <<"get">>,
                      attrs = [{<<"url">>, GetURL}]}
           ]}.
```

## Integration Benefits

### For XMPP Clients
- ✅ **Zero Configuration**: No HMAC secrets needed
- ✅ **Automatic Authentication**: Uses existing XMPP session
- ✅ **Standard XEP-0363**: Full compliance with all clients
- ✅ **Error Reduction**: No more 404/authentication failures

### For Administrators  
- ✅ **Centralized Management**: All configuration in ejabberd
- ✅ **User Quotas**: Per-user upload limits
- ✅ **Audit Logging**: Complete upload tracking
- ✅ **Security**: Temporary tokens, no shared secrets

### For HMAC File Server
- ✅ **Token Validation**: Simple Bearer token authentication
- ✅ **User Context**: Know which XMPP user uploaded files
- ✅ **Quota Integration**: Enforce limits from ejabberd
- ✅ **Simplified Auth**: No complex HMAC verification needed

## Implementation Plan

### Phase 1: Core Module
```erlang
-module(mod_http_upload_hmac).
-behaviour(gen_mod).

-export([start/2, stop/1, process_iq/1, mod_options/1]).

% XEP-0363 IQ handler
process_iq(#iq{type = get, sub_el = #xmlel{name = <<"request">>}} = IQ) ->
    User = jid:user(IQ#iq.from),
    Server = jid:server(IQ#iq.from),
    
    % Extract file info from request
    {Filename, Size, ContentType} = extract_file_info(IQ#iq.sub_el),
    
    % Check quotas and permissions
    case check_upload_permission(User, Server, Size) of
        ok ->
            % Generate upload slot
            SlotResponse = generate_slot_response(User, Filename, Size, ContentType),
            IQ#iq{type = result, sub_el = SlotResponse};
        {error, Reason} ->
            IQ#iq{type = error, sub_el = generate_error(Reason)}
    end.
```

### Phase 2: HMAC Server Integration
```go
// Enhanced token validation in HMAC File Server
func validateBearerToken(token, user, filename string, size int64) error {
    // Verify token with ejabberd shared secret
    payload := fmt.Sprintf("%s\x00%s\x00%d", user, filename, size)
    expectedToken := generateHMAC(payload, ejabberdSecret)
    
    if !hmac.Equal([]byte(token), []byte(expectedToken)) {
        return errors.New("invalid token")
    }
    
    // Check token expiry and user permissions
    return validateTokenExpiry(token)
}
```

### Phase 3: Configuration Integration
```yaml
# ejabberd.yml
modules:
  mod_http_upload_hmac:
    hmac_server_url: "http://localhost:8080"
    hmac_shared_secret: "your-secure-secret"
    max_size: 104857600  # 100MB
    quota_per_user: 1073741824  # 1GB
    token_expiry: 3600  # 1 hour
    allowed_extensions: [".jpg", ".png", ".pdf", ".mp4"]
```

## Migration Path

### Current Setup → Module Integration
1. **Install Module**: Deploy `mod_http_upload_hmac` to ejabberd
2. **Configure Integration**: Set HMAC server URL and shared secret  
3. **Update HMAC Server**: Add Bearer token authentication support
4. **Test Integration**: Verify XMPP clients work seamlessly
5. **Migrate Users**: Remove client-side HMAC configuration

### Backward Compatibility
- ✅ **Dual Authentication**: Support both Bearer tokens and legacy HMAC
- ✅ **Gradual Migration**: Clients can migrate one by one
- ✅ **Fallback Support**: Legacy mode for non-integrated setups

## Technical Specifications

### Token Format
```
Bearer <base64(hmac-sha256(user + filename + size + timestamp, secret))>
```

### API Enhancement
```http
PUT /upload/uuid/filename.ext?token=bearer_token&user=username
Authorization: Bearer <token>
Content-Length: 12345

[file content]
```

### Response Format (Success)
```http
HTTP/1.1 201 Created
Content-Type: application/json

{
    "success": true,
    "filename": "filename.ext",
    "size": 12345,
    "user": "username@example.org",
    "uploaded_at": "2025-08-25T10:30:00Z"
}
```

## Development Priority

### High Priority Benefits
1. **Eliminate 404 Errors**: Solves current XMPP client issues
2. **Simplify Deployment**: No more client-side HMAC configuration
3. **Enhance Security**: Temporary tokens instead of shared secrets
4. **Improve UX**: Seamless file uploads for all XMPP clients

### Implementation Effort
- **Ejabberd Module**: ~2-3 days development
- **HMAC Server Updates**: ~1 day integration
- **Testing & Documentation**: ~1 day
- **Total**: ~1 week for complete solution

## Conclusion

An ejabberd module would **dramatically improve** the HMAC File Server ecosystem by:
- ✅ Eliminating authentication complexity
- ✅ Providing seamless XMPP integration  
- ✅ Solving current 404/re-auth issues
- ✅ Following XEP-0363 standards perfectly
- ✅ Enabling enterprise-grade user management

**This is definitely worth implementing!** It would make HMAC File Server the most user-friendly XEP-0363 solution available.

---
*HMAC File Server 3.2.2 + Ejabberd Integration Proposal*  
*Date: August 25, 2025*
