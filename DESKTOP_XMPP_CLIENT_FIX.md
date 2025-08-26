# 🖥️ DESKTOP XMPP CLIENT UPLOAD FIX - Dino & Gajim After Restart

## 🎯 Problem Analysis

**Issue:** Dino and Gajim can't upload after restart, Android works after reconnection

**Root Cause:** Desktop XMPP clients restore cached sessions with expired tokens, while mobile clients get fresh authentication.

---

## ⚡ IMMEDIATE FIX (Try This First!)

### Step 1: Clear Client Caches
```bash
# Stop XMPP clients completely
pkill -f dino
pkill -f gajim
sleep 5

# Backup existing data (optional)
cp -r ~/.local/share/dino ~/.local/share/dino.backup 2>/dev/null || true
cp -r ~/.local/share/gajim ~/.local/share/gajim.backup 2>/dev/null || true

# Clear caches that may contain expired tokens
rm -rf ~/.cache/dino/ 2>/dev/null || true
rm -rf ~/.cache/gajim/ 2>/dev/null || true

# Clear specific upload-related cached files
find ~/.local/share/dino -name '*upload*' -delete 2>/dev/null || true
find ~/.local/share/gajim -name '*upload*' -delete 2>/dev/null || true
find ~/.local/share/dino -name '*token*' -delete 2>/dev/null || true
find ~/.local/share/gajim -name '*token*' -delete 2>/dev/null || true

# Restart clients
dino &
gajim &
```

### Step 2: Test Upload
- Try uploading a small file in both Dino and Gajim
- Should work now with fresh authentication

---

## 🔧 ENHANCED SERVER SOLUTION

If the cache clearing doesn't work, deploy the enhanced server:

### Deploy Enhanced Server
```bash
cd /root/hmac-file-server

# Use the enhanced server binary
./hmac-file-server-desktop-fixed -config config-mobile-resilient.toml
```

### What the Enhanced Server Fixes:
- **24-hour grace period** specifically for desktop XMPP clients (Dino, Gajim)
- **48-hour session restoration** window for cached tokens after restart
- **Enhanced detection** of desktop vs mobile XMPP clients
- **Special logging** for desktop client authentication issues

---

## 📊 Technical Details

### Enhanced Client Detection:
```
Desktop XMPP Clients: 24-hour grace period (session restoration)
Mobile XMPP Clients: 12-hour grace period (network switching)
Network Resilience: 72-hour ultra-grace period (critical scenarios)
```

### Log Messages to Watch For:
```
🖥️  Desktop XMPP client detected (Dino/Gajim), using 24-hour grace period
🖥️  DESKTOP SESSION RESTORE: allowing within 48-hour restoration window
```

---

## 🌐 Network Configuration Check

Your setup: **Notebook (WLAN + Ethernet) → Router → HMAC File Server**

### Potential Network Issues:
1. **Multiple interfaces** may cause IP confusion
2. **Router NAT** may assign different IPs after restart
3. **Cached connections** may use old IP addresses

### Check Network Configuration:
```bash
# Check active network interfaces
ip addr show | grep -E "(wlan|eth|eno|wlp)" -A2

# Check default routes
ip route show | grep default

# Check if multiple interfaces have IPs
ifconfig | grep "inet " | grep -v "127.0.0.1"
```

---

## 🚨 Troubleshooting Steps

### If Upload Still Fails:

1. **Check Server Logs:**
```bash
tail -f /var/log/hmac-file-server-mobile.log | grep -E "(Desktop|XMPP|token|auth)"
```

2. **Check Client User-Agent:**
- Look for log entries showing how clients identify themselves
- Ensure Dino/Gajim are detected as desktop XMPP clients

3. **Verify Token Generation:**
- Check if clients are getting fresh tokens after restart
- Look for "expired beyond grace period" messages

4. **Network Debugging:**
```bash
# Check if clients can reach server
curl -I http://localhost:8080/health

# Check if router/NAT is affecting connections
netstat -tuln | grep 8080
```

---

## 💡 Why This Happens

### Desktop vs Mobile Behavior:
- **Desktop clients (Dino/Gajim):** Save session state to disk, restore after restart
- **Mobile clients:** Reconnect fresh, get new authentication tokens
- **Server:** Original grace periods not sufficient for cached/restored sessions

### Network Complexity:
- **WLAN + Ethernet:** Multiple network paths can confuse client IP detection
- **Router NAT:** May assign different internal IPs after restart
- **Cached connections:** Old network state restored with expired tokens

---

## ✅ Expected Results

After applying the fix:
- ✅ **Dino uploads work** immediately after restart
- ✅ **Gajim uploads work** immediately after restart  
- ✅ **Android continues working** after disconnect/reconnect
- ✅ **Network switching** (WLAN ↔ Ethernet) handled gracefully
- ✅ **Router IP changes** don't break authentication

---

## 🎯 Summary

**Root Cause:** Desktop XMPP clients restore expired cached sessions  
**Quick Fix:** Clear client caches to force fresh authentication  
**Long-term Fix:** Enhanced server with 48-hour desktop session restoration  
**Network:** Router setup is fine, issue is client-side session caching  

The enhanced server now treats desktop XMPP clients with the same network resilience as mobile clients, plus special handling for session restoration scenarios.
