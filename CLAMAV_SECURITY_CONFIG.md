# ClamAV Security Configuration Template

## 🔒 **Critical Security File Extensions**

These are the file types that should ALWAYS be scanned by ClamAV as they can contain malicious code:

### **Executable Files (HIGH RISK)**
```toml
# Windows executables
".exe", ".com", ".bat", ".cmd", ".scr", ".pif", ".dll", ".sys"

# Unix/Linux executables  
".sh", ".bash", ".csh", ".ksh", ".zsh", ".bin", ".run", ".deb", ".rpm"

# Cross-platform
".jar", ".app", ".dmg", ".pkg"
```

### **Script Files (HIGH RISK)**
```toml
# Web scripts
".php", ".asp", ".aspx", ".jsp", ".cgi", ".pl", ".py", ".rb"

# Office macros
".docm", ".xlsm", ".pptm", ".dotm", ".xltm", ".potm"

# JavaScript/VBScript
".js", ".vbs", ".vbe", ".wsf", ".wsh"
```

### **Archive Files (MEDIUM RISK)**
```toml
# Compressed archives (can contain executables)
".zip", ".rar", ".7z", ".tar", ".gz", ".bz2", ".xz", ".tgz", ".tar.gz"

# Installer packages
".msi", ".cab", ".iso"
```

### **Document Files (LOW-MEDIUM RISK)**
```toml
# Only if they support macros or embedding
".doc", ".docx", ".xls", ".xlsx", ".ppt", ".pptx", ".pdf"
```

## 🎯 **Recommended ClamAV Configuration**

### **High Security (Paranoid)**
```toml
[clamav]
clamavenabled = true
maxscansize = "100MB"  # Smaller limit for faster processing
scanfileextensions = [
    # Executables
    ".exe", ".com", ".bat", ".cmd", ".scr", ".pif", ".dll", ".sys",
    ".sh", ".bash", ".bin", ".run", ".deb", ".rpm", ".jar", ".app",
    
    # Scripts  
    ".php", ".asp", ".aspx", ".jsp", ".cgi", ".pl", ".py", ".rb",
    ".js", ".vbs", ".vbe", ".wsf", ".wsh",
    
    # Macro documents
    ".docm", ".xlsm", ".pptm", ".dotm", ".xltm", ".potm",
    
    # Archives
    ".zip", ".rar", ".7z", ".tar", ".gz", ".bz2", ".tgz", ".msi", ".iso"
]
```

### **Balanced Security (Recommended)**
```toml
[clamav]
clamavenabled = true
maxscansize = "200MB"  # Current setting
scanfileextensions = [
    # Critical executables only
    ".exe", ".com", ".bat", ".cmd", ".scr", ".dll",
    ".sh", ".bash", ".bin", ".jar",
    
    # High-risk scripts
    ".php", ".asp", ".jsp", ".js", ".vbs",
    
    # Macro documents
    ".docm", ".xlsm", ".pptm",
    
    # Compressed files
    ".zip", ".rar", ".7z", ".tar.gz", ".msi"
]
```

### **Performance Optimized (Fast)**
```toml
[clamav]
clamavenabled = true
maxscansize = "50MB"  # Smaller files only
scanfileextensions = [
    # Only the most dangerous
    ".exe", ".com", ".bat", ".scr", ".dll",
    ".sh", ".bin", ".jar", ".php", ".js", ".zip"
]
```

## 🚫 **Files That Should NEVER Be Scanned**

These file types are safe and scanning them wastes resources:

```toml
# Media files (completely safe)
".mp4", ".avi", ".mov", ".mkv", ".wmv", ".flv", ".webm",
".mp3", ".wav", ".flac", ".aac", ".ogg", ".m4a",
".jpg", ".jpeg", ".png", ".gif", ".bmp", ".tiff", ".svg", ".webp",

# Text/Data files (safe)
".txt", ".log", ".csv", ".json", ".xml", ".yaml", ".yml",

# Large data files (safe, would be slow to scan)
".sql", ".dump", ".backup", ".tar.xz", ".img", ".vmdk"
```

## ⚡ **Performance Impact Analysis**

| File Type | Size | Scan Time | Security Risk | Recommendation |
|-----------|------|-----------|---------------|----------------|
| `.exe` | 10MB | 2-5s | ⚠️ HIGH | Always scan |
| `.zip` | 50MB | 10-30s | ⚠️ MEDIUM | Scan if <200MB |
| `.mp4` | 1GB | 5+ minutes | ✅ NONE | Never scan |
| `.pdf` | 5MB | 1-3s | ⚠️ LOW | Optional |

## 🔧 **Implementation for Production**

Update `/etc/hmac-file-server/config.toml`:

```toml
[clamav]
clamavenabled = true  # Enable for security
maxscansize = "200MB"  # Skip very large files
numscanworkers = 2
clamavsocket = "/var/run/clamav/clamd.ctl"

# CRITICAL SECURITY FILES ONLY
scanfileextensions = [
    # Windows executables
    ".exe", ".com", ".bat", ".cmd", ".scr", ".dll",
    
    # Unix executables
    ".sh", ".bash", ".bin", ".jar",
    
    # Dangerous scripts
    ".php", ".asp", ".jsp", ".js", ".vbs",
    
    # Macro-enabled documents
    ".docm", ".xlsm", ".pptm",
    
    # Compressed archives (can hide malware)
    ".zip", ".rar", ".7z", ".tar.gz", ".msi"
]
```

This configuration:
- ✅ **Protects against malware** in dangerous file types
- ✅ **Skips harmless media files** entirely
- ✅ **Fast processing** for large uploads
- ✅ **Configurable** via standard config file
