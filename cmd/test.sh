#!/bin/bash

# Configuration
SERVER_URL="http://[::1]:8080"  # Replace with your actual server URL
SECRET="a-orc-and-a-humans-is-drinking-ale"  # Replace with your HMAC secret key
UPLOAD_PATH="hmac_icon.png"  # Test file to upload
PROTOCOL_TYPE="v2"  # Use v2, v, or token as needed

# Generate HMAC based on protocol type
generate_hmac() {
    local file_path="$1"
    local content_length="$2"
    local protocol="$3"
    local mac_string=""

    if [[ "$protocol" == "v" ]]; then
        mac_string=$(printf "%s %s" "$file_path" "$content_length" | openssl dgst -sha256 -hmac "$SECRET" | awk '{print $2}')
    elif [[ "$protocol" == "v2" || "$protocol" == "token" ]]; then
        local content_type
        content_type=$(file --mime-type -b "$file_path")
        if [[ -z "$content_type" ]]; then
            content_type="application/octet-stream"
        fi
        mac_string=$(printf "%s\0%s\0%s" "$file_path" "$content_length" "$content_type" | openssl dgst -sha256 -hmac "$SECRET" | awk '{print $2}')
    fi

    echo "$mac_string"
}

# Perform upload
perform_upload() {
    local file_path="$1"
    local protocol="$2"

    if [[ ! -f "$file_path" ]]; then
        echo "File not found: $file_path"
        exit 1
    fi

    local content_length
    content_length=$(stat -c%s "$file_path")

    local hmac_value
    hmac_value=$(generate_hmac "$file_path" "$content_length" "$protocol")

    local file_name
    file_name=$(basename "$file_path")

    local request_url
    request_url="${SERVER_URL}/${file_name}?${protocol}=${hmac_value}"

    echo "Uploading to: $request_url"

    curl -X PUT \
        -H "Content-Type: application/octet-stream" \
        -H "Content-Length: $content_length" \
        --data-binary @"$file_path" \
        "$request_url"
}

# Execute the script
perform_upload "$UPLOAD_PATH" "$PROTOCOL_TYPE"
