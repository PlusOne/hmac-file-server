#!/bin/bash
# Gitea Actions Runner Manager - Multi-Instance
# Fully automatic install/remove of multiple act_runner instances with systemd
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

RUNNER_BIN="/usr/local/bin/act_runner"
RUNNER_BASE="/var/lib/gitea/act_runner"
RUNNER_USER="act_runner"
RUNNER_VERSION="0.2.13"
GITEA_URL="https://git.uuxo.net"
RUNNER_LABELS="ubuntu-latest:docker://gitea/runner-images:ubuntu-latest,ubuntu-22.04:docker://gitea/runner-images:ubuntu-22.04,ubuntu-20.04:docker://gitea/runner-images:ubuntu-20.04"

info()    { echo -e "${GREEN}[✓]${NC} $1"; }
warn()    { echo -e "${YELLOW}[!]${NC} $1"; }
error()   { echo -e "${RED}[✗]${NC} $1"; }
header()  { echo -e "\n${BLUE}━━━ $1 ━━━${NC}\n"; }

check_root() {
    if [ "$(id -u)" -ne 0 ]; then
        error "This script must be run as root"
        exit 1
    fi
}

detect_arch() {
    case "$(uname -m)" in
        x86_64)  echo "amd64" ;;
        aarch64) echo "arm64" ;;
        armv7l)  echo "arm-7" ;;
        *)       error "Unsupported architecture: $(uname -m)"; exit 1 ;;
    esac
}

get_gitea_user() {
    local cfg
    for cfg in /etc/gitea/app.ini /var/lib/gitea/custom/conf/app.ini; do
        if [ -f "$cfg" ]; then
            local u
            u=$(grep -i "^RUN_USER" "$cfg" 2>/dev/null | cut -d'=' -f2 | tr -d ' ')
            if [ -n "$u" ]; then echo "$u"; return; fi
        fi
    done
    echo "git"
}

get_gitea_config() {
    for cfg in /etc/gitea/app.ini /var/lib/gitea/custom/conf/app.ini; do
        if [ -f "$cfg" ]; then echo "$cfg"; return; fi
    done
    echo ""
}

generate_token() {
    local gitea_bin gitea_config gitea_user token
    gitea_bin=$(command -v gitea 2>/dev/null || true)
    gitea_config=$(get_gitea_config)
    gitea_user=$(get_gitea_user)

    if [ -n "$gitea_bin" ] && [ -n "$gitea_config" ]; then
        token=$(su -c "${gitea_bin} --config ${gitea_config} actions generate-runner-token" "${gitea_user}" 2>/dev/null | tail -1)
        if [ -n "$token" ] && [ ${#token} -ge 20 ]; then
            echo "$token"
            return
        fi
    fi

    error "Cannot auto-generate token (Gitea CLI not found or failed)"
    echo "  Get one from: ${GITEA_URL}/-/admin/runners" >&2
    read -rp "Paste registration token: " token
    if [ -z "$token" ]; then
        error "No token provided"
        exit 1
    fi
    echo "$token"
}

# Get instance dir, service name, service file for a given instance number
instance_dir()     { echo "${RUNNER_BASE}/instance-${1}"; }
service_name()     { echo "act_runner_${1}"; }
service_file()     { echo "/etc/systemd/system/act_runner_${1}.service"; }

# Count existing runner instances
count_instances() {
    local count=0
    for d in "${RUNNER_BASE}"/instance-*/; do
        [ -d "$d" ] && ((count++)) || true
    done
    echo "$count"
}

# List existing instance numbers
list_instance_ids() {
    for d in "${RUNNER_BASE}"/instance-*/; do
        [ -d "$d" ] && basename "$d" | sed 's/instance-//'
    done 2>/dev/null
}

install_single() {
    local n="$1"
    local inst_dir svc_name svc_file runner_name
    inst_dir=$(instance_dir "$n")
    svc_name=$(service_name "$n")
    svc_file=$(service_file "$n")
    runner_name="$(hostname)-runner-${n}"

    # Already running?
    if systemctl is-active --quiet "${svc_name}" 2>/dev/null && [ -f "${inst_dir}/.runner" ]; then
        info "Runner #${n}: already running"
        return 0
    fi

    info "Setting up runner #${n}..."

    # Working dir + config
    mkdir -p "${inst_dir}"
    if [ ! -f "${inst_dir}/config.yaml" ]; then
        ${RUNNER_BIN} generate-config > "${inst_dir}/config.yaml"
    fi

    # Register
    if [ ! -f "${inst_dir}/.runner" ]; then
        local token
        token=$(generate_token)
        cd "${inst_dir}"
        ${RUNNER_BIN} register \
            --instance "${GITEA_URL}" \
            --token "${token}" \
            --name "${runner_name}" \
            --labels "${RUNNER_LABELS}" \
            --no-interactive
        info "Runner #${n}: registered as ${runner_name}"
    fi

    # Permissions
    chown -R "${RUNNER_USER}:${RUNNER_USER}" "${inst_dir}"

    # Systemd service
    cat > "${svc_file}" << SVCEOF
[Unit]
Description=Gitea Actions Runner #${n}
Documentation=https://docs.gitea.com/usage/actions/act-runner
After=docker.service network-online.target
Requires=docker.service
Wants=network-online.target

[Service]
Type=simple
User=${RUNNER_USER}
Group=${RUNNER_USER}
WorkingDirectory=${inst_dir}
ExecStart=${RUNNER_BIN} daemon --config ${inst_dir}/config.yaml
Environment=HOME=${inst_dir}
Restart=always
RestartSec=10
NoNewPrivileges=false
StandardOutput=journal
StandardError=journal
SyslogIdentifier=act_runner_${n}

[Install]
WantedBy=multi-user.target
SVCEOF

    systemctl daemon-reload
    systemctl enable --quiet "${svc_name}"
    systemctl start "${svc_name}"
    sleep 1

    if systemctl is-active --quiet "${svc_name}"; then
        info "Runner #${n}: active"
    else
        error "Runner #${n}: failed to start. Check: journalctl -u ${svc_name} -n 50"
    fi
}

install_runners() {
    local count="${1:-1}"
    header "Install ${count} Gitea Actions Runner(s)"

    # Docker check
    if ! command -v docker &>/dev/null; then
        error "Docker not installed. Run: curl -fsSL https://get.docker.com | sh"
        exit 1
    fi
    systemctl is-active --quiet docker || systemctl start docker

    # Binary
    if [ -x "${RUNNER_BIN}" ]; then
        info "Binary: $(${RUNNER_BIN} --version 2>/dev/null || echo "${RUNNER_BIN}")"
    else
        local arch
        arch=$(detect_arch)
        info "Downloading act_runner v${RUNNER_VERSION} (${arch})..."
        local url="https://gitea.com/gitea/act_runner/releases/download/v${RUNNER_VERSION}/act_runner-${RUNNER_VERSION}-linux-${arch}"
        curl -fsSL -o "${RUNNER_BIN}" "${url}" || { error "Download failed"; exit 1; }
        chmod +x "${RUNNER_BIN}"
        info "Installed: $(${RUNNER_BIN} --version 2>/dev/null)"
    fi

    # System user
    if ! id "${RUNNER_USER}" &>/dev/null; then
        useradd -r -s /bin/false -m -d "${RUNNER_BASE}" "${RUNNER_USER}"
        info "Created user: ${RUNNER_USER}"
    fi
    usermod -aG docker "${RUNNER_USER}" 2>/dev/null || true
    mkdir -p "${RUNNER_BASE}"

    # Migrate legacy single-instance if exists
    if [ -f "${RUNNER_BASE}/.runner" ] && [ ! -d "${RUNNER_BASE}/instance-1" ]; then
        info "Migrating existing runner to instance-1..."
        mkdir -p "${RUNNER_BASE}/instance-1"
        for f in .runner config.yaml; do
            [ -f "${RUNNER_BASE}/${f}" ] && mv "${RUNNER_BASE}/${f}" "${RUNNER_BASE}/instance-1/"
        done
        # Migrate old service
        if [ -f "/etc/systemd/system/act_runner.service" ]; then
            systemctl stop act_runner 2>/dev/null || true
            systemctl disable --quiet act_runner 2>/dev/null || true
            rm -f /etc/systemd/system/act_runner.service
        fi
        chown -R "${RUNNER_USER}:${RUNNER_USER}" "${RUNNER_BASE}/instance-1"
    fi

    # Install each instance
    for n in $(seq 1 "$count"); do
        install_single "$n"
    done

    header "Done — $(count_instances) runner(s) active"
    echo "  Status:  $0 status"
    echo "  Logs:    $0 logs <N>"
    echo "  Remove:  $0 remove [N|all]"
}

remove_single() {
    local n="$1"
    local inst_dir svc_name svc_file
    inst_dir=$(instance_dir "$n")
    svc_name=$(service_name "$n")
    svc_file=$(service_file "$n")

    if [ ! -d "${inst_dir}" ] && [ ! -f "${svc_file}" ]; then
        warn "Runner #${n}: not found"
        return
    fi

    systemctl stop "${svc_name}" 2>/dev/null || true
    systemctl disable --quiet "${svc_name}" 2>/dev/null || true
    [ -f "${svc_file}" ] && rm -f "${svc_file}"
    [ -d "${inst_dir}" ] && rm -rf "${inst_dir}"
    info "Runner #${n}: removed"
}

remove_runners() {
    local target="${1:-all}"
    header "Removing Runner(s)"

    if [ "$target" = "all" ]; then
        # Remove all instances
        for n in $(list_instance_ids); do
            remove_single "$n"
        done
        # Also remove legacy single-instance service
        if [ -f "/etc/systemd/system/act_runner.service" ]; then
            systemctl stop act_runner 2>/dev/null || true
            systemctl disable --quiet act_runner 2>/dev/null || true
            rm -f /etc/systemd/system/act_runner.service
            info "Legacy service removed"
        fi
        systemctl daemon-reload
        # Clean up shared resources
        [ -f "${RUNNER_BIN}" ] && rm -f "${RUNNER_BIN}" && info "Binary removed"
        [ -d "${RUNNER_BASE}" ] && rm -rf "${RUNNER_BASE}" && info "Data removed"
        id "${RUNNER_USER}" &>/dev/null && userdel "${RUNNER_USER}" 2>/dev/null && info "User removed" || true
    else
        remove_single "$target"
        systemctl daemon-reload
    fi

    header "Done"
}

show_status() {
    header "Runner Status"
    local found=0

    # Check legacy service
    if systemctl is-active --quiet "act_runner" 2>/dev/null; then
        warn "Legacy service (act_runner) still running — consider: $0 install <count>"
        systemctl status act_runner --no-pager 2>&1 | head -5
        echo ""
        found=1
    fi

    for n in $(list_instance_ids); do
        local svc_name
        svc_name=$(service_name "$n")
        if systemctl is-active --quiet "${svc_name}" 2>/dev/null; then
            info "Runner #${n}: active"
        else
            error "Runner #${n}: inactive"
        fi
        found=1
    done

    if [ "$found" -eq 0 ]; then
        error "No runners installed"
    fi
}

# Main
check_root
case "${1:-}" in
    install)
        install_runners "${2:-1}"
        ;;
    remove)
        remove_runners "${2:-all}"
        ;;
    status)
        show_status
        ;;
    restart)
        target="${2:-all}"
        if [ "$target" = "all" ]; then
            for n in $(list_instance_ids); do
                systemctl restart "$(service_name "$n")" && info "Runner #${n}: restarted"
            done
        else
            systemctl restart "$(service_name "$target")" && info "Runner #${target}: restarted"
        fi
        ;;
    logs)
        n="${2:-1}"
        journalctl -u "$(service_name "$n")" -f
        ;;
    *)
        echo "Usage: $0 {install|remove|status|restart|logs} [count|N|all]"
        echo ""
        echo "  install [N]     Install N runners (default: 1). Skips existing."
        echo "  remove [N|all]  Remove runner #N or all (default: all)"
        echo "  status          Show status of all runners"
        echo "  restart [N|all] Restart runner #N or all (default: all)"
        echo "  logs [N]        Follow logs of runner #N (default: 1)"
        echo ""
        echo "Examples:"
        echo "  $0 install 3    Install 3 parallel runners"
        echo "  $0 status       Show all runner statuses"
        echo "  $0 remove 2     Remove only runner #2"
        echo "  $0 remove       Remove everything"
        ;;
esac
