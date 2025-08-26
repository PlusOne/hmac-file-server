# Ejabberd HMAC File Server Integration Module
# Makefile for compilation, installation, and testing

# Configuration
ERLC = erlc
MODULE_NAME = mod_http_upload_hmac
MODULE_SRC = $(MODULE_NAME).erl
MODULE_BEAM = $(MODULE_NAME).beam

# Default ejabberd paths (auto-detected during install)
EJABBERD_INCLUDE_DIR = /opt/ejabberd/lib/ejabberd-*/include
EJABBERD_MODULES_DIR = /opt/ejabberd/lib/ejabberd-*/ebin

# Compilation flags
ERLC_FLAGS = -I $(EJABBERD_INCLUDE_DIR) -W -v

# Colors for output
GREEN = \033[0;32m
YELLOW = \033[1;33m
RED = \033[0;31m
NC = \033[0m # No Color

.PHONY: all compile install clean test help

# Default target
all: compile

# Compile the module
compile: $(MODULE_BEAM)

$(MODULE_BEAM): $(MODULE_SRC)
	@echo -e "$(GREEN)Compiling $(MODULE_SRC)...$(NC)"
	$(ERLC) $(ERLC_FLAGS) -o . $(MODULE_SRC)
	@echo -e "$(GREEN)✓ Compilation successful$(NC)"

# Install module to ejabberd
install: compile
	@echo -e "$(YELLOW)Installing module to ejabberd...$(NC)"
	@if [ ! -d "$(shell echo $(EJABBERD_MODULES_DIR))" ]; then \
		echo -e "$(RED)Error: ejabberd modules directory not found$(NC)"; \
		echo -e "$(YELLOW)Run: make detect-paths$(NC)"; \
		exit 1; \
	fi
	sudo cp $(MODULE_BEAM) $(shell echo $(EJABBERD_MODULES_DIR))/
	sudo chown ejabberd:ejabberd $(shell echo $(EJABBERD_MODULES_DIR))/$(MODULE_BEAM)
	sudo chmod 644 $(shell echo $(EJABBERD_MODULES_DIR))/$(MODULE_BEAM)
	@echo -e "$(GREEN)✓ Module installed$(NC)"

# Auto-install with script
auto-install:
	@echo -e "$(GREEN)Running automatic installation...$(NC)"
	./install.sh

# Detect ejabberd paths
detect-paths:
	@echo -e "$(YELLOW)Detecting ejabberd installation paths...$(NC)"
	@echo "Include directories:"
	@find /opt /usr -name "ejabberd.hrl" -type f 2>/dev/null | head -5 | sed 's/ejabberd.hrl//' || echo "  None found"
	@echo "Module directories:"
	@find /opt /usr -name "ebin" -path "*/ejabberd*" -type d 2>/dev/null | head -5 || echo "  None found"

# Test the installation
test: 
	@echo -e "$(GREEN)Running integration tests...$(NC)"
	./test.sh all

# Test specific components
test-token:
	./test.sh token

test-health:
	./test.sh health

test-upload:
	./test.sh upload

test-ejabberd:
	./test.sh ejabberd

# Clean compiled files
clean:
	@echo -e "$(YELLOW)Cleaning compiled files...$(NC)"
	rm -f *.beam
	@echo -e "$(GREEN)✓ Clean complete$(NC)"

# Uninstall module from ejabberd
uninstall:
	@echo -e "$(YELLOW)Removing module from ejabberd...$(NC)"
	sudo rm -f $(shell echo $(EJABBERD_MODULES_DIR))/$(MODULE_BEAM)
	@echo -e "$(GREEN)✓ Module removed$(NC)"

# Check ejabberd status
status:
	@echo -e "$(GREEN)Checking ejabberd status...$(NC)"
	@if command -v ejabberdctl >/dev/null 2>&1; then \
		ejabberdctl status || echo -e "$(RED)ejabberd is not running$(NC)"; \
		echo; \
		echo "Loaded modules:"; \
		ejabberdctl modules | grep -E "(http_upload|mod_http)" || echo "  No HTTP upload modules found"; \
	else \
		echo -e "$(RED)ejabberdctl not found$(NC)"; \
	fi

# Check HMAC server status  
hmac-status:
	@echo -e "$(GREEN)Checking HMAC File Server status...$(NC)"
	@if systemctl is-active hmac-file-server >/dev/null 2>&1; then \
		echo -e "$(GREEN)✓ HMAC File Server is running$(NC)"; \
		curl -s http://localhost:8080/health && echo || echo -e "$(RED)Health check failed$(NC)"; \
	else \
		echo -e "$(RED)✗ HMAC File Server is not running$(NC)"; \
	fi

# Development: watch for changes and recompile
watch:
	@echo -e "$(YELLOW)Watching for changes (Ctrl+C to stop)...$(NC)"
	@while true; do \
		if [ $(MODULE_SRC) -nt $(MODULE_BEAM) ]; then \
			echo -e "$(GREEN)Source changed, recompiling...$(NC)"; \
			make compile; \
		fi; \
		sleep 2; \
	done

# Generate example configuration
config:
	@echo -e "$(GREEN)Generating example configuration...$(NC)"
	@cat << 'EOF'
# Add to ejabberd.yml modules section:

modules:
  mod_http_upload_hmac:
    hmac_server_url: "http://localhost:8080"
    hmac_shared_secret: "your-secure-secret-here"
    max_size: 104857600  # 100MB
    quota_per_user: 1073741824  # 1GB  
    token_expiry: 3600  # 1 hour
    allowed_extensions: 
      - ".jpg"
      - ".png"
      - ".pdf"
      - ".mp4"
      - ".mp3"
    iqdisc: one_queue

# Comment out existing mod_http_upload:
# mod_http_upload: []
EOF

# Show logs
logs:
	@echo -e "$(GREEN)Showing recent ejabberd logs...$(NC)"
	journalctl -u ejabberd --no-pager -n 50

logs-follow:
	@echo -e "$(GREEN)Following ejabberd logs (Ctrl+C to stop)...$(NC)"
	journalctl -u ejabberd -f

# Restart services
restart:
	@echo -e "$(YELLOW)Restarting ejabberd...$(NC)"
	sudo systemctl restart ejabberd
	@echo -e "$(YELLOW)Restarting HMAC File Server...$(NC)"
	sudo systemctl restart hmac-file-server
	@echo -e "$(GREEN)✓ Services restarted$(NC)"

# Development setup
dev-setup:
	@echo -e "$(GREEN)Setting up development environment...$(NC)"
	make detect-paths
	make compile
	@echo -e "$(GREEN)✓ Development setup complete$(NC)"
	@echo -e "$(YELLOW)Next steps:$(NC)"
	@echo "1. Configure ejabberd.yml (make config)"
	@echo "2. Install module (make install)"
	@echo "3. Restart services (make restart)"
	@echo "4. Test integration (make test)"

# Show help
help:
	@echo -e "$(GREEN)HMAC File Server - Ejabberd Module Makefile$(NC)"
	@echo
	@echo -e "$(YELLOW)Build Commands:$(NC)"
	@echo "  make compile      - Compile the module"
	@echo "  make install      - Install module to ejabberd"
	@echo "  make auto-install - Run full installation script"
	@echo "  make clean        - Remove compiled files"
	@echo "  make uninstall    - Remove module from ejabberd"
	@echo
	@echo -e "$(YELLOW)Testing Commands:$(NC)"
	@echo "  make test         - Run all integration tests"
	@echo "  make test-token   - Test Bearer token generation"
	@echo "  make test-health  - Test HMAC server health"
	@echo "  make test-upload  - Test file upload with Bearer token"
	@echo "  make test-ejabberd- Test ejabberd module status"
	@echo
	@echo -e "$(YELLOW)Utility Commands:$(NC)"
	@echo "  make status       - Check ejabberd status"
	@echo "  make hmac-status  - Check HMAC server status"
	@echo "  make logs         - Show recent ejabberd logs"
	@echo "  make logs-follow  - Follow ejabberd logs"
	@echo "  make restart      - Restart both services"
	@echo "  make config       - Generate example configuration"
	@echo
	@echo -e "$(YELLOW)Development Commands:$(NC)"
	@echo "  make dev-setup    - Setup development environment"
	@echo "  make detect-paths - Find ejabberd installation paths"
	@echo "  make watch        - Auto-recompile on changes"
	@echo
	@echo -e "$(YELLOW)Variables:$(NC)"
	@echo "  ERLC=$(ERLC)"
	@echo "  EJABBERD_INCLUDE_DIR=$(EJABBERD_INCLUDE_DIR)"
	@echo "  EJABBERD_MODULES_DIR=$(EJABBERD_MODULES_DIR)"

# Default help when no target
.DEFAULT_GOAL := help
