# 🚀 Professional Emacs Configuration Makefile
# Make managing your Emacs configuration easy and professional

.PHONY: help install clean backup update test status

# Default target
help:
	@echo "🚀 Professional Emacs Configuration Management"
	@echo "=============================================="
	@echo ""
	@echo "Available commands:"
	@echo "  install    - Install/update the configuration"
	@echo "  clean      - Clean temporary files and caches"
	@echo "  backup     - Create backup of current configuration"
	@echo "  update     - Update from git repository"
	@echo "  test       - Test configuration loading"
	@echo "  status     - Show configuration status"
	@echo "  help       - Show this help message"
	@echo ""

# Install/update configuration
install:
	@echo "🚀 Installing Professional Emacs Configuration..."
	@if [ -d "~/.emacs.d" ]; then \
		echo "📁 Backing up existing configuration..."; \
		mv ~/.emacs.d ~/.emacs.d.backup.$$(date +%Y%m%d_%H%M%S); \
	fi
	@echo "📥 Cloning configuration..."
	@git clone https://github.com/krispi/emacs-config.git ~/.emacs.d
	@echo "✅ Installation completed successfully!"
	@echo "🎯 Start Emacs to begin using your new configuration!"

# Clean temporary files and caches
clean:
	@echo "🧹 Cleaning temporary files and caches..."
	@rm -rf ~/.emacs.d/elpaca/builds/*
	@rm -rf ~/.emacs.d/elpaca/queues/*
	@rm -rf ~/.emacs.d/elpaca/repos/*
	@echo "✅ Cleanup completed!"

# Create backup of current configuration
backup:
	@echo "💾 Creating backup of current configuration..."
	@if [ -d "~/.emacs.d" ]; then \
		cp -r ~/.emacs.d ~/.emacs.d.backup.$$(date +%Y%m%d_%H%M%S); \
		echo "✅ Backup created successfully!"; \
	else \
		echo "❌ No configuration found to backup"; \
	fi

# Update from git repository
update:
	@echo "🔄 Updating configuration from repository..."
	@if [ -d "~/.emacs.d" ]; then \
		cd ~/.emacs.d && git pull origin main; \
		echo "✅ Update completed successfully!"; \
	else \
		echo "❌ No configuration found to update"; \
	fi

# Test configuration loading
test:
	@echo "🧪 Testing configuration loading..."
	@emacs --batch --eval "(progn (load-file \"init.el\") (message \"Configuration test completed successfully!\"))"
	@echo "✅ Configuration test completed!"

# Show configuration status
status:
	@echo "📊 Configuration Status"
	@echo "======================"
	@if [ -d "~/.emacs.d" ]; then \
		echo "✅ Configuration directory: ~/.emacs.d"; \
		if [ -f "~/.emacs.d/init.el" ]; then \
			echo "✅ Main configuration: init.el"; \
		else \
			echo "❌ Main configuration: Missing"; \
		fi; \
		if [ -d "~/.emacs.d/elpaca" ]; then \
			echo "✅ Package manager: Elpaca"; \
		else \
			echo "❌ Package manager: Not installed"; \
		fi; \
	else \
		echo "❌ Configuration directory: Not found"; \
	fi
