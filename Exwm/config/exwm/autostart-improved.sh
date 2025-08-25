#!/bin/bash
# =============================================================================
# EXWM Autostart Script - Emacs AI 2.0 + EXWM
# =============================================================================
# 
# This script handles automatic startup of essential system services and
# applications when EXWM initializes.
#
# Author: Krispi
# Version: 2.0
# License: GPL-3.0
# =============================================================================

# =============================================================================
# CONFIGURATION
# =============================================================================

# Script configuration
SCRIPT_DIR="$HOME/.config/exwm"
LOG_FILE="$SCRIPT_DIR/autostart.log"
CONFIG_DIR="$HOME/.config"

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Check if a program is installed
is_installed() {
    command -v "$1" &>/dev/null
}

# Run program only if installed
run_if_installed() {
    if is_installed "$1"; then
        "$@" &
        log_event "Started: $1"
        return 0
    else
        log_event "Not installed: $1"
        return 1
    fi
}

# Log events with timestamp
log_event() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "$LOG_FILE"
}

# Kill existing processes
kill_existing() {
    local process_name="$1"
    if pgrep -x "$process_name" >/dev/null; then
        pkill -x "$process_name"
        sleep 1
        log_event "Killed existing: $process_name"
    fi
}

# Wait for process to start
wait_for_process() {
    local process_name="$1"
    local max_wait=10
    local count=0
    
    while ! pgrep -x "$process_name" >/dev/null && [ $count -lt $max_wait ]; do
        sleep 0.5
        ((count++))
    done
    
    if pgrep -x "$process_name" >/dev/null; then
        log_event "Confirmed running: $process_name"
        return 0
    else
        log_event "Failed to start: $process_name"
        return 1
    fi
}

# =============================================================================
# SCRIPT INITIALIZATION
# =============================================================================

# Create log directory if it doesn't exist
mkdir -p "$(dirname "$LOG_FILE")"

# Start logging
log_event "=== EXWM Autostart Script Started ==="
log_event "User: $USER"
log_event "Home: $HOME"
log_event "Display: $DISPLAY"

# =============================================================================
# DESKTOP BACKGROUND
# =============================================================================

log_event "--- Setting up desktop background ---"

# Set desktop background
if is_installed nitrogen; then
    nitrogen --restore &
    log_event "Started nitrogen for desktop background"
elif is_installed feh; then
    feh --bg-scale ~/.config/wallpaper.jpg &
    log_event "Started feh for desktop background"
elif is_installed hsetroot; then
    hsetroot -solid "#2E3440" &
    log_event "Set solid background color"
fi

# =============================================================================
# COMPOSITOR (VISUAL EFFECTS)
# =============================================================================

log_event "--- Starting compositor ---"

# Kill existing compositor processes
kill_existing picom
kill_existing compton

# Start compositor with configuration
if is_installed picom; then
    if [ -f "$CONFIG_DIR/picom/picom.conf" ]; then
        picom -b --config "$CONFIG_DIR/picom/picom.conf" &
        log_event "Started picom with custom config"
    else
        picom -b &
        log_event "Started picom with default config"
    fi
    wait_for_process picom
elif is_installed compton; then
    compton -b &
    log_event "Started compton"
    wait_for_process compton
fi

# =============================================================================
# NOTIFICATION SYSTEM
# =============================================================================

log_event "--- Starting notification system ---"

# Kill existing notification daemon
kill_existing dunst

# Start notification daemon
if is_installed dunst; then
    dunst &
    log_event "Started dunst notification daemon"
    wait_for_process dunst
fi

# =============================================================================
# SYSTEM APPLETS
# =============================================================================

log_event "--- Starting system applets ---"

# Network management
if is_installed nm-applet; then
    nm-applet &
    log_event "Started network manager applet"
fi

# Volume control
if is_installed volumeicon; then
    volumeicon &
    log_event "Started volume control applet"
fi

# Battery monitoring
if is_installed cbatticon; then
    cbatticon -u 20 &
    log_event "Started battery monitor applet"
fi

# Clipboard manager
if is_installed parcellite; then
    parcellite &
    log_event "Started clipboard manager"
fi

# Bluetooth management
if is_installed blueman-applet; then
    blueman-applet &
    log_event "Started bluetooth manager applet"
fi

# Printer management
if is_installed system-config-printer-applet; then
    system-config-printer-applet &
    log_event "Started printer manager applet"
fi

# =============================================================================
# SYSTEM CONFIGURATION
# =============================================================================

log_event "--- Configuring system settings ---"

# Set keyboard layout
if is_installed setxkbmap; then
    setxkbmap pl &
    log_event "Set keyboard layout to Polish"
fi

# Start keyring daemon
if is_installed gnome-keyring-daemon; then
    /usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh &
    log_event "Started gnome-keyring-daemon"
fi

# =============================================================================
# STATUS BAR (OPTIONAL)
# =============================================================================

log_event "--- Starting status bar ---"

# Polybar status bar
if is_installed polybar; then
    # Kill existing polybar instances
    kill_existing polybar
    
    # Wait for processes to terminate
    while pgrep -u "$UID" -x polybar >/dev/null; do
        sleep 0.5
    done
    
    # Start polybar with EXWM configuration
    if [ -f "$CONFIG_DIR/polybar/config.ini" ]; then
        polybar exwm-bar &
        log_event "Started polybar with custom config"
    else
        polybar exwm &
        log_event "Started polybar with default config"
    fi
    wait_for_process polybar
fi

# =============================================================================
# SCRIPT PERMISSIONS
# =============================================================================

log_event "--- Setting script permissions ---"

# Ensure scripts are executable
if [ -f "$SCRIPT_DIR/powermenu.sh" ]; then
    chmod +x "$SCRIPT_DIR/powermenu.sh"
    log_event "Set powermenu.sh as executable"
fi

if [ -f "$SCRIPT_DIR/autostart.sh" ]; then
    chmod +x "$SCRIPT_DIR/autostart.sh"
    log_event "Set autostart.sh as executable"
fi

# =============================================================================
# CUSTOM APPLICATIONS
# =============================================================================

log_event "--- Starting custom applications ---"

# Add your custom autostart applications here
# Example:
# if is_installed firefox; then
#     firefox &
#     log_event "Started Firefox"
# fi

# if is_installed slack; then
#     slack &
#     log_event "Started Slack"
# fi

# =============================================================================
# SCRIPT COMPLETION
# =============================================================================

log_event "=== EXWM Autostart Script Completed ==="
log_event "Total running processes: $(ps aux | wc -l)"

# Display summary
echo "EXWM autostart completed. Check log: $LOG_FILE"

exit 0