#!/bin/bash
# =============================================================================
# X11 Startup Script for Emacs AI 2.0 + EXWM
# =============================================================================
# 
# This script initializes the X11 environment and starts the EXWM
# desktop environment with proper configuration and error handling.
#
# Author: Krispi
# Version: 2.0
# License: GPL-3.0
# =============================================================================

# =============================================================================
# CONFIGURATION
# =============================================================================

# Script configuration
SCRIPT_NAME="$(basename "$0")"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="$HOME/.config/exwm/xinitrc.log"
CONFIG_DIR="$HOME/.config"

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Log events with timestamp
log_event() {
    local message="$1"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $SCRIPT_NAME: $message" >> "$LOG_FILE"
}

# Display error message
show_error() {
    local message="$1"
    echo "ERROR: $message" >&2
    log_event "ERROR: $message"
}

# Display info message
show_info() {
    local message="$1"
    echo "INFO: $message"
    log_event "INFO: $message"
}

# Check if a program is installed
is_installed() {
    command -v "$1" &>/dev/null
}

# Check if running in X11
check_x11() {
    if [ -z "$DISPLAY" ]; then
        show_error "DISPLAY variable not set. Are you running in X11?"
        return 1
    fi
    return 0
}

# Check if running as root
check_root() {
    if [ "$EUID" -eq 0 ]; then
        show_error "This script should not be run as root"
        exit 1
    fi
}

# Create necessary directories
create_directories() {
    local dirs=(
        "$HOME/.config/exwm"
        "$HOME/.config/picom"
        "$HOME/.config/rofi"
        "$HOME/.local/share/applications"
    )
    
    for dir in "${dirs[@]}"; do
        if [ ! -d "$dir" ]; then
            mkdir -p "$dir"
            show_info "Created directory: $dir"
        fi
    done
}

# =============================================================================
# X11 ENVIRONMENT SETUP
# =============================================================================

# Load X resources
load_x_resources() {
    show_info "Loading X resources..."
    
    # Load .Xresources if it exists
    if [ -f "$HOME/.Xresources" ]; then
        xrdb -merge "$HOME/.Xresources"
        show_info "Loaded .Xresources"
    fi
    
    # Load .Xdefaults if it exists
    if [ -f "$HOME/.Xdefaults" ]; then
        xrdb -merge "$HOME/.Xdefaults"
        show_info "Loaded .Xdefaults"
    fi
    
    # Set default X resources
    xrdb -merge << EOF
! Default X resources for EXWM
*background: #1e1e2e
*foreground: #cdd6f4
*cursorColor: #cba6f7
*pointerColor: #cba6f7
EOF
    show_info "Set default X resources"
}

# Configure keyboard layout
setup_keyboard() {
    show_info "Setting up keyboard layout..."
    
    # Set Polish keyboard layout
    if is_installed setxkbmap; then
        setxkbmap pl
        show_info "Set keyboard layout to Polish"
    else
        show_error "setxkbmap not found"
    fi
    
    # Enable numlock
    if is_installed numlockx; then
        numlockx on
        show_info "Enabled numlock"
    fi
}

# Configure mouse
setup_mouse() {
    show_info "Setting up mouse..."
    
    # Set mouse acceleration
    if is_installed xset; then
        xset m 1 1
        show_info "Set mouse acceleration"
    fi
    
    # Enable mouse keys
    if is_installed xset; then
        xset m on
        show_info "Enabled mouse keys"
    fi
}

# Configure display
setup_display() {
    show_info "Setting up display..."
    
    # Set DPI if needed
    if [ -n "$DPI" ]; then
        xrdb -merge << EOF
Xft.dpi: $DPI
EOF
        show_info "Set DPI to $DPI"
    fi
    
    # Enable DPMS
    if is_installed xset; then
        xset +dpms
        xset dpms 300 600 900
        show_info "Enabled DPMS with timeout 5/10/15 minutes"
    fi
}

# =============================================================================
# SYSTEM SERVICES
# =============================================================================

# Start system services
start_system_services() {
    show_info "Starting system services..."
    
    # Start dbus session
    if ! pgrep -x "dbus-daemon" >/dev/null; then
        eval "$(dbus-launch --sh-syntax --exit-with-session)"
        show_info "Started dbus session"
    fi
    
    # Start pulseaudio if not running
    if is_installed pulseaudio && ! pgrep -x "pulseaudio" >/dev/null; then
        pulseaudio --start --log-level=0 &
        show_info "Started pulseaudio"
    fi
    
    # Start network manager if needed
    if is_installed nm-applet && ! pgrep -x "nm-applet" >/dev/null; then
        nm-applet &
        show_info "Started network manager applet"
    fi
}

# =============================================================================
# EXWM STARTUP
# =============================================================================

# Start EXWM
start_exwm() {
    show_info "Starting EXWM desktop environment..."
    
    # Check if Emacs is installed
    if ! is_installed emacs; then
        show_error "Emacs is not installed. Please install Emacs first."
        exit 1
    fi
    
    # Set environment variables
    export EXWM_MODE=1
    export EXWM_CONFIG_DIR="$SCRIPT_DIR"
    
    # Start Emacs with EXWM
    if [ "$1" = "--debug" ]; then
        show_info "Starting Emacs with debug mode..."
        exec dbus-launch --exit-with-session emacs --debug-init
    else
        show_info "Starting Emacs with EXWM..."
        exec dbus-launch --exit-with-session emacs
    fi
}

# =============================================================================
# CLEANUP AND ERROR HANDLING
# =============================================================================

# Cleanup function
cleanup() {
    show_info "Cleaning up..."
    
    # Kill background processes
    jobs -p | xargs -r kill
    
    # Restore default cursor
    if is_installed xsetroot; then
        xsetroot -cursor_name left_ptr
    fi
    
    show_info "Cleanup completed"
}

# Error handler
error_handler() {
    local exit_code=$?
    show_error "Script failed with exit code $exit_code"
    cleanup
    exit $exit_code
}

# Signal handlers
trap cleanup EXIT
trap error_handler ERR

# =============================================================================
# MAIN EXECUTION
# =============================================================================

# Main function
main() {
    show_info "Starting X11 initialization for EXWM..."
    
    # Check prerequisites
    check_root
    check_x11
    
    # Create log directory
    mkdir -p "$(dirname "$LOG_FILE")"
    
    # Log script start
    log_event "X11 initialization started"
    log_event "User: $USER"
    log_event "Home: $HOME"
    log_event "Display: $DISPLAY"
    log_event "Script directory: $SCRIPT_DIR"
    
    # Create necessary directories
    create_directories
    
    # Setup X11 environment
    load_x_resources
    setup_keyboard
    setup_mouse
    setup_display
    
    # Start system services
    start_system_services
    
    # Start EXWM
    start_exwm "$@"
}

# =============================================================================
# SCRIPT EXECUTION
# =============================================================================

# Check if script is sourced
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # Script is executed directly
    main "$@"
else
    # Script is sourced
    show_info "Script sourced, use main function to start"
fi

# =============================================================================
# END OF X11 STARTUP SCRIPT
# =============================================================================