#!/bin/bash
# =============================================================================
# EXWM Power Menu Script - Emacs AI 2.0 + EXWM
# =============================================================================
# 
# This script provides a power management menu using rofi for the EXWM
# desktop environment.
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
ROFI_THEME="$HOME/.config/rofi/powermenu.rasi"
ROFI_THEME_FALLBACK="$HOME/.config/rofi/config.rasi"
LOG_FILE="$HOME/.config/exwm/powermenu.log"

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Log events with timestamp
log_event() {
    local message="$1"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $SCRIPT_NAME: $message" >> "$LOG_FILE"
}

# Check if a program is installed
is_installed() {
    command -v "$1" &>/dev/null
}

# Display error message
show_error() {
    local message="$1"
    if is_installed notify-send; then
        notify-send -u critical "EXWM Power Menu Error" "$message"
    else
        echo "ERROR: $message" >&2
    fi
    log_event "ERROR: $message"
}

# Check if running as root
check_root() {
    if [ "$EUID" -eq 0 ]; then
        show_error "This script should not be run as root"
        exit 1
    fi
}

# =============================================================================
# POWER MANAGEMENT FUNCTIONS
# =============================================================================

# Lock screen
lock_screen() {
    log_event "Locking screen"
    if is_installed slock; then
        slock
    elif is_installed i3lock; then
        i3lock
    elif is_installed xlock; then
        xlock
    else
        show_error "No screen locker found (slock, i3lock, or xlock)"
        return 1
    fi
}

# Logout from EXWM
logout_exwm() {
    log_event "Logging out from EXWM"
    if is_installed pkill; then
        pkill emacs
    else
        show_error "pkill not found"
        return 1
    fi
}

# Suspend system
suspend_system() {
    log_event "Suspending system"
    if is_installed systemctl; then
        systemctl suspend
    elif is_installed pm-suspend; then
        pm-suspend
    else
        show_error "No suspend command found (systemctl or pm-suspend)"
        return 1
    fi
}

# Hibernate system
hibernate_system() {
    log_event "Hibernating system"
    if is_installed systemctl; then
        systemctl hibernate
    elif is_installed pm-hibernate; then
        pm-hibernate
    else
        show_error "No hibernate command found (systemctl or pm-hibernate)"
        return 1
    fi
}

# Restart system
restart_system() {
    log_event "Restarting system"
    if is_installed systemctl; then
        systemctl reboot
    elif is_installed reboot; then
        reboot
    else
        show_error "No restart command found (systemctl or reboot)"
        return 1
    fi
}

# Shutdown system
shutdown_system() {
    log_event "Shutting down system"
    if is_installed systemctl; then
        systemctl poweroff
    elif is_installed poweroff; then
        poweroff
    elif is_installed shutdown; then
        shutdown -h now
    else
        show_error "No shutdown command found (systemctl, poweroff, or shutdown)"
        return 1
    fi
}

# =============================================================================
# ROFI INTEGRATION
# =============================================================================

# Get rofi theme path
get_rofi_theme() {
    if [ -f "$ROFI_THEME" ]; then
        echo "$ROFI_THEME"
    elif [ -f "$ROFI_THEME_FALLBACK" ]; then
        echo "$ROFI_THEME_FALLBACK"
    else
        echo ""
    fi
}

# Show power menu using rofi
show_power_menu() {
    local theme_path
    theme_path=$(get_rofi_theme)
    
    if [ -z "$theme_path" ]; then
        show_error "No rofi theme found"
        return 1
    fi
    
    # Power menu options
    local options="Zablokuj ekran\nWyloguj\nUśpij\nHibernuj\nUruchom ponownie\nWyłącz"
    
    # Show menu and get user choice
    local chosen
    chosen=$(echo -e "$options" | rofi -dmenu -i -p "Zasilanie" -theme "$theme_path")
    
    # Handle user choice
    case "$chosen" in
        "Zablokuj ekran")
            lock_screen
            ;;
        "Wyloguj")
            logout_exwm
            ;;
        "Uśpij")
            suspend_system
            ;;
        "Hibernuj")
            hibernate_system
            ;;
        "Uruchom ponownie")
            restart_system
            ;;
        "Wyłącz")
            shutdown_system
            ;;
        "")
            log_event "No option selected"
            ;;
        *)
            log_event "Unknown option selected: $chosen"
            ;;
    esac
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

# Check if not running as root
check_root

# Create log directory if it doesn't exist
mkdir -p "$(dirname "$LOG_FILE")"

# Log script start
log_event "Power menu script started"

# Check if rofi is installed
if ! is_installed rofi; then
    show_error "rofi is not installed"
    exit 1
fi

# Show power menu
if show_power_menu; then
    log_event "Power menu completed successfully"
else
    log_event "Power menu failed"
    exit 1
fi

exit 0