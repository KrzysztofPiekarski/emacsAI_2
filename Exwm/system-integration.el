;; =============================================================================
;; EXWM System Integration - Emacs AI 2.0 + EXWM
;; =============================================================================
;; 
;; This file handles system-level integration for EXWM including:
;; - LightDM integration
;; - X11 configuration
;; - System tray and notifications
;; - Power management
;; - Multi-monitor support
;;
;; Author: Krispi
;; Version: 2.0
;; License: GPL-3.0
;; =============================================================================

;; =============================================================================
;; SYSTEM INTEGRATION SETTINGS
;; =============================================================================

;; Enable system tray integration
(use-package exwm-systemtray
  :after exwm
  :config
  ;; System tray height
  (setq exwm-systemtray-height 24)
  
  ;; Enable system tray
  (exwm-systemtray-enable)
  
  ;; Customize system tray appearance
  (setq exwm-systemtray-icon-gap 2))

;; =============================================================================
;; MULTI-MONITOR SUPPORT
;; =============================================================================

;; Enable RandR for multi-monitor support
(use-package exwm-randr
  :after exwm
  :config
  ;; Enable RandR
  (exwm-randr-enable)
  
  ;; Default workspace output (adjust for your setup)
  (setq exwm-randr-workspace-output-plist '(0 "eDP-1"))
  
  ;; Handle screen changes
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP-1 --auto")))
  
  ;; Multi-monitor workspace distribution
  (defun exwm-setup-multi-monitor ()
    "Setup workspaces for multi-monitor configuration."
    (interactive)
    ;; Example: Distribute workspaces across monitors
    ;; (exwm-randr-workspace-output-plist
    ;;  '(0 "eDP-1"    ; Workspace 0 on laptop screen
    ;;     1 "HDMI-1"  ; Workspace 1 on external monitor
    ;;     2 "eDP-1"   ; Workspace 2 on laptop screen
    ;;     3 "HDMI-1")) ; Workspace 3 on external monitor
    )
  
  ;; Run multi-monitor setup
  (add-hook 'exwm-init-hook 'exwm-setup-multi-monitor))

;; =============================================================================
;; POWER MANAGEMENT INTEGRATION
;; =============================================================================

;; Desktop environment integration for power management
(use-package desktop-environment
  :after exwm
  :config
  ;; Enable desktop environment mode
  (desktop-environment-mode)
  
  ;; Brightness control
  (setq desktop-environment-brightness-small-increment "2%+")
  (setq desktop-environment-brightness-small-decrement "2%-")
  (setq desktop-environment-brightness-normal-increment "5%+")
  (setq desktop-environment-brightness-normal-decrement "5%-")
  
  ;; Volume control
  (setq desktop-environment-volume-small-increment "2%+")
  (setq desktop-environment-volume-small-decrement "2%-")
  (setq desktop-environment-volume-normal-increment "5%+")
  (setq desktop-environment-volume-normal-decrement "5%-")
  
  ;; Screen brightness commands
  (setq desktop-environment-brightness-get-command "brightnessctl get")
  (setq desktop-environment-brightness-set-command "brightnessctl set %s")
  
  ;; Volume commands
  (setq desktop-environment-volume-get-command "pamixer --get-volume")
  (setq desktop-environment-volume-set-command "pamixer --set-volume %s")
  (setq desktop-environment-volume-mute-command "pamixer --toggle-mute"))

;; =============================================================================
;; NOTIFICATION SYSTEM INTEGRATION
;; =============================================================================

;; Notification system integration
(use-package exwm-notification
  :after exwm
  :if (require 'exwm-notification nil t)
  :config
  ;; Customize notification appearance
  (setq exwm-notification-timeout 5000)
  
  ;; Notification position
  (setq exwm-notification-position 'top-right))

;; =============================================================================
;; CLIPBOARD INTEGRATION
;; =============================================================================

;; Clipboard integration
(use-package exwm-clipboard
  :after exwm
  :if (require 'exwm-clipboard nil t)
  :config
  ;; Enable clipboard integration
  (exwm-clipboard-enable))

;; =============================================================================
;; NETWORK AND SYSTEM STATUS
;; =============================================================================

;; Network status integration
(defun exwm-network-status ()
  "Get current network status."
  (interactive)
  (let ((status (shell-command-to-string "nmcli -t -f active,ssid dev wifi | grep '^yes' | cut -d: -f2")))
    (if (string-empty-p status)
        "No WiFi"
      (concat "WiFi: " (string-trim status)))))

;; Battery status integration
(defun exwm-battery-status ()
  "Get current battery status."
  (interactive)
  (let ((battery (shell-command-to-string "cat /sys/class/power_supply/BAT0/capacity 2>/dev/null")))
    (if (string-empty-p battery)
        "AC"
      (concat "Battery: " (string-trim battery) "%"))))

;; System load integration
(defun exwm-system-load ()
  "Get current system load."
  (interactive)
  (let ((load (shell-command-to-string "uptime | awk -F'load average:' '{print $2}' | awk '{print $1}'")))
    (if (string-empty-p load)
        "Unknown"
      (string-trim load))))

;; =============================================================================
;; SYSTEM TRAY APPLETS
;; =============================================================================

;; System tray applet management
(defun exwm-start-system-applets ()
  "Start essential system tray applets."
  (interactive)
  
  ;; Network manager applet
  (when (executable-find "nm-applet")
    (start-process-shell-command "nm-applet" nil "nm-applet"))
  
  ;; Volume control applet
  (when (executable-find "volumeicon")
    (start-process-shell-command "volumeicon" nil "volumeicon"))
  
  ;; Battery monitor applet
  (when (executable-find "cbatticon")
    (start-process-shell-command "cbatticon" nil "cbatticon -u 20"))
  
  ;; Clipboard manager
  (when (executable-find "parcellite")
    (start-process-shell-command "parcellite" nil "parcellite"))
  
  ;; Bluetooth manager
  (when (executable-find "blueman-applet")
    (start-process-shell-command "blueman-applet" nil "blueman-applet")))

;; =============================================================================
;; COMPOSITOR INTEGRATION
;; =============================================================================

;; Compositor management
(defun exwm-start-compositor ()
  "Start compositor for visual effects."
  (interactive)
  
  ;; Kill existing compositor processes
  (when (executable-find "pkill")
    (start-process-shell-command "kill-compositor" nil "pkill picom; pkill compton"))
  
  ;; Start picom (preferred)
  (if (executable-find "picom")
      (progn
        (if (file-exists-p "~/.config/picom/picom.conf")
            (start-process-shell-command "picom" nil "picom -b --config ~/.config/picom/picom.conf")
          (start-process-shell-command "picom" nil "picom -b"))
        (message "Started picom compositor"))
    
    ;; Fallback to compton
    (if (executable-find "compton")
        (progn
          (start-process-shell-command "compton" nil "compton -b")
          (message "Started compton compositor"))
      (message "No compositor found (picom or compton)"))))

;; =============================================================================
;; DESKTOP BACKGROUND
;; =============================================================================

;; Desktop background management
(defun exwm-set-background ()
  "Set desktop background."
  (interactive)
  
  ;; Try nitrogen first
  (if (executable-find "nitrogen")
      (start-process-shell-command "nitrogen" nil "nitrogen --restore")
    
    ;; Fallback to feh
    (if (executable-find "feh")
        (if (file-exists-p "~/.config/wallpaper.jpg")
            (start-process-shell-command "feh" nil "feh --bg-scale ~/.config/wallpaper.jpg")
          (start-process-shell-command "feh" nil "feh --bg-scale ~/.config/wallpaper.png"))
      
      ;; Fallback to solid color
      (if (executable-find "hsetroot")
          (start-process-shell-command "hsetroot" nil "hsetroot -solid '#2E3440'")
        (message "No background setter found")))))

;; =============================================================================
;; SYSTEM INITIALIZATION
;; =============================================================================

;; System initialization hook
(defun exwm-system-init ()
  "Initialize system integration features."
  (interactive)
  
  ;; Start system applets
  (exwm-start-system-applets)
  
  ;; Start compositor
  (exwm-start-compositor)
  
  ;; Set background
  (exwm-set-background)
  
  ;; Start notification daemon
  (when (executable-find "dunst")
    (start-process-shell-command "dunst" nil "dunst"))
  
  ;; Start keyring daemon
  (when (executable-find "gnome-keyring-daemon")
    (start-process-shell-command "keyring" nil "/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh"))
  
  (message "EXWM system integration initialized"))

;; =============================================================================
;; HOOKS AND INTEGRATION
;; =============================================================================

;; Add system initialization to EXWM startup
(add-hook 'exwm-init-hook 'exwm-system-init)

;; =============================================================================
;; PROVIDE MODULE
;; =============================================================================

(provide 'system-integration)

;; =============================================================================
;; END OF SYSTEM INTEGRATION
;; =============================================================================
