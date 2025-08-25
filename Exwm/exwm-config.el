;; =============================================================================
;; EXWM Configuration - Emacs AI 2.0 + EXWM Desktop Environment
;; =============================================================================
;; 
;; This file provides a complete EXWM configuration with:
;; - Workspace management (4 workspaces)
;; - Rofi integration for application launching
;; - System tray and power management
;; - Automatic application placement
;; - Enhanced key bindings
;; - Multi-monitor support
;;
;; Author: Krispi
;; Version: 2.0
;; License: GPL-3.0
;; =============================================================================

;; =============================================================================
;; PACKAGE INSTALLATION AND REQUIREMENTS
;; =============================================================================

(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (require 'exwm-systemtray)
  (require 'exwm-randr))

;; =============================================================================
;; CORE EXWM CONFIGURATION
;; =============================================================================

;; Basic EXWM settings
(setq exwm-workspace-number 4)                    ; Number of workspaces
(setq exwm-workspace-show-indicator t)            ; Show workspace indicator
(setq exwm-layout-show-all-buffers t)             ; Show all buffers in layout

;; Buffer naming - use application name as buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;; =============================================================================
;; WORKSPACE MANAGEMENT
;; =============================================================================

;; Automatic application placement to specific workspaces
(setq exwm-manage-configurations
      '(
        ;; Web browsing - Workspace 1
        ((equal exwm-class-name "Firefox") workspace 1)
        ((equal exwm-class-name "Chromium") workspace 1)
        ((equal exwm-class-name "Google-chrome") workspace 1)
        
        ;; Communication - Workspace 2
        ((equal exwm-class-name "Thunderbird") workspace 2)
        ((equal exwm-class-name "TelegramDesktop") workspace 2)
        ((equal exwm-class-name "Slack") workspace 2)
        
        ;; Development - Workspace 3
        ((equal exwm-class-name "code") workspace 3)
        ((equal exwm-class-name "jetbrains-idea") workspace 3)
        ((equal exwm-class-name "emacs") workspace 3)
        
        ;; Media and entertainment - Workspace 4
        ((equal exwm-class-name "vlc") workspace 4)
        ((equal exwm-class-name "spotify") workspace 4)
        ((equal exwm-class-name "gimp") workspace 4)
        
        ;; Floating windows
        ((equal exwm-class-name "Gimp") floating t)
        ((equal exwm-class-name "Pavucontrol") floating t)
        ((equal exwm-class-name "Blueman-manager") floating t)
        ))

;; =============================================================================
;; APPLICATION LAUNCHING AND INTEGRATION
;; =============================================================================

;; Rofi application launcher
(defun exwm-run-rofi ()
  "Launch rofi as application launcher."
  (interactive)
  (start-process-shell-command "rofi" nil "rofi -show drun -theme ~/.config/rofi/config.rasi"))

;; Rofi window switcher
(defun exwm-run-rofi-window ()
  "Switch windows using rofi."
  (interactive)
  (start-process-shell-command "rofi-window" nil "rofi -show window -theme ~/.config/rofi/config.rasi"))

;; Power menu using rofi
(defun exwm-run-powermenu ()
  "Launch power menu using rofi."
  (interactive)
  (start-process-shell-command "power-menu" nil "~/.config/exwm/powermenu.sh"))

;; Terminal launcher
(defun exwm-run-terminal ()
  "Launch terminal application."
  (interactive)
  (start-process-shell-command "terminal" nil "alacritty"))

;; Screen locker
(defun exwm-lock-screen ()
  "Lock the screen."
  (interactive)
  (start-process-shell-command "slock" nil "slock"))

;; =============================================================================
;; NATIVE X11 APPLICATION SUPPORT
;; =============================================================================

;; Key simulation for native X11 applications
(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-w] . [delete])
        ([?\M-w] . [backspace])))

;; =============================================================================
;; AUTOSTART AND INITIALIZATION
;; =============================================================================

;; Autostart applications
(defun exwm-autostart ()
  "Autostart applications after EXWM initialization."
  (interactive)
  (start-process-shell-command "autostart" nil "~/.config/exwm/autostart.sh"))

;; Run autostart after EXWM is ready
(add-hook 'exwm-init-hook 'exwm-autostart)

;; =============================================================================
;; ENHANCED WORKSPACE FEATURES
;; =============================================================================

;; Workspace indicator timeout
(setq exwm-workspace-indicator-timeout 2)

;; =============================================================================
;; WINDOW MANAGEMENT ENHANCEMENTS
;; =============================================================================

;; Enable floating windows for certain applications
(setq exwm-floating-setup-hook
      (lambda ()
        (exwm-layout-hide-mode-line)))

;; =============================================================================
;; PERFORMANCE AND RELIABILITY
;; =============================================================================

;; Optimize buffer management
(setq exwm-layout-show-all-buffers t)

;; =============================================================================
;; LOAD MODULAR COMPONENTS
;; =============================================================================

;; Load system integration
(require 'system-integration)

;; Load keybindings
(require 'exwm-keybindings)

;; =============================================================================
;; FINAL INITIALIZATION
;; =============================================================================

;; Enable EXWM
(exwm-enable)

;; =============================================================================
;; PROVIDE MODULE
;; =============================================================================

(provide 'exwm-config)

;; =============================================================================
;; END OF EXWM CONFIGURATION
;; =============================================================================