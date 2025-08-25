;; =============================================================================
;; EXWM Keybindings - Emacs AI 2.0 + EXWM
;; =============================================================================
;; 
;; This file contains all keybindings and shortcuts for the EXWM
;; desktop environment, organized by functionality.
;;
;; Author: Krispi
;; Version: 2.0
;; License: GPL-3.0
;; =============================================================================

;; =============================================================================
;; WORKSPACE MANAGEMENT KEYBINDINGS
;; =============================================================================

;; Workspace switching: Super + 1-4
(defun exwm-setup-workspace-keys ()
  "Setup workspace switching keybindings."
  (setq exwm-input-global-keys
        (append exwm-input-global-keys
                (mapcar (lambda (i)
                          `(,(kbd (format "s-%d" i)) .
                            (lambda ()
                              (interactive)
                              (exwm-workspace-switch-create ,(- i 1)))))
                        (number-sequence 1 4)))))

;; =============================================================================
;; APPLICATION LAUNCHING KEYBINDINGS
;; =============================================================================

;; Application launcher keybindings
(defun exwm-setup-launcher-keys ()
  "Setup application launcher keybindings."
  (setq exwm-input-global-keys
        (append exwm-input-global-keys
                `(
                  ;; Application launcher
                  (,(kbd "s-p") . exwm-run-rofi)
                  
                  ;; Window switcher
                  (,(kbd "s-Tab") . exwm-run-rofi-window)
                  
                  ;; Power menu
                  (,(kbd "s-x") . exwm-run-powermenu)
                  
                  ;; Terminal
                  (,(kbd "s-<return>") . exwm-run-terminal)
                  ))))

;; =============================================================================
;; WINDOW MANAGEMENT KEYBINDINGS
;; =============================================================================

;; Window management keybindings
(defun exwm-setup-window-keys ()
  "Setup window management keybindings."
  (setq exwm-input-global-keys
        (append exwm-input-global-keys
                `(
                  ;; Close window
                  (,(kbd "s-q") . (lambda ()
                                   (interactive)
                                   (if exwm--id
                                       (exwm-workspace-delete-window)
                                     (delete-window))))
                  
                  ;; Fullscreen toggle
                  (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
                  
                  ;; Maximize toggle
                  (,(kbd "s-m") . exwm-layout-toggle-maximization)
                  
                  ;; Floating toggle
                  (,(kbd "s-t") . exwm-floating-toggle-floating)
                  
                  ;; Move window to next workspace
                  (,(kbd "s-<right>") . (lambda ()
                                          (interactive)
                                          (exwm-workspace-move-window 
                                           (exwm-workspace--position (exwm-workspace--current) 1))))
                  
                  ;; Move window to previous workspace
                  (,(kbd "s-<left>") . (lambda ()
                                         (interactive)
                                         (exwm-workspace-move-window 
                                          (exwm-workspace--position (exwm-workspace--current) -1))))
                  
                  ;; Resize window
                  (,(kbd "s-<up>") . (lambda ()
                                       (interactive)
                                       (exwm-layout-enlarge-window 0 -20)))
                  
                  (,(kbd "s-<down>") . (lambda ()
                                         (interactive)
                                         (exwm-layout-enlarge-window 0 20)))
                  ))))

;; =============================================================================
;; SYSTEM CONTROL KEYBINDINGS
;; =============================================================================

;; System control keybindings
(defun exwm-setup-system-keys ()
  "Setup system control keybindings."
  (setq exwm-input-global-keys
        (append exwm-input-global-keys
                `(
                  ;; Screen lock
                  (,(kbd "s-l") . exwm-lock-screen)
                  
                  ;; Reload configuration
                  (,(kbd "s-r") . exwm-reset)
                  
                  ;; Toggle keyboard input mode
                  (,(kbd "s-i") . exwm-input-toggle-keyboard)
                  
                  ;; Screenshot
                  (,(kbd "s-s") . (lambda ()
                                   (interactive)
                                   (start-process-shell-command "screenshot" nil "flameshot gui")))
                  
                  ;; Volume control
                  (,(kbd "s-=") . (lambda ()
                                   (interactive)
                                   (start-process-shell-command "volume-up" nil "pamixer -i 5")))
                  
                  (,(kbd "s--") . (lambda ()
                                   (interactive)
                                   (start-process-shell-command "volume-down" nil "pamixer -d 5")))
                  
                  (,(kbd "s-0") . (lambda ()
                                   (interactive)
                                   (start-process-shell-command "volume-mute" nil "pamixer -t")))
                  
                  ;; Brightness control
                  (,(kbd "s-+") . (lambda ()
                                   (interactive)
                                   (start-process-shell-command "brightness-up" nil "brightnessctl set +5%")))
                  
                  (,(kbd "s-_") . (lambda ()
                                   (interactive)
                                   (start-process-shell-command "brightness-down" nil "brightnessctl set 5%-")))
                  ))))

;; =============================================================================
;; MEDIA CONTROL KEYBINDINGS
;; =============================================================================

;; Media control keybindings
(defun exwm-setup-media-keys ()
  "Setup media control keybindings."
  (setq exwm-input-global-keys
        (append exwm-input-global-keys
                `(
                  ;; Media playback control
                  (,(kbd "s-<prior>") . (lambda ()
                                         (interactive)
                                         (start-process-shell-command "media-prev" nil "playerctl previous")))
                  
                  (,(kbd "s-<next>") . (lambda ()
                                         (interactive)
                                         (start-process-shell-command "media-next" nil "playerctl next")))
                  
                  (,(kbd "s-<home>") . (lambda ()
                                         (interactive)
                                         (start-process-shell-command "media-play-pause" nil "playerctl play-pause")))
                  
                  (,(kbd "s-<end>") . (lambda ()
                                        (interactive)
                                        (start-process-shell-command "media-stop" nil "playerctl stop")))
                  ))))

;; =============================================================================
;; DEVELOPMENT KEYBINDINGS
;; =============================================================================

;; Development-specific keybindings
(defun exwm-setup-development-keys ()
  "Setup development-specific keybindings."
  (setq exwm-input-global-keys
        (append exwm-input-global-keys
                `(
                  ;; Quick file operations
                  (,(kbd "s-e") . (lambda ()
                                   (interactive)
                                   (start-process-shell-command "file-manager" nil "thunar")))
                  
                  ;; Quick terminal
                  (,(kbd "s-`") . (lambda ()
                                   (interactive)
                                   (start-process-shell-command "quick-terminal" nil "alacritty")))
                  
                  ;; Browser
                  (,(kbd "s-b") . (lambda ()
                                   (interactive)
                                   (start-process-shell-command "browser" nil "firefox")))
                  ))))

;; =============================================================================
;; EMERGENCY KEYBINDINGS
;; =============================================================================

;; Emergency keybindings for system recovery
(defun exwm-setup-emergency-keys ()
  "Setup emergency keybindings for system recovery."
  (setq exwm-input-global-keys
        (append exwm-input-global-keys
                `(
                  ;; Force quit application
                  (,(kbd "s-F4") . (lambda ()
                                    (interactive)
                                    (when exwm--id
                                      (exwm-workspace-delete-window))))
                  
                  ;; Emergency restart EXWM
                  (,(kbd "s-F12") . (lambda ()
                                      (interactive)
                                      (exwm-reset)))
                  
                  ;; Emergency terminal
                  (,(kbd "s-F1") . (lambda ()
                                    (interactive)
                                    (start-process-shell-command "emergency-terminal" nil "xterm")))
                  ))))

;; =============================================================================
;; CUSTOM APPLICATION KEYBINDINGS
;; =============================================================================

;; Custom application keybindings
(defun exwm-setup-custom-app-keys ()
  "Setup custom application keybindings."
  (setq exwm-input-global-keys
        (append exwm-input-global-keys
                `(
                  ;; Custom application shortcuts
                  ;; Add your custom shortcuts here
                  
                  ;; Example:
                  ;; (,(kbd "s-c") . (lambda ()
                  ;;                  (interactive)
                  ;;                  (start-process-shell-command "calculator" nil "gnome-calculator")))
                  ))))

;; =============================================================================
;; KEYBINDING SETUP FUNCTION
;; =============================================================================

;; Main keybinding setup function
(defun exwm-setup-all-keybindings ()
  "Setup all EXWM keybindings."
  (interactive)
  
  ;; Initialize global keys list
  (setq exwm-input-global-keys '())
  
  ;; Setup all keybinding categories
  (exwm-setup-workspace-keys)
  (exwm-setup-launcher-keys)
  (exwm-setup-window-keys)
  (exwm-setup-system-keys)
  (exwm-setup-media-keys)
  (exwm-setup-development-keys)
  (exwm-setup-emergency-keys)
  (exwm-setup-custom-app-keys)
  
  ;; Apply keybindings
  (setq exwm-input-global-keys exwm-input-global-keys)
  
  (message "EXWM keybindings configured"))

;; =============================================================================
;; KEYBINDING HELP SYSTEM
;; =============================================================================

;; Keybinding help system
(defun exwm-show-keybindings-help ()
  "Show EXWM keybindings help."
  (interactive)
  (let ((help-buffer (get-buffer-create "*EXWM Keybindings*")))
    (with-current-buffer help-buffer
      (erase-buffer)
      (insert "EXWM Keybindings Reference\n")
      (insert "==========================\n\n")
      
      (insert "Workspace Management:\n")
      (insert "  Super + 1-4     - Switch to workspace 1-4\n")
      (insert "  Super + ←/→      - Move window between workspaces\n\n")
      
      (insert "Application Launchers:\n")
      (insert "  Super + p        - Application launcher (rofi)\n")
      (insert "  Super + Tab      - Window switcher (rofi)\n")
      (insert "  Super + x        - Power menu\n")
      (insert "  Super + Return   - Terminal\n\n")
      
      (insert "Window Management:\n")
      (insert "  Super + q        - Close window\n")
      (insert "  Super + f        - Toggle fullscreen\n")
      (insert "  Super + m        - Toggle maximize\n")
      (insert "  Super + t        - Toggle floating\n")
      (insert "  Super + ↑/↓      - Resize window\n\n")
      
      (insert "System Control:\n")
      (insert "  Super + l        - Lock screen\n")
      (insert "  Super + r        - Reload EXWM\n")
      (insert "  Super + i        - Toggle keyboard mode\n")
      (insert "  Super + s        - Screenshot\n\n")
      
      (insert "Volume Control:\n")
      (insert "  Super + =        - Volume up\n")
      (insert "  Super + -        - Volume down\n")
      (insert "  Super + 0        - Mute/unmute\n\n")
      
      (insert "Brightness Control:\n")
      (insert "  Super + +        - Brightness up\n")
      (insert "  Super + _        - Brightness down\n\n")
      
      (insert "Media Control:\n")
      (insert "  Super + PgUp     - Previous track\n")
      (insert "  Super + PgDn     - Next track\n")
      (insert "  Super + Home      - Play/pause\n")
      (insert "  Super + End       - Stop\n\n")
      
      (insert "Emergency:\n")
      (insert "  Super + F4       - Force quit\n")
      (insert "  Super + F12      - Restart EXWM\n")
      (insert "  Super + F1       - Emergency terminal\n\n")
      
      (insert "Note: Super key is usually the Windows/Command key\n")
      (insert "All keybindings can be customized in exwm-keybindings.el"))
      
      (view-mode 1)
      (display-buffer help-buffer))))

;; =============================================================================
;; KEYBINDING INITIALIZATION
;; =============================================================================

;; Initialize keybindings when EXWM is ready
(with-eval-after-load 'exwm
  (add-hook 'exwm-init-hook 'exwm-setup-all-keybindings))

;; =============================================================================
;; PROVIDE MODULE
;; =============================================================================

(provide 'exwm-keybindings)

;; =============================================================================
;; END OF EXWM KEYBINDINGS
;; =============================================================================
