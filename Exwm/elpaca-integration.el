;; =============================================================================
;; Elpaca Integration for EXWM - Emacs AI 2.0 + EXWM
;; =============================================================================
;; 
;; This file handles package installation and integration for EXWM
;; using the Elpaca package manager.
;;
;; Author: Krispi
;; Version: 2.0
;; License: GPL-3.0
;; =============================================================================

;; =============================================================================
;; EXWM PACKAGE INSTALLATION
;; =============================================================================

;; Install EXWM from source (recommended for latest features)
(elpaca '(exwm :host github
               :repo "ch11ng/exwm"
               :branch "master"))

;; Desktop environment integration for multimedia keys and system controls
(elpaca '(desktop-environment
          :host github
          :repo "DamienCassou/desktop-environment"))

;; Wait for packages to be installed
(elpaca-wait)

;; =============================================================================
;; LOAD EXWM CONFIGURATION
;; =============================================================================

;; Load EXWM configuration after packages are ready
(add-to-list 'load-path "~/.emacs.d/Exwm/")
(load "exwm-config")

;; =============================================================================
;; DESKTOP ENVIRONMENT INTEGRATION
;; =============================================================================

;; Configure desktop-environment for better multimedia key support
(use-package desktop-environment
  :after exwm
  :config
  (desktop-environment-mode)
  
  ;; Customize brightness control increments
  (setq desktop-environment-brightness-small-increment "2%+")
  (setq desktop-environment-brightness-small-decrement "2%-")
  (setq desktop-environment-brightness-normal-increment "5%+")
  (setq desktop-environment-brightness-normal-decrement "5%-")
  
  ;; Customize volume control increments
  (setq desktop-environment-volume-small-increment "2%+")
  (setq desktop-environment-volume-small-decrement "2%-")
  (setq desktop-environment-volume-normal-increment "5%+")
  (setq desktop-environment-volume-normal-decrement "5%-"))

;; =============================================================================
;; POST-EXWM INITIALIZATION
;; =============================================================================

;; Custom initialization after EXWM is loaded
(defun elpaca--exwm-startup ()
  "Initialize additional features after EXWM startup."
  ;; Ensure system tray is properly configured
  (when (require 'exwm-systemtray nil t)
    (exwm-systemtray-enable))
  
  ;; Set up additional hooks if needed
  (add-hook 'exwm-workspace-switch-hook
            (lambda ()
              (message "Switched to workspace %d" (exwm-workspace--position (exwm-workspace--current) 0))))
  
  ;; Log successful startup
  (message "EXWM initialized successfully with Elpaca integration"))

;; Hook into EXWM initialization
(with-eval-after-load 'exwm
  (add-hook 'exwm-init-hook #'elpaca--exwm-startup))

;; =============================================================================
;; PROVIDE MODULE
;; =============================================================================

(provide 'elpaca-integration)

;; =============================================================================
;; END OF ELPACA INTEGRATION
;; =============================================================================