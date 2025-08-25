;;; utils.el --- Professional Utility Functions & Enhanced Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, utilities, tools, navigation, file-management
;; Description: Professional utility functions and enhanced tools for productivity
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; This module provides comprehensive utility functions and enhanced tools:
;; - Advanced navigation with Avy
;; - Terminal integration with VTerm
;; - Keybinding hints with Which-key
;; - File tree navigation with Neotree
;; - Professional file management with Treemacs
;; - Icon integration for visual enhancement
;;
;; Features:
;; - Fast character and line navigation
;; - Integrated terminal capabilities
;; - Intelligent keybinding hints
;; - Beautiful file tree navigation
;; - Professional file management
;; - Visual icon enhancement
;; - Workflow optimization tools

;;; Code:

;; ============================================================================
;; 🚀 ADVANCED NAVIGATION - FAST MOVEMENT & JUMPING
;; ============================================================================

;; === AVY - INTELLIGENT NAVIGATION ===
;; Avy provides fast, intelligent navigation to characters and lines
;; with visual feedback for efficient movement
(use-package avy
  :bind (("M-g M-g" . avy-goto-char-timer)  ;; Jump to character
         ("M-g f" . avy-goto-line)))         ;; Jump to line

;; ============================================================================
;; 🖥️ TERMINAL INTEGRATION - SEAMLESS SHELL ACCESS
;; ============================================================================

;; === VTERM - INTEGRATED TERMINAL ===
;; VTerm provides seamless terminal integration within Emacs
;; with advanced features and buffer management
(use-package vterm
  :commands vterm
  :config
  ;; Open or switch to existing vterm buffer
  (defun my/vterm ()
    "Switch to *vterm* buffer or open new one if it doesn't exist."
    (interactive)
    (if (get-buffer "*vterm*")
        (pop-to-buffer "*vterm*")
      (vterm)))

  ;; Kill vterm buffer
  (defun my/kill-vterm ()
    "Close *vterm* buffer if it exists."
    (interactive)
    (when (get-buffer "*vterm*")
      (kill-buffer "*vterm*")
      (message "vterm killed")))

  ;; Keybindings for terminal management
  (global-set-key (kbd "C-c t") #'my/vterm)      ;; Open/switch to vterm
  (global-set-key (kbd "C-c k") #'my/kill-vterm)) ;; Kill vterm

;; ============================================================================
;; ⌨️ KEYBINDING HINTS - INTELLIGENT HELP SYSTEM
;; ============================================================================

;; === WHICH-KEY - KEYBINDING ASSISTANCE ===
;; Which-key provides intelligent keybinding hints and suggestions
;; for better discoverability and learning
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)) ;; Faster display of hints

;; Load keybindings configuration
(load "keybindings.el")

;; ============================================================================
;; 🌳 FILE TREE NAVIGATION - VISUAL FILE MANAGEMENT
;; ============================================================================

;; === NEOTREE - BEAUTIFUL FILE TREE ===
;; Neotree provides a beautiful, visual file tree navigation
;; with enhanced appearance and smart features
(use-package neotree
  :ensure t
  :after (all-the-icons projectile)
  :bind ([f8] . neotree-toggle)
  :custom
  ;; Neotree style (icons in GUI, arrows in terminal)
  (neo-theme (if window-system 'icons 'arrow))
  (neo-window-width 35)                ;; Neotree window width - powiększone
  (neo-window-position 'left)          ;; Neotree position (left, right, top, bottom)
  (neo-smart-open t)                   ;; Auto-open file on click
  (neo-confirm-create-file #'off-p)    ;; Confirm file creation
  (neo-confirm-create-directory #'off-p) ;; Confirm directory creation
  (neo-show-updir-line t)              ;; Show parent directory
  (neo-mode-line-type 'neotree)        ;; Modeline type
  (neo-banner-message "🌳 File Explorer") ;; Banner message
  (neo-show-slash-for-folder t)        ;; Show slash for folders
  (neo-vc-integration '(face))          ;; Git integration
  :config
  ;; Function declarations to suppress warnings
  (declare-function linum-mode "linum" (&optional arg))
  (declare-function all-the-icons-icon-for-file "all-the-icons")
  (declare-function all-the-icons-icon-for-dir-with-chevron "all-the-icons")
  (declare-function nerd-icons-icon-for-file "nerd-icons")
  (declare-function nerd-icons-icon-for-dir "nerd-icons")
  (declare-function nerd-icons-octicon "nerd-icons")
  (declare-function projectile-project-buffers "projectile")
  
  ;; Enhanced color scheme and fonts for Neotree
  (set-face-attribute 'neo-dir-link-face nil 
                      :foreground "#87CEEB" 
                      :weight 'bold)            ;; Directory color - sky blue, bold
  (set-face-attribute 'neo-file-link-face nil 
                      :foreground "#DDA0DD")    ;; File color - plum
  (set-face-attribute 'neo-root-dir-face nil 
                      :foreground "#FFD700" 
                      :weight 'bold)            ;; Root directory - gold, bold
  (set-face-attribute 'neo-expand-btn-face nil 
                      :foreground "#32CD32")    ;; Expand button - lime green
  (set-face-attribute 'neo-banner-face nil 
                      :foreground "#FF69B4" 
                      :weight 'bold)            ;; Banner - hot pink, bold
  (set-face-attribute 'neo-header-face nil 
                      :foreground "#FFA500")    ;; Header - orange
  
  ;; Git integration colors
  (set-face-attribute 'neo-vc-default-face nil :foreground "#D3D3D3")  ;; Default git
  (set-face-attribute 'neo-vc-user-face nil :foreground "#90EE90")     ;; User changes - light green
  (set-face-attribute 'neo-vc-up-to-date-face nil :foreground "#98FB98") ;; Up to date - pale green
  (set-face-attribute 'neo-vc-edited-face nil :foreground "#FFA500")   ;; Edited - orange
  (set-face-attribute 'neo-vc-needs-merge-face nil :foreground "#FF6347") ;; Needs merge - tomato
  (set-face-attribute 'neo-vc-unlocked-changes-face nil :foreground "#FFD700") ;; Unlocked changes - gold
  (set-face-attribute 'neo-vc-added-face nil :foreground "#32CD32")    ;; Added - lime green
  (set-face-attribute 'neo-vc-removed-face nil :foreground "#FF4500")  ;; Removed - red orange
  (set-face-attribute 'neo-vc-conflict-face nil :foreground "#FF0000") ;; Conflict - red
  (set-face-attribute 'neo-vc-missing-face nil :foreground "#8B0000")  ;; Missing - dark red
  (set-face-attribute 'neo-vc-ignored-face nil :foreground "#696969")  ;; Ignored - dim gray
  
  ;; Auto refresh when files change
  (setq neo-auto-indent-point t)
  (setq neo-modern-sidebar t)
  (setq neo-window-fixed-size nil)
  
  ;; Key bindings within neotree
  (define-key neotree-mode-map (kbd "TAB") 'neotree-quick-look)
  (define-key neotree-mode-map (kbd "q") 'neotree-hide)
  (define-key neotree-mode-map (kbd "RET") 'neotree-enter)
  (define-key neotree-mode-map (kbd "g") 'neotree-refresh)
  (define-key neotree-mode-map (kbd "n") 'neotree-next-line)
  (define-key neotree-mode-map (kbd "p") 'neotree-previous-line)
  (define-key neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (define-key neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

;; === ICON INTEGRATION - VISUAL ENHANCEMENT ===
;; All-the-icons provides beautiful icons for enhanced visual appearance
;; only in graphical mode for optimal performance
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  ;; Auto-install fonts if not already installed
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))
  
  ;; Enhanced icon configurations
  (setq all-the-icons-color-icons t)
  (setq all-the-icons-for-buffer t)
  
  ;; Set icon scale for better appearance
  (setq all-the-icons-scale-factor 1.0))

;; === NERD-ICONS - MODERN ICON SUPPORT ===
;; Nerd-icons provides modern icon support and better performance
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p)
  :config
  ;; Auto-install fonts if not already installed
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t)))

;; === ALL-THE-ICONS DIRED - ICONS IN DIRED ===
;; Add beautiful icons to dired file manager
(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; === NERD-ICONS DIRED - MODERN DIRED ICONS ===
;; Modern alternative to all-the-icons-dired
(use-package nerd-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode . nerd-icons-dired-mode))

;; === LINUM MODE COMPATIBILITY ===
;; Ensure linum-mode is available for neotree compatibility
(when (< emacs-major-version 26)
  (require 'linum nil t))

;; === NEOTREE VISUAL EFFECTS - ENHANCED EXPERIENCE ===
;; Add smooth visual effects and animations
(defun my/neotree-startup-hook ()
  "Custom startup configuration for Neotree."
  (setq-local line-spacing 0.2)  ;; Add line spacing for better readability
  (setq-local cursor-type nil)   ;; Hide cursor in neotree
  (hl-line-mode 1))              ;; Highlight current line

(add-hook 'neotree-mode-hook 'my/neotree-startup-hook)

;; Auto-resize neotree when opening files
(defun my/neotree-resize-when-idle ()
  "Auto-resize neotree when idle."
  (when (neo-global--window-exists-p)
    (save-selected-window
      (neo-global--select-window)
      (let ((fit-window-to-buffer-horizontally t))
        (shrink-window-if-larger-than-buffer)))))

;; Enable smooth scrolling in neotree
(add-hook 'neotree-mode-hook 
          (lambda ()
            (setq-local scroll-margin 0)
            (setq-local scroll-conservatively 0)))

;; === NEOTREE KEYBINDINGS - QUICK ACCESS ===
;; Configure keybindings for Neotree file navigation
(global-set-key (kbd "C-x t") 'neotree-toggle)    ;; Toggle Neotree
(global-set-key (kbd "C-x C-n") 'neotree-find)    ;; Find file in Neotree
(global-set-key (kbd "<f12>") 'neotree-toggle)    ;; F12 toggle
(global-set-key (kbd "C-c t") 'neotree-dir)       ;; Open neotree in directory

;; ============================================================================
;; 🗂️ PROFESSIONAL FILE MANAGEMENT - ADVANCED NAVIGATION
;; ============================================================================

;; === TREEMACS - PROFESSIONAL FILE EXPLORER ===
;; Treemacs provides professional file management capabilities
;; with advanced features and LSP integration
(use-package treemacs
  :config)

;; === LSP TREEMACS - INTELLIGENT INTEGRATION ===
;; LSP Treemacs integrates Language Server Protocol features
;; with Treemacs for enhanced development experience
(use-package lsp-treemacs
  :after (lsp-mode treemacs))

;; ============================================================================
;; 🎯 PROVIDE & FINALIZATION
;; ============================================================================

;; Provide the utils module
(provide 'utils)

;; Display success message when module is loaded
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'utils)
              (message "🛠️  Utilities: Professional Tools Ready")
              (message "🚀 Advanced Navigation: Avy Jump System Active")
              (message "🖥️  Terminal Integration: VTerm Ready")
              (message "⌨️  Keybinding Hints: Which-key Active")
              (message "🌳 File Tree Navigation: Neotree Configured")
              (message "🗂️  Professional File Management: Treemacs Ready")
              (message "✨ Visual Enhancement: Icons Integrated"))))

;;; utils.el ends here
