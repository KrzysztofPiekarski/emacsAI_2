;;; keybindings.el --- Professional Keybindings & Interface Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, keybindings, interface, productivity, shortcuts
;; Description: Professional keybindings and interface configuration for enhanced productivity
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; This module provides comprehensive keybindings and interface configuration:
;; - Basic Emacs interface setup and customization
;; - Professional keybindings for productivity
;; - Buffer and window management shortcuts
;; - LSP integration keybindings
;; - Search and navigation enhancements
;; - Translation tool shortcuts
;; - File and buffer operations
;;
;; Features:
;; - Clean, professional interface appearance
;; - Intuitive keybindings for common operations
;; - Enhanced buffer and window management
;; - LSP integration for development
;; - Efficient search and navigation
;; - Translation tool integration
;; - Professional workflow optimization

;;; Code:

;; ============================================================================
;; 🎨 BASIC INTERFACE - CLEAN & PROFESSIONAL APPEARANCE
;; ============================================================================

;; === INTERFACE CUSTOMIZATION ===
;; Configure basic Emacs interface for professional appearance
;; and optimal user experience
(setq inhibit-startup-message t)                    ;; Disable startup message
(menu-bar-mode -1)                                  ;; Hide menu bar
(tool-bar-mode -1)                                  ;; Hide tool bar
(scroll-bar-mode -1)                                ;; Hide scroll bar
(global-display-line-numbers-mode t)                ;; Show line numbers
(global-font-lock-mode 1)                           ;; Enable syntax highlighting
(set-frame-font "Fira Code Retina 12" nil t)       ;; Set professional font

;; ============================================================================
;; ⌨️ PRODUCTIVITY SHORTCUTS - ESSENTIAL OPERATIONS
;; ============================================================================

;; === ESSENTIAL PRODUCTIVITY SHORTCUTS ===
;; Core shortcuts for daily productivity and workflow optimization
(global-set-key (kbd "C-x C-b") 'ibuffer)          ;; Enhanced buffer list
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Escape key functionality

;; === BUFFER NAVIGATION - EFFICIENT BUFFER MANAGEMENT ===
;; Quick navigation between buffers for seamless workflow
(global-set-key (kbd "C-x C-p") 'previous-buffer)  ;; Previous buffer
(global-set-key (kbd "C-x C-n") 'next-buffer)      ;; Next buffer

;; === FILE & BUFFER OPERATIONS - DATA MANAGEMENT ===
;; Essential operations for file and buffer management
(global-set-key (kbd "C-x C-s") 'save-buffer)      ;; Save buffer
(global-set-key (kbd "C-x k") 'kill-buffer)        ;; Kill buffer

;; ============================================================================
;; 🪟 WINDOW MANAGEMENT - FLEXIBLE LAYOUT CONTROL
;; ============================================================================

;; === WINDOW SPLITTING - LAYOUT CONTROL ===
;; Window splitting and management for multi-tasking
(global-set-key (kbd "C-x 2") 'split-window-below)     ;; Split horizontally
(global-set-key (kbd "C-x 3") 'split-window-right)    ;; Split vertically
(global-set-key (kbd "C-x 1") 'delete-other-windows)  ;; Maximize current window

;; ============================================================================
;; 🔍 LSP INTEGRATION - INTELLIGENT DEVELOPMENT
;; ============================================================================

;; === LSP NAVIGATION - CODE INTELLIGENCE ===
;; Language Server Protocol integration for intelligent code navigation
(global-set-key (kbd "M-.") 'lsp-find-definition)  ;; Go to definition

;; ============================================================================
;; 🔎 SEARCH & NAVIGATION - EFFICIENT FINDING
;; ============================================================================

;; === INCREMENTAL SEARCH - FAST TEXT FINDING ===
;; Quick search capabilities for efficient text navigation
(global-set-key (kbd "C-s") 'isearch-forward)      ;; Forward search
(global-set-key (kbd "C-r") 'isearch-backward)     ;; Backward search

;; === WINDOW NAVIGATION - QUICK WINDOW MOVEMENT ===
;; Efficient navigation between windows using arrow keys
(global-set-key (kbd "M-<left>") 'windmove-left)   ;; Move to left window
(global-set-key (kbd "M-<right>") 'windmove-right) ;; Move to right window
(global-set-key (kbd "M-<up>") 'windmove-up)       ;; Move to upper window
(global-set-key (kbd "M-<down>") 'windmove-down)   ;; Move to lower window

;; ============================================================================
;; 📁 FILE & BUFFER OPERATIONS - WORKFLOW OPTIMIZATION
;; ============================================================================

;; === FILE OPERATIONS - QUICK ACCESS ===
;; Essential file operations for daily workflow
(global-set-key (kbd "C-x C-f") 'find-file)        ;; Find file
(global-set-key (kbd "C-c b") 'switch-to-buffer)   ;; Switch buffer

;; ============================================================================
;; 💾 BACKUP & SAFETY - DATA PROTECTION
;; ============================================================================

;; === BACKUP CONFIGURATION - DATA SAFETY ===
;; Configure backup and safety features for data protection
(setq backup-directory-alist `(("." . "~/.saves"))) ;; Backup directory
(setq auto-save-default nil)                        ;; Disable auto-save
(delete-selection-mode 1)                           ;; Enable delete selection

;; ============================================================================
;; 🌐 TRANSLATION TOOLS - MULTI-LANGUAGE SUPPORT
;; ============================================================================

;; === TRANSLATION SHORTCUTS - LANGUAGE SUPPORT ===
;; Quick access to translation tools for multi-language development
;; and documentation
(global-set-key (kbd "C-c t r") 'translator/translate-region)            ;; Polish → English
(global-set-key (kbd "C-c t p") 'translator/translate-region-en-to-pl)   ;; English → Polish
(global-set-key (kbd "C-c t b") 'translator/translate-buffer-to-english) ;; Buffer Polish → English

;; ============================================================================
;; 🎯 PROVIDE & FINALIZATION
;; ============================================================================

;; Provide the keybindings module
(provide 'keybindings)

;; Display success message when module is loaded
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'keybindings)
              (message "⌨️  Keybindings: Professional Shortcuts Configured")
              (message "🎨 Interface: Clean & Professional Appearance")
              (message "🪟 Window Management: Flexible Layout Control")
              (message "🔍 LSP Integration: Intelligent Development Ready")
              (message "🔎 Search & Navigation: Efficient Finding Active")
              (message "📁 File Operations: Workflow Optimization Ready")
              (message "🌐 Translation Tools: Multi-language Support Active"))))

;;; keybindings.el ends here
