;;; org-mode.el --- Professional Org Mode & Productivity Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, org-mode, productivity, note-taking, presentations
;; Description: Professional Org Mode configuration with enhanced productivity features
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; This module provides a comprehensive Org Mode configuration:
;; - Core Org Mode setup and customization
;; - Org Roam for knowledge management
;; - Modern Org appearance with org-modern
;; - Presentation capabilities with org-present
;; - File and image management with org-download
;; - Enhanced bullets and visual elements
;; - PDF tools integration for document handling
;;
;; Features:
;; - Professional note-taking and organization
;; - Knowledge management with Org Roam
;; - Beautiful modern appearance
;; - Presentation mode for meetings and demos
;; - File and image management
;; - Enhanced visual elements and bullets
;; - PDF document support

;;; Code:

;; ============================================================================
;; 📝 CORE ORG MODE - PROFESSIONAL NOTE-TAKING SYSTEM
;; ============================================================================

;; === ORG MODE - CORE CONFIGURATION ===
;; Org Mode is the heart of Emacs productivity
;; providing powerful note-taking, organization, and task management
(use-package org
  :ensure t
  :init
  (setq org-directory "~/org")                    ;; Set Org directory
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))             ;; Create directory if it doesn't exist
  :config
  ;; Additional configuration can be added here as needed
  )

;; ============================================================================
;; 🧠 ORG ROAM - KNOWLEDGE MANAGEMENT SYSTEM
;; ============================================================================

;; === ORG ROAM - KNOWLEDGE MANAGEMENT ===
;; Org Roam provides a powerful knowledge management system
;; with bidirectional linking and automatic database synchronization
(use-package org-roam
  :ensure t
  :after org
  :init
  (let ((dir (expand-file-name "org-roam" org-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))                     ;; Create org-roam directory
    (setq org-roam-directory dir))
  :config
  (org-roam-db-autosync-mode))                   ;; Enable automatic database sync

;; ============================================================================
;; ✨ ORG MODERN - BEAUTIFUL VISUAL APPEARANCE
;; ============================================================================

;; === ORG MODERN - MODERN VISUAL STYLE ===
;; Org Modern provides beautiful, modern visual enhancements
;; for Org Mode with enhanced bullets, tables, and formatting
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "✸" "✿"))          ;; Custom star symbols
  (org-modern-table t)                           ;; Enable modern tables
  (org-modern-list '((?- . "•") (?+ . "‣") (?* . "◆"))) ;; Custom list markers
  (org-modern-block-fringe nil)                  ;; Disable block fringe
  (org-modern-todo nil)                          ;; Disable modern todo
  (setq-default line-spacing 0.2)                ;; Enhanced line spacing
  (set-face-attribute 'default nil :family "Fira Code Retina" :height 120)) ;; Custom font

;; ============================================================================
;; 🎭 ORG PRESENT - PROFESSIONAL PRESENTATIONS
;; ============================================================================

;; === ORG PRESENT - PRESENTATION MODE ===
;; Org Present provides minimalist presentation capabilities
;; directly within Emacs for meetings and demonstrations
(use-package org-present
  :ensure t 
  :hook ((org-present-mode . org-present-setup)
         (org-present-mode-quit . org-present-teardown))
  :config
  ;; Helper function for presentation setup
  (defun org-present-setup ()
    "Configure presentation mode for optimal viewing experience"
    (org-display-inline-images)                   ;; Show inline images
    (org-present-big)                            ;; Enlarge text
    (org-hide-leading-stars)                     ;; Hide leading stars
    (org-present-hide-cursor)                    ;; Hide cursor
    (org-present-read-only))                     ;; Make read-only

  ;; Helper function for presentation cleanup
  (defun org-present-teardown ()
    "Clean up after presentation mode"
    (org-remove-inline-images)                   ;; Remove inline images
    (org-present-small)                          ;; Restore normal text size
    (org-show-leading-stars)                     ;; Show leading stars
    (org-present-show-cursor)                    ;; Show cursor
    (org-present-read-write)))                   ;; Make editable

;; ============================================================================
;; 📁 ORG DOWNLOAD - FILE & IMAGE MANAGEMENT
;; ============================================================================

;; === ORG DOWNLOAD - FILE MANAGEMENT ===
;; Org Download provides seamless file and image management
;; for Org Mode with automatic organization and screenshot support
(use-package org-download
  :ensure t
  :after org
  :hook (org-mode . org-download-enable)
  :custom
  (org-download-method 'directory)               ;; Use directory method
  (org-download-image-dir (expand-file-name "images" org-directory)) ;; Image directory
  (org-download-heading-lvl nil)                 ;; No heading level restriction
  (org-download-screenshot-method "xfce4-screenshooter -r -o cat > %s")) ;; Screenshot method

;; ============================================================================
;; 🎯 ORG BULLETS - ENHANCED VISUAL ELEMENTS
;; ============================================================================

;; === ORG BULLETS - ENHANCED BULLETS ===
;; Org Bullets provides enhanced bullet points and visual elements
;; for better organization and readability
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))) ;; Enable in Org mode

;; ============================================================================
;; 📄 PDF TOOLS - DOCUMENT HANDLING
;; ============================================================================

;; === PDF TOOLS - PDF DOCUMENT SUPPORT ===
;; PDF Tools provides comprehensive PDF viewing and manipulation
;; capabilities directly within Emacs
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))                           ;; Install PDF tools

;; Disable line numbers in PDF view mode for better appearance
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

;; ============================================================================
;; 🎯 PROVIDE & FINALIZATION
;; ============================================================================

;; Provide the org-mode module
(provide 'org-mode)

;; Display success message when module is loaded
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'org-mode)
              (message "📝 Org Mode: Professional Note-Taking System Ready")
              (message "🧠 Knowledge Management: Org Roam Active")
              (message "✨ Visual Enhancement: Org Modern Configured")
              (message "🎭 Presentation Mode: Org Present Ready")
              (message "📁 File Management: Org Download Active")
              (message "🎯 Visual Elements: Org Bullets Enhanced")
              (message "📄 Document Support: PDF Tools Integrated"))))

;;; org-mode.el ends here
