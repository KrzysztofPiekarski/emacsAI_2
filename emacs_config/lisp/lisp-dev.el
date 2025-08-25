;;; lisp-dev.el --- Professional Lisp Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, lisp, development, emacs-lisp, common-lisp, repl
;; Description: Professional Lisp development environment with enhanced tools and REPL support
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; This module provides comprehensive Lisp development capabilities:
;; - Enhanced Emacs Lisp development environment
;; - Common Lisp support with SBCL integration
;; - Smart indentation and parentheses handling
;; - Advanced completion and documentation tools
;; - REPL integration for interactive development
;;
;; Features:
;; - Smart indentation with aggressive-indent
;; - Intelligent parentheses management
;; - Enhanced documentation with helpful
;; - Advanced completion with company mode
;; - Common Lisp REPL integration
;; - Professional development workflow

;;; Code:

;; ============================================================================
;; 🎯 SMART INDENTATION - INTELLIGENT CODE FORMATTING
;; ============================================================================

;; === PARINFER RUST MODE - ADVANCED INDENTATION ===
;; Parinfer provides intelligent indentation based on parentheses structure
;; Automatically downloads Rust binary for optimal performance
;; (use-package parinfer-rust-mode
;;   :hook ((emacs-lisp-mode lisp-mode) . parinfer-rust-mode)
;;   :config
;;   (setq parinfer-rust-auto-download t))

;; === AGGRESSIVE INDENTATION - AUTOMATIC FORMATTING ===
;; Aggressive indent provides automatic indentation for Lisp code
;; Ensures consistent and professional code formatting
(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-mode) . aggressive-indent-mode))

;; ============================================================================
;; 🧩 PARENTHESES MANAGEMENT - STRUCTURED EDITING
;; ============================================================================

;; === SMARTPARENS - INTELLIGENT PARENTHESES ===
;; Smartparens provides intelligent parentheses handling and balancing
;; Enhances structured editing for Lisp dialects
(use-package smartparens
  :hook ((emacs-lisp-mode lisp-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

;; === PAREDIT ALTERNATIVE - STRUCTURED EDITING ===
;; Paredit provides alternative structured editing capabilities
;; Choose between smartparens or paredit based on preference
;; (use-package paredit
;;   :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))

;; ============================================================================
;; 📚 DOCUMENTATION & HELP - ENHANCED LEARNING
;; ============================================================================

;; === HELPFUL - INTELLIGENT DOCUMENTATION ===
;; Helpful provides enhanced documentation and help system
;; Replaces default help functions with more informative versions
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)      ;; Enhanced function help
   ("C-h v" . helpful-variable)      ;; Enhanced variable help
   ("C-h k" . helpful-key)           ;; Enhanced key help
   ("C-h x" . helpful-command)))     ;; Enhanced command help

;; === ELISP-DEF - DEFINITION NAVIGATION ===
;; Elisp-def provides cross-reference and definition jumping
;; Enhances navigation in Emacs Lisp code
(use-package elisp-def
  :hook (emacs-lisp-mode . elisp-def-mode))

;; ============================================================================
;; 🎨 EMACS LISP ENHANCEMENTS - SPECIALIZED SUPPORT
;; ============================================================================

;; === EMACS LISP HOOK - CUSTOMIZED ENVIRONMENT ===
;; Customize Emacs Lisp mode with enhanced features
;; Provides optimized development environment for Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode 1)                           ;; Enable eldoc for documentation
            (setq-local completion-at-point-functions ;; Configure completion functions
                        (list #'cape-elisp-block      ;; Elisp block completion
                              #'cape-elisp-symbol     ;; Elisp symbol completion
                              #'cape-dabbrev          ;; Dynamic abbreviation completion
                              #'cape-keyword          ;; Keyword completion
                              #'cape-file))))         ;; File completion

;; ============================================================================
;; 🔍 COMPLETION ENGINE - INTELLIGENT SUGGESTIONS
;; ============================================================================

;; === COMPANY MODE - ADVANCED COMPLETION ===
;; Company mode provides intelligent completion for Lisp development
;; Enhances productivity with context-aware suggestions
(use-package company
  :hook ((emacs-lisp-mode lisp-mode) . company-mode))

;; ============================================================================
;; 🚀 COMMON LISP SUPPORT - PROFESSIONAL DEVELOPMENT
;; ============================================================================

;; === SLY - COMMON LISP INTEGRATION ===
;; SLY provides Common Lisp support with SBCL integration
;; Enables professional Common Lisp development workflow
(use-package sly
  :when (executable-find "sbcl")                    ;; Only load if SBCL is available
  :init
  (setq inferior-lisp-program "sbcl"))              ;; Set SBCL as Lisp implementation

;; ============================================================================
;; 🎯 PROVIDE & FINALIZATION
;; ============================================================================

;; Provide the lisp-dev module
(provide 'lisp-dev)

;; Display success message when module is loaded
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'lisp-dev)
              (message "🐍 Lisp Development: Professional Environment Ready")
              (message "🎯 Smart Indentation: Aggressive Indent Active")
              (message "🧩 Parentheses Management: Smartparens Configured")
              (message "📚 Documentation: Helpful System Active")
              (message "🔍 Completion Engine: Company Mode Ready")
              (message "🚀 Common Lisp: SLY Integration Available"))))

;;; lisp-dev.el ends here
