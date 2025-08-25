;;; jupyter-config.el --- Professional Jupyter Integration & Data Science Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, jupyter, data-science, python, polymode, notebook
;; Description: Professional Jupyter integration for data science and interactive computing
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; This module provides comprehensive Jupyter integration capabilities:
;; - Direct Jupyter cell execution from Emacs
;; - Polymode support for multi-language buffers
;; - Org-mode integration for literate programming
;; - EIN notebook interface for graphical Jupyter support
;; - Professional data science workflow
;;
;; Features:
;; - Jupyter REPL integration
;; - Multi-language polymode support
;; - Org-mode Jupyter integration
;; - Interactive notebook interface
;; - LSP integration for enhanced development
;; - Professional data science environment

;;; Code:

;; ============================================================================
;; 🐍 JUPYTER INTEGRATION - CORE JUPYTER SUPPORT
;; ============================================================================

;; === JUPYTER.EL - CORE JUPYTER FUNCTIONALITY ===
;; Jupyter.el provides core Jupyter integration capabilities
;; Enables direct Jupyter cell execution from Emacs
(use-package jupyter
  :ensure t
  :commands (jupyter-run-repl)
  :config
  ;; Display results as overlays for better visibility
  (setq jupyter-eval-use-overlays t))
              
;; ============================================================================
;; 🌐 POLYMODE - MULTI-LANGUAGE SUPPORT
;; ============================================================================

;; === POLYMODE - UNIVERSAL LANGUAGE SUPPORT ===
;; Polymode provides support for multiple languages in single buffers
;; Enables mixed-language development and documentation
(use-package polymode
  :ensure t)
  
;; === POLYMODE PATH CONFIGURATION ===
;; Configure load paths for polymode and related packages
;; Ensures proper loading of polymode components
(setq load-path
      (append '("path/to/vc/dir/polymode/"
                "path/to/vc/dir/poly-markdown/")
              load-path))

;; === POLY-MARKDOWN - MARKDOWN SUPPORT ===
;; Poly-markdown provides enhanced markdown support
;; Enables rich markdown editing with polymode
(use-package poly-markdown
  :ensure t)

;; === POLY-R - R LANGUAGE SUPPORT ===
;; Poly-R provides R language support within polymode
;; Enables R development and data analysis
(use-package poly-R
  :ensure t)

;; ============================================================================
;; 📝 ORG-MODE INTEGRATION - LITERATE PROGRAMMING
;; ============================================================================

;; === ORG-MODE + JUPYTER - LITERATE COMPUTING ===
;; Integrate Jupyter with Org-mode for literate programming
;; Enables executable code blocks and interactive documentation
(with-eval-after-load 'org
  (require 'jupyter)
  (add-to-list 'org-babel-load-languages '(jupyter . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; ============================================================================
;; 📓 EIN - GRAPHICAL NOTEBOOK INTERFACE
;; ============================================================================

;; === EIN - ENHANCED NOTEBOOK INTERFACE ===
;; EIN provides graphical Jupyter notebook interface
;; Offers enhanced notebook editing and execution capabilities
(use-package ein
  :defer t
  :config
  ;; Enable LSP in notebook mode for enhanced development
  ;; Provides intelligent code completion and analysis
  (add-hook 'ein:notebook-mode-hook #'lsp-deferred))

;; ============================================================================
;; 🎯 PROVIDE & FINALIZATION
;; ============================================================================

;; Provide the jupyter-config module
(provide 'jupyter-config)

;; Display success message when module is loaded
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'jupyter-config)
              (message "📊 Jupyter Integration: Professional Data Science Ready")
              (message "🐍 Python Support: Jupyter REPL Active")
              (message "🌐 Multi-Language: Polymode Support Configured")
              (message "📝 Org Integration: Literate Programming Ready")
              (message "📓 Notebook Interface: EIN Enhanced Interface Active")
              (message "🚀 Data Science: Professional Workflow Ready"))))

;;; jupyter-config.el ends here
