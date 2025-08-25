;;; development.el --- Narzędzia deweloperskie i debugowanie -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, development, debugging, dap, linting, flycheck
;; Description: Zaawansowane narzędzia deweloperskie z debugowaniem i lintowaniem
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Ten moduł zapewnia narzędzia deweloperskie:
;; - Debugowanie z DAP (Debug Adapter Protocol)
;; - Lintowanie i sprawdzanie jakości kodu
;; - Narzędzia analizy kodu
;; - Profilowanie i optymalizacja

;;; Code:

;; ============================================================================
;; 🔍 SPRAWDZANIE JAKOŚCI - LINTING I ANALIZA KODU
;; ============================================================================

;; === Flycheck - Lintowanie i sprawdzanie błędów ===
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)) 

;; === Flycheck Inline - Inline błędy ===
(use-package flycheck-inline
  :ensure t
  :after flycheck
  :config
  (global-flycheck-inline-mode))

;; === Flycheck Popup - Popup z błędami ===
(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-popup-tip-mode))

;; ============================================================================
;; 🐛 DEBUGOWANIE - ZAAWANSOWANE NARZĘDZIA DEBUGOWANIA
;; ============================================================================

;; === DAP Mode - Debugowanie z LSP (Python) ===
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable
        (or (when (getenv "CONDA_PREFIX")
              (concat (getenv "CONDA_PREFIX") "/bin/python"))
            "python3")))

;; === DAP UI - Interfejs debugowania ===
;; DAP UI jest częścią dap-mode
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable
        (or (when (getenv "CONDA_PREFIX")
              (concat (getenv "CONDA_PREFIX") "/bin/python"))
            "python3")))

;; === DAP Hydra - Hydra dla debugowania ===
;; DAP Hydra jest częścią dap-mode
;; (use-package dap-hydra
;;   :ensure t
;;   :after dap-mode)

;; ============================================================================
;; 📊 ANALIZA KODU - PROFILOWANIE I OPTYMALIZACJA
;; ============================================================================

;; === Profiler - Profilowanie Emacs ===
(use-package profiler
  :commands (profiler-start profiler-stop profiler-report))

;; === Memory Report - Raport pamięci ===
(use-package memory-report
  :commands memory-report)

;; === Benchmark - Testowanie wydajności ===
(use-package benchmark-init
  :ensure t
  :config
  (benchmark-init/activate))

;; ============================================================================
;; 🎯 DOSTARCZENIE I FINALIZACJA
;; ============================================================================

;; Dostarcz moduł deweloperski
(provide 'development)

;; Wyświetl komunikat sukcesu po załadowaniu modułu
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'development)
              (message "🔍 Narzędzia deweloperskie: Gotowe")
              (message "🐛 Debugowanie DAP: Aktywne")
              (message "🔍 Lintowanie Flycheck: Aktywne")
              (message "📊 Profilowanie: Gotowe"))))

;;; development.el ends here
