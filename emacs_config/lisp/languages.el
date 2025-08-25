;;; languages.el --- Wsparcie dla języków programowania -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, languages, lsp, programming, modes
;; Description: Kompleksowe wsparcie dla języków programowania z LSP
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Ten moduł zapewnia wsparcie dla różnych języków programowania:
;; - Tryby językowe (Python, Go, Rust, JavaScript, C/C++, etc.)
;; - Konfiguracja LSP dla każdego języka
;; - Specjalne ustawienia i hooks
;; - Integracja z środowiskami (Conda, etc.)

;;; Code:

;; ============================================================================
;; 🌐 LANGUAGE SERVER PROTOCOL - INTELIGENTNE WSPARCIE PROGRAMOWANIA
;; ============================================================================

;; === LSP Mode - Serwer językowy podpowiedzi ===
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (go-mode . lsp)
         (js-mode . lsp)
         (java-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (yaml-mode . lsp)
         (bash-mode . lsp)
         (markdown-mode . lsp)
         (web-mode . lsp))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable t)      ;; Pasek nawigacji
  (setq lsp-enable-symbol-highlighting t)       ;; Podświetlanie symboli
  (setq lsp-enable-on-type-formatting nil))     ;; Formatowanie przy pisaniu

;; === LSP UI - Dokumentacja, tooltips, boczne podpowiedzi ===
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)                    ;; Dokumentacja
  (setq lsp-ui-doc-show-with-cursor t)          ;; Pokaż z kursorem
  (setq lsp-ui-doc-delay 0.2)                   ;; Opóźnienie dokumentacji
  (setq lsp-ui-doc-position 'at-point)          ;; Pozycja dokumentacji
  (setq lsp-ui-doc-max-height 30)               ;; Maksymalna wysokość
  (setq lsp-ui-sideline-enable t)               ;; Boczne podpowiedzi
  (setq lsp-ui-sideline-show-hover t)           ;; Pokaż hover
  (setq lsp-ui-sideline-show-code-actions t))   ;; Pokaż akcje kodu
  
;; === LSP Pyright - Serwer języka Python ===
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; ============================================================================
;; 🐍 PYTHON - SPECJALNE WSPARCIE I KONFIGURACJA
;; ============================================================================

;; === Pyvenv - Wirtualne środowiska Python ===
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)) 

;; === Isortify - Sortowanie importów ===
(use-package isortify
  :ensure t
  :hook (python-mode . isortify-mode))

;; === Conda - Automatyczne aktywowanie środowisk ===
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t))

;; === Ustawienia Python - Hooks i konfiguracja ===
(add-hook 'python-mode-hook #'font-lock-mode)
(add-hook 'python-mode-hook #'display-line-numbers-mode)

;; ============================================================================
;; 🌍 RÓŻNORODNE TRYBY JĘZYKOWE
;; ============================================================================

;; === Go Mode - Wsparcie dla języka Go ===
(use-package go-mode
  :ensure t
  :hook (before-save . gofmt-before-save))      ;; LSP dla Go przez lsp-mode

;; === Web Mode - Wsparcie dla HTML/CSS/JS ===
(use-package web-mode 
  :ensure t
  :mode "\\.html?\\'")

;; === YAML Mode - Wsparcie dla YAML ===
(use-package yaml-mode 
  :ensure t
  :mode "\\.ya?ml\\'")

;; === Bash Completion - Uzupełnianie Bash ===
(use-package bash-completion
  :ensure t)

;; === Haskell Mode - Wsparcie dla Haskell ===
(use-package haskell-mode
  :ensure t)

;; === Clojure Mode - Wsparcie dla Clojure ===
(use-package clojure-mode
  :ensure t)

;; === Rust Mode - Wsparcie dla Rust ===
(use-package rust-mode
  :ensure t)

;; === TypeScript Mode - Wsparcie dla TypeScript ===
(use-package typescript-mode
  :ensure t)

;; === JSON Mode - Wsparcie dla JSON ===
(use-package json-mode
  :ensure t)

;; === Lua Mode - Wsparcie dla Lua ===
(use-package lua-mode
  :ensure t)

;; === Ruby Mode - Wsparcie dla Ruby ===
(use-package ruby-mode
  :ensure t)

;; === Java Mode - Wsparcie dla Java ===
;; Java mode jest wbudowany w Emacs
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

;; === C# Mode - Wsparcie dla C# ===
(use-package csharp-mode
  :ensure t)

;; === PHP Mode - Wsparcie dla PHP ===
(use-package php-mode
  :ensure t)

;; === Scala Mode - Wsparcie dla Scala ===
(use-package scala-mode
  :ensure t)

;; === Kotlin Mode - Wsparcie dla Kotlin ===
(use-package kotlin-mode
  :ensure t)

;; === Dart Mode - Wsparcie dla Dart ===
(use-package dart-mode
  :ensure t)

;; === Swift Mode - Wsparcie dla Swift ===
(use-package swift-mode
  :ensure t)

;; === R Mode - Wsparcie dla R ===
(use-package ess
  :ensure t)

;; === Julia Mode - Wsparcie dla Julia ===
(use-package julia-mode
  :ensure t)

;; === Zig Mode - Wsparcie dla Zig ===
(use-package zig-mode
  :ensure t)

;; === Nim Mode - Wsparcie dla Nim ===
(use-package nim-mode
  :ensure t)

;; === Crystal Mode - Wsparcie dla Crystal ===
(use-package crystal-mode
  :ensure t)

;; === Odin Mode - Wsparcie dla Odin ===
;; Odin mode - dostępny w Melpa
;; (use-package odin-mode
;;   :ensure t
;;   :mode "\\.odin\\'")

;; === Gleam Mode - Wsparcie dla Gleam ===
;; Gleam mode - dostępny w Melpa
;; (use-package gleam-mode
;;   :ensure t
;;   :mode "\\.gleam\\'")

;; === V Mode - Wsparcie dla V ===
;; V mode - dostępny w Melpa
;; (use-package v-mode
;;   :ensure t)

;; === Carbon Mode - Wsparcie dla Carbon (przez C++ mode) ===
;; Carbon używa C++ mode z clangd LSP

;; ============================================================================
;; 🎯 DOSTARCZENIE I FINALIZACJA
;; ============================================================================

;; Dostarcz moduł języków
(provide 'languages)

;; Wyświetl komunikat sukcesu po załadowaniu modułu
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'languages)
              (message "🌍 Wsparcie dla języków programowania: Gotowe")
              (message "🌐 LSP Mode: Aktywny")
              (message "🐍 Python: Conda + Pyright")
              (message "🚀 Go, Rust, JavaScript: LSP")
              (message "✨ Wszystkie tryby językowe: Załadowane"))))

;;; languages.el ends here
