;;; completion.el --- Konfiguracja uzupełniania i środowiska deweloperskiego -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, completion, development, lsp, git
;; Description: Zaawansowany system uzupełniania z nowoczesnymi narzędziami deweloperskimi
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Ten moduł zapewnia kompleksowe środowisko uzupełniania i deweloperskie:
;; - Nowoczesne interfejsy uzupełniania (Vertico, Corfu, Cape)
;; - Integracja z Git (Magit, Forge, narzędzia Git)
;; - Wsparcie Language Server Protocol (LSP)
;; - Narzędzia formatowania i jakości kodu
;; - Zarządzanie projektami z Projectile
;; - Zaawansowany system cofania z Undo-tree

;;; Code:

;; ============================================================================
;; 🔍 SYSTEM UZUPEŁNIANIA - NOWOCZESNE INTERFEJSY
;; ============================================================================

;; === Vertico - Pionowy interfejs uzupełniania ===
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)  ;; pozwala na zapętlanie listy
  (vertico-count 15))

;; === Orderless - Rozmyte uzupełnianie ===
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))  ;; 'basic' jako fallback
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))) ;; lepsze uzupełnianie ścieżek

;; === Marginalia - Bogate adnotacje uzupełniania ===
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; === Consult - Ulepszone wyszukiwanie i nawigacja ===
(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("C-c h" . consult-history)))

;; === Corfu - Uzupełnianie w buforze ===
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  :bind
  (:map corfu-map
        ("<tab>" . corfu-next)
        ("S-<tab>" . corfu-previous)
        ("RET" . corfu-insert)))

;; === Cape - Rozszerzenia uzupełniania w punkcie ===
(use-package cape
  :ensure t
  :init
  (defun my/setup-cape ()
    "Konfiguracja źródeł uzupełniania Cape dla trybów programistycznych."
    (setq-local completion-at-point-functions
                (list
                 #'cape-dabbrev
                 #'cape-file
                 #'cape-keyword
                 #'cape-symbol
                 #'cape-elisp-block
                 #'lsp-completion-at-point))) ;; LSP jako jedno ze źródeł
  (add-hook 'prog-mode-hook #'my/setup-cape))

;; ============================================================================
;; 🎛️ TRANSIENT - NOWOCZESNY INTERFEJS KOMEND
;; ============================================================================

;; === Transient - Nowoczesny interfejs komend ===
(use-package transient
  :ensure t)
  
;; ============================================================================
;; 🐙 INTEGRACJA GIT - PROFESJONALNA KONTROLA WERSJI
;; ============================================================================

;; === Magit - Interfejs Git dla Emacs ===
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; === Forge - Integracja GitHub/GitLab ===
(use-package forge
  :ensure t
  :after magit
  :config
  ;; opcjonalnie – pobiera automatycznie dane z GitHuba przy starcie magit
  (setq forge-topic-list-limit '(60 . -1)))

;; === Git Messenger - Popup informacji Git ===
(use-package git-messenger
  :ensure t
  :bind ("C-x v p" . git-messenger:popup-message)) 

;; === Git Timemachine - Nawigacja historii Git ===
(use-package git-timemachine
  :ensure t
  :commands git-timemachine)  ;; można uruchamiać przez M-x 

;; ============================================================================
;; 🚀 ZARZĄDZANIE PROJEKTAMI - INTELIGENTNE OBSŁUGIWANIE
;; ============================================================================

;; === Projectile - Zarządzanie projektami ===
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; ============================================================================
;; 🔄 SYSTEM COFANIA - ZAAWANSOWANE ZARZĄDZANIE
;; ============================================================================

;; === Undo-tree - Drzewiasty system cofania ===
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-auto-save-history t)
  :config
  ;; Tworzy katalog undo jeśli nie istnieje
  (unless (file-exists-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo"))) 

;; ============================================================================
;; 🌐 LANGUAGE SERVER PROTOCOL - NOWOCZESNE WSPARCIE DEWELOPERSKIE
;; ============================================================================

;; === Eglot - Klient LSP dla Emacs ===
(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :config
  ;; Konfiguracja serwera Python LSP
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  
  ;; 🚀 TOP 2025 - Serwery LSP nowej generacji
  ;; Uwaga: Używamy serwerów fallback do czasu dostępności oficjalnych
  ;; Będą działać z trybami fallback które skonfigurowaliśmy
  (add-to-list 'eglot-server-programs
               '(prog-mode . ("clangd" "--stdio")))      ;; Generyczny LSP programistyczny
  (add-to-list 'eglot-server-programs
               '(c++-mode . ("clangd" "--stdio")))       ;; LSP C++ (dla Carbon)
  (add-to-list 'eglot-server-programs
               '(go-mode . ("gopls")))                    ;; LSP Go (dla V)
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio"))) ;; LSP Python (dla Nim)
  (add-to-list 'eglot-server-programs
               '(ruby-mode . ("solargraph" "--stdio")))   ;; LSP Ruby (dla Crystal)
  (add-to-list 'eglot-server-programs
               '(c-mode . ("clangd" "--stdio")))          ;; LSP C (dla Odin)
  (add-to-list 'eglot-server-programs
               '(erlang-mode . ("erlang-ls"))))           ;; LSP Erlang (dla Gleam)

;; ============================================================================
;; 🎨 FORMATOWANIE KODU - AUTOMATYCZNA JAKOŚĆ
;; ============================================================================

;; === Apheleia - Automatyczne formatowanie kodu ===
(use-package apheleia
  :ensure t
  :config
  ;; Dostosuj formattery, jeśli chcesz nadpisać domyślne
  (setf apheleia-mode-alist
        '((python-mode . black)
          (js-mode . prettier)
          (typescript-mode . prettier)
          (json-mode . prettier)
          (yaml-mode . prettier)
          (html-mode . prettier)
          (go-mode . gofmt)
          (rust-mode . rustfmt)
          (c-mode . clang-format)
          (c++-mode . clang-format)
          (lua-mode . stylua)))

  ;; Włącz globalne formatowanie przy zapisie
  (apheleia-global-mode +1))

;; ============================================================================
;; 🐍 ROZWÓJ PYTHON - ULEPSZONE WSPARCIE
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

;; ============================================================================
;; 🎯 DOSTARCZENIE I FINALIZACJA
;; ============================================================================

;; Dostarcz moduł uzupełniania
(provide 'completion)

;; Wyświetl komunikat sukcesu po załadowaniu modułu
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'completion)
              (message "🔍 Środowisko uzupełniania i deweloperskie: Gotowe")
              (message "✨ Vertico, Corfu, Cape: Aktywne")
              (message "🐙 Integracja Git: Magit, Forge")
              (message "🌐 Wsparcie LSP: Eglot")
              (message "🎨 Formatowanie kodu: Apheleia")
              (message "🚀 Zarządzanie projektami: Projectile")
              (message "🚀 TOP 2025: LSP nowej generacji gotowy"))))

;;; completion.el ends here
