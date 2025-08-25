;;; editing.el --- Podstawowe narzędzia edycji -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, editing, basic, tools, formatting
;; Description: Podstawowe narzędzia edycji i formatowania kodu
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Ten moduł zawiera podstawowe narzędzia edycji:
;; - Formatowanie kodu
;; - Podstawowe narzędzia edycji
;; - Hooks i konfiguracja edycji
;; - Narzędzia jakości kodu

;;; Code:

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
          (lua-mode . stylua)
          (elixir-mode . mix-format)
          (scala-mode . scalafmt)
          (haskell-mode . ormolu)
          (ocaml-mode . ocamlformat)
          (reason-mode . refmt)
          (clojure-mode . cljfmt)
          (fennel-mode . fnlfmt)
          (zig-mode . zig-fmt)
          (nim-mode . nimpretty)
          (crystal-mode . crystal-tool-format)
          (odin-mode . odinfmt)
          (gleam-mode . gleam-format)
          (v-mode . v-fmt)
          (carbon-mode . clang-format))) ;; Carbon używa clang-format

  ;; Włącz globalne formatowanie przy zapisie
  (apheleia-global-mode +1))

;; ============================================================================
;; 🔧 PODSTAWOWE NARZĘDZIA EDYCJI
;; ============================================================================

;; === Multiple Cursors - Wielokrotne kursory ===
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; === Expand Region - Rozszerzanie regionu ===
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; === Move Text - Przenoszenie tekstu ===
(use-package move-text
  :ensure t
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

;; === Drag Stuff - Przeciąganie tekstu ===
(use-package drag-stuff
  :ensure t
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

;; === Duplicate Line - Duplikowanie linii ===
;; Funkcja do duplikowania linii (wbudowana w Emacs)
(defun duplicate-line ()
  "Duplikuj aktualną linię."
  (interactive)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (move-end-of-line 1)
    (newline)
    (insert line)))
(global-set-key (kbd "C-c d") 'duplicate-line)

;; ============================================================================
;; 📝 TRYBY EDYCJI I KONFIGURACJA
;; ============================================================================

;; === Electric Pair - Automatyczne nawiasy ===
(electric-pair-mode 1)

;; === Electric Indent - Automatyczne wcięcia ===
(electric-indent-mode 1)

;; === Delete Selection - Zastąp zaznaczenie ===
(delete-selection-mode 1)

;; === Transient Mark - Tymczasowe zaznaczenie ===
(transient-mark-mode 1)

;; === Show Paren - Pokaż nawiasy ===
(show-paren-mode 1)

;; === Line Number - Numery linii ===
(global-display-line-numbers-mode 1)

;; === Highlight Line - Podświetl linię ===
(global-hl-line-mode 1)

;; ============================================================================
;; 🎯 DOSTARCZENIE I FINALIZACJA
;; ============================================================================

;; Dostarcz moduł edycji
(provide 'editing)

;; Wyświetl komunikat sukcesu po załadowaniu modułu
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'editing)
              (message "✏️ Podstawowe narzędzia edycji: Gotowe")
              (message "🎨 Formatowanie kodu: Apheleia")
              (message "🔧 Wielokursory i rozszerzenia: Aktywne")
              (message "📝 Tryby edycji: Włączone"))))

;;; editing.el ends here

