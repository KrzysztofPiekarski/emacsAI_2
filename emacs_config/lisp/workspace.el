;;; workspace.el --- Zarządzanie projektami i dokumentacja -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, workspace, projects, snippets, documentation, markdown
;; Description: Zaawansowane zarządzanie projektami z snippetami i dokumentacją
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Ten moduł zapewnia zarządzanie przestrzenią roboczą:
;; - Zarządzanie projektami i workspace
;; - Snippety i szablony kodu
;; - Markdown i dokumentacja
;; - Narzędzia do pisania

;;; Code:

;; ============================================================================
;; 🚀 ZARZĄDZANIE PROJEKTAMI - INTELIGENTNE OBSŁUGIWANIE
;; ============================================================================

;; === Projectile - Zarządzanie projektami ===
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; === Projectile Ripgrep - Szybkie wyszukiwanie ===
(use-package ripgrep
  :ensure t
  :after projectile)

;; === Projectile Git - Integracja z Git ===
(use-package projectile-git-autofetch
  :ensure t
  :after projectile
  :config
  (projectile-git-autofetch-mode))

;; === Projectile Test - Testy w projektach ===
;; Projectile Test - dostępny w Melpa
;; (use-package projectile-test
;;   :ensure t
;;   :after projectile)

;; ============================================================================
;; ✂️ SNIPPETY - SZABLONY I AUTOUZUPEŁNIANIE KODU
;; ============================================================================

;; === Yasnippet - System snippetów ===
(use-package yasnippet
  :ensure t
  :config
  ;; Konfiguracja YASnippet z obsługą duplikatów
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  
  ;; Pozwól na różne snippety o tym samym kluczu
  (setq yas-key-syntaxes '("w_" "w" "w_." "w_.()" "^ "))
  
  ;; Nie wyświetlaj ostrzeżeń o duplikatach
  (setq yas-verbosity 1)
  
  ;; Włącz w trybach programowania
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode)
  
  ;; Globalne włączenie
  (yas-global-mode 1))

;; === Yasnippet Snippets - Kolekcja snippetów ===
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  ;; Ładuj snippety bez konfliktów
  (yas-reload-all))

;; === Yasnippet Classic - Snippety dla klas ===
(use-package yasnippet-classic-snippets
  :ensure t
  :after yasnippet
  :config
  ;; Załaduj classic snippets z niższym priorytetem
  (let ((classic-dir (cl-find-if (lambda (d) 
                                    (string-match "yasnippet-classic-snippets" (or d "")))
                                  yas-snippet-dirs)))
    (when classic-dir
      ;; Przenieś classic snippets na koniec listy (najniższy priorytet)
      (setq yas-snippet-dirs (append (remove classic-dir yas-snippet-dirs) 
                                      (list classic-dir))))))

;; === Company Yasnippet - Integracja z Company ===
;; Company Yasnippet - dostępny w Melpa
;; (use-package company-yasnippet
;;   :ensure t
;;   :after (yasnippet))

;; ============================================================================
;; 📝 MARKDOWN - PROFESJONALNE PISANIE I DOKUMENTACJA
;; ============================================================================

;; === Markdown Mode - Edycja Markdown ===
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  ;; Włącz podświetlanie bloków kodu
  (setq markdown-fontify-code-blocks-natively t)
  ;; Dodaj rozpoznawanie języków w blokach kodu
  (add-to-list 'markdown-code-lang-modes '("python" . python-mode))
  (add-to-list 'markdown-code-lang-modes '("bash" . sh-mode))
  (add-to-list 'markdown-code-lang-modes '("rust" . rust-mode))
  (add-to-list 'markdown-code-lang-modes '("go" . go-mode))
  (add-to-list 'markdown-code-lang-modes '("javascript" . js-mode))
  (add-to-list 'markdown-code-lang-modes '("typescript" . typescript-mode)))

;; === Markdown Preview - Podgląd Markdown ===
(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode)

;; === Grip Mode - GitHub-styled Markdown ===
(use-package grip-mode
  :ensure t
  :after markdown-mode)

;; === Markdown TOC - Spis treści ===
(use-package markdown-toc
  :ensure t
  :after markdown-mode)

;; === Markdown All in One - Wszystko w jednym ===
(use-package markdown-mode
  :after markdown-mode)

;; ============================================================================
;; 📚 DOKUMENTACJA - NARZĘDZIA DO PISANIA
;; ============================================================================

;; === Writeroom - Tryb pisania ===
(use-package writeroom-mode
  :ensure t
  :commands writeroom-mode)

;; === Olivetti - Centrowanie tekstu ===
(use-package olivetti
  :ensure t
  :commands olivetti-mode)

;; === Visual Fill Column - Wizualne kolumny ===
(use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode)

;; === Auto Fill - Automatyczne zawijanie ===
(use-package auto-fill
  :hook (text-mode . auto-fill-mode))

;; === Flyspell - Sprawdzanie pisowni ===
(use-package flyspell
  :ensure t
  :hook (text-mode . flyspell-mode)
  :hook (prog-mode . flyspell-prog-mode)
  :config
  ;; Konfiguracja Hunspell
  (when (executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
            ("pl_PL" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
    
    ;; Sprawdź dostępne słowniki
    (let ((dicts (split-string (shell-command-to-string "hunspell -D 2>&1 | grep -E '^[a-z_]+'") "\n" t)))
      (when dicts
        (setq ispell-local-dictionary (car dicts))))
    
    ;; Fallback do aspell jeśli hunspell nie działa
    (unless (and (boundp 'ispell-local-dictionary) ispell-local-dictionary)
      (when (executable-find "aspell")
        (setq ispell-program-name "aspell")
        (setq ispell-local-dictionary "english")))))

;; ============================================================================
;; 🎯 DOSTARCZENIE I FINALIZACJA
;; ============================================================================

;; Dostarcz moduł workspace
(provide 'workspace)

;; Wyświetl komunikat sukcesu po załadowaniu modułu
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'workspace)
              (message "🚀 Zarządzanie projektami: Gotowe")
              (message "✂️ Snippety Yasnippet: Aktywne")
              (message "📝 Markdown: Pełne wsparcie")
              (message "📚 Narzędzia do pisania: Gotowe"))))

;;; workspace.el ends here
