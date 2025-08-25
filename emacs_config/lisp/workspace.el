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
  :init
  (yas-global-mode 1))

;; === Yasnippet Snippets - Kolekcja snippetów ===
(use-package yasnippet-snippets
  :ensure t)

;; === Yasnippet Class - Snippety dla klas ===
(use-package yasnippet-classic-snippets
  :ensure t)

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
  :hook (prog-mode . flyspell-prog-mode))

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
