;; ----------------------------------
;; Podstawowa konfiguracja Emacsa
;; ----------------------------------
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(global-font-lock-mode 1)
(set-frame-font "Fira Code Retina 12" nil t)

;; === UŻYTECZNE SKRÓTY ===
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Przełączanie się między buforami
(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)

;; Zapisz plik i zamknij bufor
(global-set-key (kbd "C-x C-s") 'save-buffer)
(global-set-key (kbd "C-x k") 'kill-buffer)

;; Zmienianie widoczności okien (split ekran)
(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)
(global-set-key (kbd "C-x 1") 'delete-other-windows)

;; Przejdź do definicji funkcji (LSP)
(global-set-key (kbd "M-.") 'lsp-find-definition)

;; Wyszukiwanie w pliku
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)

;; Alternatywne okna (windmove)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)

;; Operacje na plikach
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-c b") 'switch-to-buffer)

;; Edycja i zachowanie bufora
(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-default nil)
(delete-selection-mode 1)

;; === SKRÓTY: Translator ===
(global-set-key (kbd "C-c t r") 'translator/translate-region)            ;; PL → EN
(global-set-key (kbd "C-c t p") 'translator/translate-region-en-to-pl)   ;; EN → PL
(global-set-key (kbd "C-c t b") 'translator/translate-buffer-to-english) ;; Bufor PL → EN

(provide 'keybindings)
