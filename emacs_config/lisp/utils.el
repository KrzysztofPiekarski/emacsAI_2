;;; utils.el --- Przydatne narzędzia i skróty

;; === Avy ===
(use-package avy
  :bind (("M-g M-g" . avy-goto-char-timer)  ;; skok do znaku
         ("M-g f" . avy-goto-line)))       ;; skok do linii

;; === VTerm ===
(use-package vterm
  :commands vterm
  :config
  ;; Otwórz lub przełącz do istniejącego bufora vterm
  (defun my/vterm ()
    "Przełącz do *vterm* lub otwórz nowy, jeśli nie istnieje."
    (interactive)
    (if (get-buffer "*vterm*")
        (pop-to-buffer "*vterm*")
      (vterm)))

  ;; Zabij bufor vterm
  (defun my/kill-vterm ()
    "Zamknij bufor *vterm*, jeśli istnieje."
    (interactive)
    (when (get-buffer "*vterm*")
      (kill-buffer "*vterm*")
      (message "vterm killed")))

  ;; Skróty klawiszowe
  (global-set-key (kbd "C-c t") #'my/vterm)
  (global-set-key (kbd "C-c k") #'my/kill-vterm))


;; === Which-key ===
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)) ;; szybsze pokazywanie
(load "keybindings.el")

;; === NEOTREE ===
(use-package neotree
  :bind ([f8] . neotree-toggle)
  :custom
  ;; Styl Neotree (ikony w GUI, strzałki w terminalu)
  (neo-theme (if window-system 'icons 'arrow))
  (neo-window-width 30)                ;; Szerokość okna Neotree
  (neo-window-position 'left)          ;; Pozycja Neotree (left, right, top, bottom)
  (neo-smart-open t)                   ;; Automatyczne otwieranie pliku po kliknięciu
  :config
  ;; Kolorystyka i czcionki dla Neotree
  (set-face-attribute 'neo-dir-link-face nil :foreground "#5F87B5")  ;; Kolor dla katalogów
  (set-face-attribute 'neo-file-link-face nil :foreground "#A8A8A8") ;; Kolor dla plików
  )

;; Ładowanie ikon tylko w trybie graficznym
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

;; Skróty klawiszowe
(global-set-key (kbd "C-x t") 'neotree-toggle) ;; Zmieniamy skrót klawiszowy
(global-set-key (kbd "C-x C-n") 'neotree-find) ;; Przechodzenie do pliku w Neotree 

;; === TREEMACS ====
(use-package treemacs
  :config)

(use-package lsp-treemacs
  :after (lsp-mode treemacs))

(provide 'utils)
