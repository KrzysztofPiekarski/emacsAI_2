;;; org-mode.el --- Konfiguracja Org-Mode i powiązanych pakietów

;; === ORG-MODE ===
(use-package org
  :ensure t
  :init
  (setq org-directory "~/org")
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))
  :config
  ;; Można dodać dodatkową konfigurację, jeśli chcesz
  )

;; === ORG-ROAM ===
(use-package org-roam
  :ensure t
:after org
  :init
  (let ((dir (expand-file-name "org-roam" org-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (setq org-roam-directory dir))
  :config
  (org-roam-db-autosync-mode))

;; === org-modern ===
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "✸" "✿"))
  (org-modern-table t)
  (org-modern-list '((?- . "•") (?+ . "‣") (?* . "◆")))
  (org-modern-block-fringe nil)
  (org-modern-todo nil)
  (setq-default line-spacing 0.2)      ;; Dodano do :custom
  (set-face-attribute 'default nil :family "Fira Code Retina" :height 120))

;; === org-present – minimalistyczne prezentacje w Emacsie ===
(use-package org-present
  :ensure t 
  :hook ((org-present-mode . org-present-setup)
         (org-present-mode-quit . org-present-teardown))
  :config
  ;; Funkcja pomocnicza do ustawień dla org-present
  (defun org-present-setup ()
    "Konfiguracja trybu prezentacji"
    (org-display-inline-images)
    (org-present-big)
    (org-hide-leading-stars)
    (org-present-hide-cursor)
    (org-present-read-only))

  (defun org-present-teardown ()
    "Czyszczenie po zakończeniu prezentacji"
    (org-remove-inline-images)
    (org-present-small)
    (org-show-leading-stars)
    (org-present-show-cursor)
    (org-present-read-write)))

;; === org-download ===
(use-package org-download
  :ensure t
  :after org
  :hook (org-mode . org-download-enable)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir (expand-file-name "images" org-directory))  ;; Zmieniona lokalizacja
  (org-download-heading-lvl nil)
  (org-download-screenshot-method "xfce4-screenshooter -r -o cat > %s"))

;; === org-bullets ===
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))) 

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))
  ;; wyłącz numerację linii w pdf-view-mode
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

(provide 'org-mode)
