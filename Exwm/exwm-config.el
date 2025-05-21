;;; desktop.el --- Przywracanie sesji EXWM + Emacs

;; === Desktop Mode: przywracanie otwartych plików i historii ===
(use-package desktop
  :init
  (setq desktop-dirname             "~/.emacs.d/desktop/"
        desktop-base-file-name      "emacs.desktop"
        desktop-base-lock-name      "lock"
        desktop-path                (list desktop-dirname)
        desktop-save                t
        desktop-auto-save-timeout  30
        desktop-restore-eager       5
        history-length              500
        savehist-additional-variables '(search-ring regexp-search-ring kill-ring)
        savehist-file "~/.emacs.d/savehist")
  :config
  (unless (file-exists-p desktop-dirname)
    (make-directory desktop-dirname t))
  (desktop-save-mode 1)
  (savehist-mode 1))

;; === Persp-mode: zachowywanie układów EXWM ===
(use-package persp-mode
  :ensure (:host github :repo "ch11ng/exwm")
  :after exwm
  :init
  (setq persp-keymap-prefix (kbd "C-c M-p"))
  :config
  (persp-mode)
  ;; Automatyczne wczytanie poprzedniego układu
  (let ((state-file "~/.emacs.d/persp-state"))
    (when (file-exists-p state-file)
      (persp-load-state-from-file state-file)))

  ;; Zapis stanu przy wyjściu z Emacsa
  (add-hook 'kill-emacs-hook
            (lambda ()
              (persp-save-state-to-file "~/.emacs.d/persp-state"))))

(provide 'session-config)
;;; session-config.el ends here
