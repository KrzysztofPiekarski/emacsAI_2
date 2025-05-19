;;; lisp-dev.el --- Lisp Development Environment Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhances support for Emacs Lisp, Common Lisp and other Lisp dialects.
;; Includes smart indentation, structure editing, completion and REPL tools.

;;; Code:

;; Parinfer for smart indentation + parentheses balance
;(use-package parinfer-rust-mode
;  :hook ((emacs-lisp-mode lisp-mode) . parinfer-rust-mode)
;  :config
;  (setq parinfer-rust-auto-download t))

;; Alternative: Aggressive indentation
(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-mode) . aggressive-indent-mode))

;; Smart parentheses handling
(use-package smartparens
  :hook ((emacs-lisp-mode lisp-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

;; Optional: Paredit (choose either this or smartparens)
;; (use-package paredit
;;   :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))

;; Helpful documentation
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)))

;; Elisp cross-reference and def jumping
(use-package elisp-def
  :hook (emacs-lisp-mode . elisp-def-mode))

;; === Hook do Emacs Lisp ===
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (setq-local completion-at-point-functions
                        (list #'cape-elisp-block
                              #'cape-elisp-symbol
                              #'cape-dabbrev
                              #'cape-keyword
                              #'cape-file))))

;; Completion engine
(use-package company
  :hook ((emacs-lisp-mode lisp-mode) . company-mode))

;; Common Lisp (SBCL, etc.) support
(use-package sly
  :when (executable-find "sbcl")
  :init
  (setq inferior-lisp-program "sbcl"))

(provide 'lisp-dev)
;;; lisp-dev.el ends here
