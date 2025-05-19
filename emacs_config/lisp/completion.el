;;; completion.el --- Completion configuration

;; === Vertico ===
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)  ;; pozwala na zapętlanie listy
  (vertico-count 15))

;; === Orderless ===
(use-package orderless
  :custom
  (completion-styles '(orderless basic))  ;; 'basic' jako fallback
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))) ;; lepsze uzupełnianie ścieżek

;; === Marginalia ===
(use-package marginalia
  :init (marginalia-mode))

;; === Consult ===
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("C-c h" . consult-history)))

;; === Corfu ===
(use-package corfu
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

;; === Cape — Completion At Point Extensions ===
(use-package cape
  :init
  (defun my/setup-cape ()
    (setq-local completion-at-point-functions
                (list
                 #'cape-dabbrev
                 #'cape-file
                 #'cape-keyword
                 #'cape-symbol
                 #'cape-elisp-block
                 #'lsp-completion-at-point))) ;; LSP jako jedno ze źródeł
  (add-hook 'prog-mode-hook #'my/setup-cape))

;; === TRANSIENT ===
(use-package transient
  :ensure t)
  
;; === MAGIT ===
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package forge
  :after magit
  :config
  ;; opcjonalnie – pobiera automatycznie dane z GitHuba przy starcie magit
  (setq forge-topic-list-limit '(60 . -1)))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)) 

(use-package git-timemachine
  :commands git-timemachine)  ;; można uruchamiać przez M-x 

;; === PROJECTILE ===
(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; === UNDO-TREE ===
(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-auto-save-history t)
  :config
  (unless (file-exists-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo"))) 

;; === Eglot (LSP) dla języków programowania ===
(use-package eglot
  :hook ((python-mode . eglot-ensure)
		 (go-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio"))))

;; === APHELEIA – automatyczne formatowanie kodu ===
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

;; === PYVENV ===
(use-package pyvenv
  :config
  (pyvenv-mode 1)) 

;; === ISORTIFY – sortowanie importów ===
(use-package isortify
  :hook (python-mode . isortify-mode))

(provide 'completion)
