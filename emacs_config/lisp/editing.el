;; === LSP Mode: językowy serwer podpowiedzi ===
(use-package lsp-mode
  :hook ((python-mode . lsp)
         (go-mode . lsp)
         (js-mode . lsp)
         (java-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (yaml-mode . lsp)
         (bash-mode . lsp)
         (markdown-mode . lsp)
         (web-mode . lsp))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting nil)) 

;; === LSP UI: dokumentacja, tooltippy, boczne podpowiedzi ===
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 0.2)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t))
  
;; === LSP Pyright: serwer języka Python ===
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; === Flycheck – lintowanie ===
(use-package flycheck
  :init (global-flycheck-mode)) 

;; === DAP – debugowanie z LSP (Python) ===
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable
        (or (when (getenv "CONDA_PREFIX")
              (concat (getenv "CONDA_PREFIX") "/bin/python"))
            "python3")))

;; === Wsparcie dla języków ===
(use-package go-mode
  :hook (before-save . gofmt-before-save)) ; LSP dla Go jest domyślnie przez lsp-mode

(use-package web-mode :mode "\\.html?\\'")
(use-package yaml-mode :mode "\\.ya?ml\\'")
(use-package bash-completion)
(use-package haskell-mode)
(use-package clojure-mode)

;; === Ustawienia Python ===
(add-hook 'python-mode-hook #'font-lock-mode)
(add-hook 'python-mode-hook #'display-line-numbers-mode)

;; === Markdown ===
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  ;; Włącz podświetlanie bloków kodu
  (setq markdown-fontify-code-blocks-natively t)
  ;; Dodaj rozpoznawanie języków w blokach kodu
  (add-to-list 'markdown-code-lang-modes '("python" . python-mode))
  (add-to-list 'markdown-code-lang-modes '("bash" . sh-mode))
  (add-to-list 'markdown-code-lang-modes '("json" . json-mode)))

(use-package markdown-preview-mode
  :after markdown-mode)

(use-package grip-mode
  :after markdown-mode)

;; === Snippety i składnia ===
(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; === Conda: automatyczne aktywowanie środowisk ===
(use-package conda
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t))

(provide 'editing)

