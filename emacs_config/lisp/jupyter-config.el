;;; jupyter-config.el --- Konfiguracja Emacsa do pracy z Jupyterem via jupyter.el + Polymode

;;; Commentary:
;; Konfiguracja Emacsa umożliwiająca uruchamianie komórek Jupyter bezpośrednio z poziomu Emacsa
;; za pomocą pakietów: jupyter.el, polymode, org-mode i opcjonalnie ein.

;;; Code:

;; === jupyter.el ===
(use-package jupyter
  :ensure t
  :commands (jupyter-run-repl)
  :config
  ;; Wyświetlaj wyniki jako overlaye
  (setq jupyter-eval-use-overlays t))
              
;; === Polymode (dla obsługi wielu języków w jednym buforze) ===
(use-package polymode
  :ensure t)
  
;; === POLYMODE PATH ===
(setq load-path
      (append '("path/to/vc/dir/polymode/"
                "path/to/vc/dir/poly-markdown/")
              load-path))

(use-package poly-markdown
  :ensure t)

(use-package poly-R
  :ensure t)

;; === Org-mode + jupyter ===
(with-eval-after-load 'org
  (require 'jupyter)
  (add-to-list 'org-babel-load-languages '(jupyter . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; === Opcjonalnie: EIN – graficzna obsługa Jupyter Notebook ===
(use-package ein
  :defer t
  :config
  ;; Włącz LSP w trybie notebooka (jeśli masz lsp-mode)
  (add-hook 'ein:notebook-mode-hook #'lsp-deferred))

(provide 'jupyter-config)

;;; jupyter-config.el ends here
