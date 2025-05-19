;; === WIZUALNE ULEPSZENIA ===
;(use-package doom-themes
;  :config
;  (load-theme 'doom-nord t))

; ===CATPPUCCIN THEME ===
(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha) ;; Możliwe: 'latte, 'frappe, 'macchiato, 'mocha
  (load-theme 'catppuccin :no-confirm)) 
  
(use-package doom-modeline
  :init (doom-modeline-mode 1))

(set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 120)
 
;; === DASHBOARD ===
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Witaj w Emacsie!")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-set-footer nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))
 
(add-hook 'dashboard-mode-hook
          (lambda ()
            (face-remap-add-relative 'default :height 1.2))) 

;; Dodatkowo: powiększenie modeline (opcjonalnie)
(custom-set-faces
 '(mode-line ((t (:family "FiraCode Nerd Font" :height 1.1 :weight normal))))
 '(mode-line-inactive ((t (:family "FiraCode Nerd Font" :height 1.2)))))

(provide 'ui)
