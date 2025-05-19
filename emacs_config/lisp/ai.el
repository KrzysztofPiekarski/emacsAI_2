;;; emacsAI.el --- AI configuration using OpenAI packages -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures OpenAI-based tools in Emacs:
;; - gptel (chat with OpenAI in buffer)
;; - chatgpt-shell (shell-like chat interface)
;; - org-ai (AI integration with Org mode)
;; Loads API key from .env (via dotenv.el) or ~/.authinfo.gpg

;;; Code:

;; === Load API key from .env ===
(use-package dotenv-mode
  :ensure t)

(use-package load-env-vars
  :ensure t
  :config
  (load-env-vars "~/.emacs.d/.env"))

;; OPTIONAL: Load from ~/.authinfo.gpg if needed
(defun my/get-openai-key-from-authinfo ()
  (let ((match (car (auth-source-search :host "openai.com"
                                        :user "openai"
                                        :require '(:user :secret)))))
    (when match
      (let ((secret (plist-get match :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

;; Fallback if needed
(unless (getenv "OPENAI_API_KEY")
  (setenv "OPENAI_API_KEY" (my/get-openai-key-from-authinfo)))

;; === GPTel ===
(use-package gptel
  :ensure t
  :custom
  (gptel-api-key (getenv "OPENAI_API_KEY"))
  (gptel-model "gpt-4")
  :config
  (defun gptel-summarize-region ()
    "Podsumuj zaznaczony tekst za pomocą GPT."
    (interactive)
    (gptel-send nil "Streszcz poniższy tekst:" (region-beginning) (region-end)))

  (defun gptel-fix-code ()
    "Popraw błędy w zaznaczonym kodzie."
    (interactive)
    (gptel-send nil "Znajdź i popraw błędy w tym kodzie:" (region-beginning) (region-end)))

  :bind (("C-c a s" . gptel-summarize-region)
         ("C-c a f" . gptel-fix-code)))

;; === ChatGPT-Shell ===
(use-package chatgpt-shell
  :ensure t
  :commands (chatgpt-shell)
  :custom
  (chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
  :bind (("C-c C-g" . chatgpt-shell)))

;; === Org-AI ===
(use-package org-ai
  :ensure t
  :hook (org-mode . org-ai-mode)
  :custom
  (org-ai-openai-api-key (getenv "OPENAI_API_KEY"))
  (org-ai-default-chat-model "gpt-4")
  :config
  (org-ai-install-yasnippets)
  :bind (:map org-mode-map
              ("C-c a c" . org-ai-complete)
              ("C-c a r" . org-ai-rewrite)
              ("C-c a x" . org-ai-explainer)))

(provide 'ai)
;;; emacsAI.el ends here
