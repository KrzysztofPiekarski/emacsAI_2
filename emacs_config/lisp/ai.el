;;; emacsAI.el --- AI configuration using OpenAI packages -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures OpenAI-based tools in Emacs:
;; - GPTel (OpenAI)
;; - ChatGPT-shell
;; - Org-AI
;; - llm.el (OpenAI, Claude, LLaMA)
;; API z .env lub ~/.authinfo.gpg
;;; Code:

;; === Load API key ===
(use-package dotenv-mode :ensure t)
(use-package load-env-vars
  :ensure t
  :config
  (unless (getenv "OPENAI_API_KEY")
    (load-env-vars "~/.emacs.d/.env")
    (setenv "OPENAI_API_KEY" (or (getenv "OPENAI_API_KEY") (my/get-openai-key-from-authinfo)))))

(defun my/get-openai-key-from-authinfo ()
  (let ((match (car (auth-source-search :host "openai.com"
                                        :user "openai"
                                        :require '(:user :secret)))))
    (when match
      (let ((secret (plist-get match :secret)))
        (if (functionp secret) (funcall secret) secret)))))
        
;; === GPTel ===
(use-package gptel
  :ensure t
  :custom
  (gptel-api-key (getenv "OPENAI_API_KEY"))
  (gptel-model "gpt-4")
  :config

  ;; Funkcja do streszczenia tekstu
  (defun gptel-summarize-region ()
    "Streszcz zaznaczony tekst przy użyciu GPT."
    (interactive)
    (if (use-region-p)
        (gptel-request
         (buffer-substring-no-properties (region-beginning) (region-end))
         :system "Streszcz poniższy tekst w kilku zdaniach:"
         :callback (lambda (response)
                     (with-output-to-temp-buffer "*GPT Summary*"
                       (princ response))))
      (message "Zaznacz najpierw tekst do streszczenia.")))

  ;; Funkcja do poprawy kodu
  (defun gptel-fix-code ()
    "Popraw błędy w zaznaczonym kodzie."
    (interactive)
    (if (use-region-p)
        (gptel-request
         (buffer-substring-no-properties (region-beginning) (region-end))
         :system "Znajdź i popraw błędy w tym kodzie, wyjaśnij zmiany:"
         :callback (lambda (response)
                     (with-output-to-temp-buffer "*GPT Fixed Code*"
                       (princ response))))
      (message "Zaznacz kod do poprawy.")))

  ;; (opcjonalnie) Dodaj więcej funkcji np. tłumaczenia, quizu itd.

  :bind (("C-c a s" . gptel-summarize-region)
         ("C-c a f" . gptel-fix-code)))
              
;; === ChatGPT-Shell ===
(use-package chatgpt-shell
  :ensure t
  :commands (chatgpt-shell)
  :custom
  (chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
  :bind (("C-c C-g" . chatgpt-shell)))

;; === llm.el (multi-backend) ===
(use-package llm
  :ensure t
  :custom
  (llm-default-backend :openai)
  (llm-backend-openai-api-key (getenv "OPENAI_API_KEY"))
  (llm-backend-openai-model "gpt-4")
  (llm-backend-anthropic-api-key (getenv "ANTHROPIC_API_KEY"))
  (llm-backend-anthropic-model "claude-3-opus-20240229")
  (llm-backend-llamacpp-host "http://localhost:8080")
  (llm-backend-llamacpp-model "llama")
  :config
  (defun my/llm-region ()
    "Zastosuj LLM na zaznaczonym regionie z promptem."
    (interactive)
    (if (use-region-p)
        (let ((prompt (read-string "Polecenie do modelu: "))
              (text (buffer-substring-no-properties (region-beginning) (region-end))))
          (llm-chat prompt text))
      (message "Zaznacz tekst do analizy.")))

  (defun my/use-openai ()
    (interactive)
    (setq llm-default-backend :openai)
    (message "Wybrano OpenAI"))

  (defun my/use-claude ()
    (interactive)
    (setq llm-default-backend :anthropic)
    (message "Wybrano Claude"))

  (defun my/use-llama ()
    (interactive)
    (setq llm-default-backend :llamacpp)
    (message "Wybrano lokalny LLaMA"))

  :bind (("C-c a l" . my/llm-region)
         ("C-c a o" . my/use-openai)
         ("C-c a c" . my/use-claude)
         ("C-c a m" . my/use-llama)))
  
;; === Toggle AI mode ===
(defun my/ai-mode-toggle ()
  "Toggle AI tools in org-mode."
  (interactive)
  (if (bound-and-true-p org-ai-mode)
      (progn
        (org-ai-mode -1)
        (message "AI mode OFF"))
    (org-ai-mode 1)
    (message "AI mode ON")))
(global-set-key (kbd "C-c a t") #'my/ai-mode-toggle)

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

;; === Aliasy ===
(defalias 'askgpt #'chatgpt-shell)
(defalias 'summarize #'gptel-summarize-region)
(defalias 'fixcode #'gptel-fix-code)
(defalias 'quizme #'gptel-create-quiz)

(provide 'ai)
;;; emacsAI.el ends here
