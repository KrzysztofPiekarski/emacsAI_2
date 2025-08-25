;;; ai.el --- Professional AI Integration & OpenAI Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, ai, openai, gpt, claude, llama, machine-learning
;; Description: Professional AI integration with OpenAI, Claude, and LLaMA for enhanced productivity
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; This module provides comprehensive AI integration capabilities:
;; - OpenAI GPT integration with GPTel
;; - ChatGPT shell for interactive conversations
;; - Multi-backend LLM support (OpenAI, Claude, LLaMA)
;; - Org-AI integration for enhanced note-taking
;; - Environment variable management for API keys
;; - Professional AI workflow tools
;;
;; Features:
;; - Multiple AI model support (GPT-4, Claude, LLaMA)
;; - Text summarization and code improvement
;; - Interactive AI conversations
;; - Org mode AI integration
;; - Secure API key management
;; - Professional AI workflow optimization

;;; Code:

;; ============================================================================
;; 🔑 API KEY MANAGEMENT - SECURE CONFIGURATION
;; ============================================================================

;; === AUTHINFO INTEGRATION - SECURE KEY STORAGE ===
;; Retrieve OpenAI API key from authinfo.gpg for enhanced security
;; This provides a secure alternative to environment variables
(defun my/get-openai-key-from-authinfo ()
  "Retrieve OpenAI API key from authinfo.gpg for secure access."
  (when (fboundp 'auth-source-search)
    (let ((match (car (auth-source-search :host "openai.com"
                                          :user "openai"
                                          :require '(:user :secret)))))
      (when match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret) (funcall secret) secret))))))

;; === ENVIRONMENT VARIABLE SUPPORT ===
;; Load environment variables for secure API key management
;; Supports both .env files and authinfo.gpg for enhanced security
(use-package dotenv-mode :ensure t)
(use-package load-env-vars
  :ensure t
  :config
  (unless (getenv "OPENAI_API_KEY")
    ;; Sprawdź czy plik .env istnieje przed próbą załadowania
    (let ((env-file "~/.emacs.d/.env"))
      (when (file-exists-p (expand-file-name env-file))
        (load-env-vars env-file))))
    
    ;; Spróbuj pobrać klucz z authinfo tylko jeśli funkcja jest dostępna
    (when (and (not (getenv "OPENAI_API_KEY"))
               (fboundp 'my/get-openai-key-from-authinfo))
      (let ((key (my/get-openai-key-from-authinfo)))
        (when key
          (setenv "OPENAI_API_KEY" key)))))
        
;; ============================================================================
;; 🤖 GPTEL - OPENAI GPT INTEGRATION
;; ============================================================================

;; === GPTEL - PROFESSIONAL GPT INTEGRATION ===
;; GPTel provides seamless OpenAI GPT integration
;; with advanced features for text processing and AI assistance
(use-package gptel
  :ensure t
  :custom
  (gptel-api-key (getenv "OPENAI_API_KEY"))
  (gptel-model "gpt-4")
  :config

  ;; === TEXT SUMMARIZATION - AI-POWERED SUMMARIES ===
  ;; Function to summarize selected text using GPT
  ;; Provides intelligent text condensation and analysis
  (defun gptel-summarize-region ()
    "Summarize selected text using GPT for quick understanding."
    (interactive)
    (if (use-region-p)
        (gptel-request
         (buffer-substring-no-properties (region-beginning) (region-end))
         :system "Streszcz poniższy tekst w kilku zdaniach:"
         :callback (lambda (response)
                     (with-output-to-temp-buffer "*GPT Summary*"
                       (princ response))))
      (message "Zaznacz najpierw tekst do streszczenia.")))

  ;; === CODE IMPROVEMENT - AI-ASSISTED CODING ===
  ;; Function to fix and improve selected code
  ;; Provides intelligent code analysis and suggestions
  (defun gptel-fix-code ()
    "Find and fix errors in selected code with AI assistance."
    (interactive)
    (if (use-region-p)
        (gptel-request
         (buffer-substring-no-properties (region-beginning) (region-end))
         :system "Znajdź i popraw błędy w tym kodzie, wyjaśnij zmiany:"
         :callback (lambda (response)
                     (with-output-to-temp-buffer "*GPT Fixed Code*"
                       (princ response))))
      (message "Zaznacz kod do poprawy.")))

  ;; === KEYBINDINGS - QUICK ACCESS ===
  ;; Configure keybindings for AI functions
  ;; Provides quick access to AI-powered features
  :bind (("C-c a s" . gptel-summarize-region)
         ("C-c a f" . gptel-fix-code)))
              
;; ============================================================================
;; 💬 CHATGPT SHELL - INTERACTIVE AI CONVERSATIONS
;; ============================================================================

;; === CHATGPT SHELL - INTERACTIVE CHAT ===
;; ChatGPT shell provides interactive AI conversations
;; with full chat history and context management
(use-package chatgpt-shell
  :ensure t
  :commands (chatgpt-shell)
  :custom
  (chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
  :bind (("C-c C-g" . chatgpt-shell)))

;; ============================================================================
;; 🌐 LLM.EL - MULTI-BACKEND AI SUPPORT
;; ============================================================================

;; === LLM.EL - UNIVERSAL AI INTERFACE ===
;; LLM.el provides multi-backend AI support
;; with seamless switching between different AI models
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
  
  ;; === REGION PROCESSING - AI TEXT ANALYSIS ===
  ;; Function to apply LLM processing to selected text
  ;; Provides flexible AI interaction with custom prompts
  (defun my/llm-region ()
    "Apply LLM processing to selected region with custom prompt."
    (interactive)
    (if (use-region-p)
        (let ((prompt (read-string "Polecenie do modelu: "))
              (text (buffer-substring-no-properties (region-beginning) (region-end))))
          (llm-chat prompt text))
      (message "Zaznacz tekst do analizy.")))

  ;; === BACKEND SWITCHING - MODEL SELECTION ===
  ;; Functions to switch between different AI backends
  ;; Provides flexibility in AI model selection
  (defun my/use-openai ()
    "Switch to OpenAI backend for AI processing."
    (interactive)
    (setq llm-default-backend :openai)
    (message "Wybrano OpenAI"))

  (defun my/use-claude ()
    "Switch to Claude backend for AI processing."
    (interactive)
    (setq llm-default-backend :anthropic)
    (message "Wybrano Claude"))

  (defun my/use-llama ()
    "Switch to local LLaMA backend for AI processing."
    (interactive)
    (setq llm-default-backend :llamacpp)
    (message "Wybrano lokalny LLaMA"))

  ;; === KEYBINDINGS - QUICK ACCESS ===
  ;; Configure keybindings for LLM functions
  ;; Provides quick access to multi-backend AI features
  :bind (("C-c a l" . my/llm-region)
         ("C-c a o" . my/use-openai)
         ("C-c a c" . my/use-claude)
         ("C-c a m" . my/use-llama)))
  
;; ============================================================================
;; 🎯 AI MODE TOGGLE - WORKFLOW OPTIMIZATION
;; ============================================================================

;; === AI MODE TOGGLE - QUICK ENABLE/DISABLE ===
;; Function to quickly toggle AI tools in org-mode
;; Provides easy control over AI feature availability
(defun my/ai-mode-toggle ()
  "Toggle AI tools in org-mode for quick enable/disable."
  (interactive)
  (if (bound-and-true-p org-ai-mode)
      (progn
        (org-ai-mode -1)
        (message "AI mode OFF"))
    (org-ai-mode 1)
    (message "AI mode ON")))
(global-set-key (kbd "C-c a t") #'my/ai-mode-toggle)

;; ============================================================================
;; 📝 ORG-AI - ENHANCED NOTE-TAKING
;; ============================================================================

;; === ORG-AI - AI-POWERED ORG MODE ===
;; Org-AI provides AI integration for Org mode
;; with enhanced note-taking and content generation
(use-package org-ai
  :ensure t
  :hook (org-mode . org-ai-mode)
  :custom
  (org-ai-openai-api-key (getenv "OPENAI_API_KEY"))
  (org-ai-default-chat-model "gpt-4")
  :config
  (org-ai-install-yasnippets)
  :bind (:map org-mode-map
              ("C-c a c" . org-ai-complete)    ;; AI completion
              ("C-c a r" . org-ai-rewrite)     ;; AI rewriting
              ("C-c a x" . org-ai-explainer))) ;; AI explanation

;; ============================================================================
;; 🔗 FUNCTION ALIASES - QUICK ACCESS
;; ============================================================================

;; === FUNCTION ALIASES - CONVENIENT ACCESS ===
;; Provide convenient aliases for common AI functions
;; Enhances workflow efficiency and accessibility
(defalias 'askgpt #'chatgpt-shell)           ;; Quick ChatGPT access
(defalias 'summarize #'gptel-summarize-region) ;; Quick summarization
(defalias 'fixcode #'gptel-fix-code)          ;; Quick code fixing
(defalias 'quizme #'gptel-create-quiz)        ;; Quick quiz creation

;; ============================================================================
;; 🎯 PROVIDE & FINALIZATION
;; ============================================================================

;; Provide the AI module
(provide 'ai)

;; Display success message when module is loaded
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'ai)
              (message "🤖 AI Integration: Professional AI Tools Ready")
              (message "🔑 API Management: Secure Key Configuration Active")
              (message "💬 GPT Integration: GPTel & ChatGPT Ready")
              (message "🌐 Multi-Backend: OpenAI, Claude, LLaMA Support")
              (message "📝 Org-AI: Enhanced Note-Taking with AI")
              (message "🎯 AI Workflow: Professional AI Tools Configured"))))

;;; ai.el ends here
