;;; translator.el --- Professional Translation Tools & Language Support -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, translation, language, google-translate, translate-shell
;; Description: Professional translation tools with Google Translate and translate-shell integration
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; This module provides comprehensive translation capabilities:
;; - Google Translate integration via translate-shell
;; - Polish to English translation
;; - English to Polish translation
;; - Full buffer translation support
;; - Professional translation workflow
;;
;; Prerequisites:
;; - Ubuntu/Debian: sudo apt install translate-shell
;; - Arch Linux: yay -S translate-shell
;;
;; Features:
;; - Fast text translation with translate-shell
;; - Region-based translation for selected text
;; - Full buffer translation capabilities
;; - Professional translation interface
;; - Multi-language support framework

;;; Code:

;; ============================================================================
;; 🌐 TRANSLATION ENGINE - CORE TRANSLATION FUNCTIONALITY
;; ============================================================================

;; === TRANSLATE-SHELL INTEGRATION ===
;; Core function to execute translate-shell commands
;; Provides the foundation for all translation operations
(defun translator--run-command (text lang)
  "Execute translate-shell command for text translation to specified language.
  
  TEXT: The text to be translated
  LANG: Target language code (e.g., 'en' for English, 'pl' for Polish)"
  (let* ((command (format "trans -b :%s \"%s\"" lang text)))
    (shell-command-to-string command)))

;; ============================================================================
;; 📝 TEXT SELECTION - REGION MANAGEMENT
;; ============================================================================

;; === REGION VALIDATION ===
;; Function to validate text selection and extract region content
;; Ensures proper text selection before translation operations
(defun translator--get-region-or-error ()
  "Get selected text region or display error if no region is selected.
  
  Returns the selected text or signals an error for user guidance."
  (unless (use-region-p)
    (error "Zaznacz tekst do przetłumaczenia"))
  (buffer-substring-no-properties (region-beginning) (region-end)))

;; ============================================================================
;; 🔄 TRANSLATION FUNCTIONS - CORE OPERATIONS
;; ============================================================================

;; === POLISH TO ENGLISH TRANSLATION ===
;; Translate selected text from Polish to English
;; Provides quick translation with minibuffer display
;;;###autoload
(defun translator/translate-region ()
  "Translate selected text from Polish to English.
  
  Displays translation result in the minibuffer for quick access.
  Requires text selection before execution."
  (interactive)
  (message "Tłumaczenie: %s"
           (translator--run-command (translator--get-region-or-error) "en")))

;; === ENGLISH TO POLISH TRANSLATION ===
;; Translate selected text from English to Polish
;; Provides reverse translation capabilities
;;;###autoload
(defun translator/translate-region-en-to-pl ()
  "Translate selected text from English to Polish.
  
  Displays translation result in the minibuffer for quick access.
  Requires text selection before execution."
  (interactive)
  (message "Tłumaczenie: %s"
           (translator--run-command (translator--get-region-or-error) "pl")))

;; === FULL BUFFER TRANSLATION ===
;; Translate entire buffer content from Polish to English
;; Creates new buffer with translation results
;;;###autoload
(defun translator/translate-buffer-to-english ()
  "Translate entire buffer content from Polish to English.
  
  Creates a new buffer '*Translation*' with complete translation.
  Useful for full document translation and review."
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (translated (translator--run-command text "en")))
    (with-output-to-temp-buffer "*Translation*"
      (princ translated))))

;; ============================================================================
;; 🎯 PROVIDE & FINALIZATION
;; ============================================================================

;; Provide the translator module
(provide 'translator)

;; Display success message when module is loaded
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'translator)
              (message "🌐 Translation Tools: Professional Language Support Ready")
              (message "🔄 Polish ↔ English: Bidirectional Translation Active")
              (message "📝 Region Translation: Quick Text Translation Ready")
              (message "📄 Buffer Translation: Full Document Translation Active")
              (message "🚀 Translate-Shell: Fast Translation Engine Ready"))))

;;; translator.el ends here
