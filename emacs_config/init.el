;;; init.el --- Emacs AI 2.0 Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, configuration, ai, development
;; Description: Profesjonalna konfiguracja Emacs z AI i nowoczesnymi narzędziami
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Emacs AI 2.0 - Profesjonalna konfiguracja z modułową architekturą
;; 
;; Struktura modułów:
;; - core.el: Podstawowa konfiguracja systemu z Elpacą
;; - completion.el: System uzupełniania i środowisko deweloperskie
;; - editing.el: Podstawowe narzędzia edycji
;; - languages.el: Wsparcie dla języków programowania
;; - development.el: Narzędzia deweloperskie i debugowanie
;; - workspace.el: Zarządzanie projektami i dokumentacja
;; - ai.el: Integracja z AI
;; - ui.el: Interfejs użytkownika
;; - keybindings.el: Skróty klawiszowe
;; - org-mode.el: Konfiguracja Org Mode
;; - utils.el: Funkcje pomocnicze

;;; Code:

;; ============================================================================
;; 🚀 INICJALIZACJA SYSTEMU - PODSTAWOWE USTAWIENIA
;; ============================================================================

;; === Podstawowe ustawienia startowe ===
(setq inhibit-startup-message t)           ;; Wyłącz wiadomość startową
(setq initial-scratch-message nil)         ;; Pusty scratch buffer
(setq frame-title-format "%b")             ;; Tytuł ramki

;; === Ustawienia wydajności ===
(setq gc-cons-threshold 100000000)         ;; Zwiększ próg GC
(setq read-process-output-max (* 1024 1024)) ;; Zwiększ bufor procesów

;; ============================================================================
;; 📁 ŁADOWANIE MODUŁÓW - MODUŁOWA ARCHITEKTURA
;; ============================================================================

;; === Dodaj katalog lisp do ścieżki ładowania ===
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; === Ładuj moduły w logicznej kolejności ===
(require 'core)        ;; Elpaca + podstawy systemu
(require 'completion)  ;; System uzupełniania
(require 'editing)     ;; Podstawowe narzędzia edycji
(require 'languages)   ;; Wsparcie dla języków
(require 'development) ;; Narzędzia deweloperskie
(require 'workspace)   ;; Zarządzanie projektami
(require 'ai)          ;; Integracja z AI
(require 'ui)          ;; Interfejs użytkownika
(require 'keybindings) ;; Skróty klawiszowe
(require 'org-mode)    ;; Org Mode
(require 'utils)       ;; Funkcje pomocnicze

;; ============================================================================
;; 🎯 FINALIZACJA I KOMUNIKATY
;; ============================================================================

;; === Komunikat gotowości systemu ===
(add-hook 'after-init-hook
          (lambda ()
            (message "🚀 Emacs AI 2.0: System gotowy!")
            (message "✨ Wszystkie moduły załadowane")
            (message "📦 Elpaca: Aktywny")
            (message "🔍 Uzupełnianie: Vertico + Corfu")
            (message "🌐 LSP: Wszystkie języki")
            (message "🐛 Debugowanie: DAP Mode")
            (message "📝 Dokumentacja: Markdown + Snippety")
            (message "🤖 AI: Integracja aktywna")
            (message "🎨 UI: Nowoczesny interfejs")
            (message "⌨️ Skróty: Space jako leader")
            (message "📚 Org Mode: Pełne wsparcie")
            (message "🔧 Narzędzia: Wszystkie gotowe")))

;; === Informacja o wersji ===
(message "Emacs AI 2.0 - Konfiguracja załadowana pomyślnie!")

;;; init.el ends here
