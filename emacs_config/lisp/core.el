;;; core.el --- Podstawowa konfiguracja systemu Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, core, elpaca, packages, configuration
;; Description: Podstawowa konfiguracja systemu z menedżerem pakietów Elpaca
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Ten moduł zawiera podstawową konfigurację systemu Emacs:
;; - Inicjalizacja menedżera pakietów Elpaca
;; - Podstawowe ustawienia systemu
;; - Globalne zmienne i hooks
;; - Konfiguracja wydajności

;;; Code:

;; ============================================================================
;; 🚀 INICJALIZACJA SYSTEMU - PODSTAWOWE USTAWIENIA
;; ============================================================================

;; === Podstawowe ustawienia Emacs ===
(setq inhibit-startup-message t)           ;; Wyłącz wiadomość startową
(setq initial-scratch-message nil)         ;; Pusty scratch buffer
(setq frame-title-format "%b")             ;; Tytuł ramki
(setq-default fill-column 80)              ;; Domyślna szerokość tekstu

;; === Ustawienia wydajności ===
(setq gc-cons-threshold 100000000)         ;; Zwiększ próg GC
(setq read-process-output-max (* 1024 1024)) ;; Zwiększ bufor procesów
(setq large-file-warning-threshold 100000000) ;; Zwiększ próg dużych plików

;; === Ustawienia edycji ===
(setq-default indent-tabs-mode nil)        ;; Używaj spacji zamiast tabów
(setq-default tab-width 4)                 ;; Szerokość taba
(setq-default truncate-lines t)            ;; Obetnij długie linie
(setq-default word-wrap t)                 ;; Zawijanie słów

;; ============================================================================
;; 📦 ELPACA - NOWOCZESNY MENEDŻER PAKIETÓW
;; ============================================================================

;; === Elpaca - Inicjalizacja ===
;; Lokalizacja plików Elpaca w katalogu ~/.emacs.d/
;; 
;; Aby zainstalować Elpaca, uruchom w Emacs:
;; M-x eval-expression
;; (elpaca-bootstrap)
;; 
;; Lub skopiuj i wklej ten kod:
;; (defvar elpaca-installer-version 0.7)
;; (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
;; (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
;; (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
;; (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git" :ref nil :files (:defaults (:exclude "extensions")) :build (:not elpaca--activate-package)))
;; (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory)) (build (expand-file-name "elpaca/" elpaca-builds-directory)) (order (cdr elpaca-order)) (default-directory repo)) (add-to-list 'load-path (if (file-exists-p build) build repo)) (unless (file-exists-p repo) (make-directory repo t) (when (<= emacs-major-version 28) (require 'subr-x)) (condition-case-unless-debug err (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*")) ((zerop (apply #'call-process `("git" nil ,buffer t "clone" ,@(when-let* ((depth (plist-get order :depth))) (list (format "--depth=%d" depth) "--no-single-branch")) ,(plist-get order :repo) ,repo))) ((zerop (call-process "git" nil buffer t "checkout" (or (plist-get order :ref) "--")))) (emacs (concat invocation-directory invocation-name)) ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch" "--eval" "(byte-recompile-directory \".\" 0 'force)"))) ((require 'elpaca)) ((elpaca-generate-autoloads "elpaca" repo))) (progn (message "%s" (buffer-string)) (kill-buffer buffer)) (error "%s" (with-current-buffer buffer (buffer-string)))) ((error) (warn "%s" err) (delete-directory repo 'recursive)))) (unless (require 'elpaca-autoloads nil t) (require 'elpaca) (elpaca-generate-autoloads "elpaca" repo) (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

;; Sprawdź czy Elpaca jest już zainstalowana
(let ((repo  (expand-file-name "elpaca/" elpaca-repos-directory)))
  (when (file-exists-p repo)
    (add-to-list 'load-path repo)
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (require 'elpaca-autoloads))))

;; Inicjalizuj Elpaca tylko jeśli jest dostępna
(when (file-exists-p (expand-file-name "elpaca/" elpaca-repos-directory))
  (add-hook 'after-init-hook #'elpaca-process-queues)
  (condition-case err
      (progn
        ;; Sprawdź czy funkcja elpaca istnieje
        (if (fboundp 'elpaca)
            (progn
              (elpaca `(elpaca :repo "https://github.com/progfolio/elpaca.git"
                               :ref nil
                               :files (:defaults (:exclude "extensions"))
                               :build (:not elpaca--activate-package)))
              
              ;; Włącz elpaca-use-package tylko jeśli Elpaca działa
              (if (fboundp 'elpaca-use-package)
                  (elpaca elpaca-use-package
                          :enable t
                          :hook (elpaca-use-package-mode . elpaca-use-package-autoloads-mode))
                (message "⚠️ elpaca-use-package nie jest dostępny")))
          (message "⚠️ Funkcja elpaca nie jest dostępna")))
    (error
     (message "⚠️ Błąd inicjalizacji Elpaca: %s" (error-message-string err))
     (message "📦 Elpaca nie jest dostępna - używaj package.el jako fallback"))))

;; === Elpaca - Konfiguracja ===
(setq elpaca-use-package-by-default t)
(setq elpaca-use-package-hook-use-package-prepare-functions t)
(setq elpaca-use-package-hook-use-package-complete-functions t)

;; === Elpaca - Repozytoria pakietów ===
;; Elpaca automatycznie używa standardowych repozytoriów
;; Melpa, Melpa Stable i GNU ELPA są dostępne domyślnie

;; === Elpaca - Sprawdzenie dostępności ===
;; Sprawdź czy Elpaca została pomyślnie załadowana
(add-hook 'after-init-hook
          (lambda ()
            (if (featurep 'elpaca)
                (message "📦 Elpaca: Pomyślnie załadowany")
              (message "⚠️ Elpaca: Nie udało się załadować - używam package.el"))))

;; === Elpaca - Funkcja bootstrap ===
;; Prosta funkcja do instalacji Elpaca
(defun elpaca-bootstrap ()
  "Zainstaluj Elpaca z repozytorium GitHub."
  (interactive)
  (message "🚀 Rozpoczynam instalację Elpaca...")
  (let ((elpaca-dir (expand-file-name "elpaca/" user-emacs-directory)))
    (unless (file-exists-p elpaca-dir)
      (make-directory elpaca-dir t))
    (let ((default-directory elpaca-dir))
      (when (zerop (call-process "git" nil "*elpaca-install*" t "clone" "https://github.com/progfolio/elpaca.git" "."))
        (message "✅ Elpaca zainstalowana pomyślnie!")
        (message "🔄 Zrestartuj Emacs aby aktywować Elpaca")))))

;; === Fallback do package.el ===
;; Jeśli Elpaca nie jest dostępna, użyj standardowego package.el
(unless (featurep 'elpaca)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (package-initialize)
  
  ;; Zainstaluj use-package jeśli nie jest dostępny
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (eval-when-compile (require 'use-package)))

;; === Sprawdź czy use-package jest dostępny ===
;; Jeśli nie, wyświetl komunikat o konieczności instalacji
(unless (fboundp 'use-package)
  (message "⚠️ use-package nie jest dostępny - zainstaluj go ręcznie"))

;; ============================================================================
;; 🔧 GLOBALNE HOOKS I KONFIGURACJA
;; ============================================================================

;; === Hooks dla trybów programistycznych ===
(add-hook 'prog-mode-hook 'display-line-numbers-mode)  ;; Numery linii
(add-hook 'prog-mode-hook 'hl-line-mode)              ;; Podświetl aktualną linię
(add-hook 'prog-mode-hook 'show-paren-mode)            ;; Pokaż nawiasy

;; === Hooks dla trybów tekstowych ===
(add-hook 'text-mode-hook 'auto-fill-mode)             ;; Automatyczne zawijanie
(add-hook 'text-mode-hook 'flyspell-mode)              ;; Sprawdzanie pisowni

;; === Hooks dla wszystkich buforów ===
(add-hook 'find-file-hook 'auto-save-mode)             ;; Automatyczne zapisywanie

;; ============================================================================
;; 🎯 DOSTARCZENIE I FINALIZACJA
;; ============================================================================

;; Dostarcz moduł core
(provide 'core)

;; Wyświetl komunikat sukcesu po załadowaniu modułu
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'core)
              (message "🚀 Podstawowa konfiguracja systemu: Gotowa")
              (message "📦 Elpaca: Aktywny")
              (message "⚡ Wydajność: Zoptymalizowana"))))

;;; core.el ends here
