;; Instalacja i konfiguracja EXWM z użyciem Elpaca
;; Ten fragment można dodać do swojej głównej konfiguracji Emacs

;; Upewnij się, że Elpaca jest już zainicjalizowana wcześniej w konfiguracji

;; Instalacja EXWM przez Elpaca
(elpaca '(exwm :host github
               :repo "ch11ng/exwm"
               :branch "master"))

;; Dodatkowe pakiety, które mogą być przydatne z EXWM
(elpaca '(desktop-environment
          :host github
          :repo "DamienCassou/desktop-environment"))

;; Czekaj na zainstalowanie pakietów
(elpaca-wait)

;; Załaduj konfigurację EXWM
(add-to-list 'load-path "~/.emacs.d/config/") ;; Dostosuj ścieżkę do swoich potrzeb
(load "exwm-config")

;; Opcjonalnie: Integracja z desktop-environment dla lepszej obsługi klawiszy multimedialnych
(use-package desktop-environment
  :after exwm
  :config
  (desktop-environment-mode)
  ;; Customizacja komend jeśli potrzebna
  (setq desktop-environment-brightness-small-increment "2%+")
  (setq desktop-environment-brightness-small-decrement "2%-")
  (setq desktop-environment-brightness-normal-increment "5%+")
  (setq desktop-environment-brightness-normal-decrement "5%-"))

;; Opcjonalnie: Integracja z EXWM i elpaca-use-package
(defun elpaca--exwm-startup ()
  "Inicjalizacja po uruchomieniu EXWM."
  ;; Kod inicjalizacji...
  )

(with-eval-after-load 'exwm
  (add-hook 'exwm-init-hook #'elpaca--exwm-startup))