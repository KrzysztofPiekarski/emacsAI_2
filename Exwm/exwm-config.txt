;; exwm-config.el - Konfiguracja EXWM do integracji z lightdm i rofi

;; Instalacja pakietów przez Elpaca
(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (require 'exwm-systemtray)
  
  ;; Zaawansowana integracja z rofi jako launcherem aplikacji
(defun exwm-run-rofi ()
  "Uruchomienie rofi jako launchera aplikacji."
  (interactive)
  (start-process-shell-command "rofi" nil "rofi -show drun -theme ~/.config/rofi/config.rasi"))

(defun exwm-run-rofi-window ()
  "Przełączanie okien przez rofi."
  (interactive)
  (start-process-shell-command "rofi-window" nil "rofi -show window -theme ~/.config/rofi/config.rasi"))

(defun exwm-run-powermenu ()
  "Uruchomienie menu zasilania przez rofi."
  (interactive)
  (start-process-shell-command "power-menu" nil "~/.config/exwm/powermenu.sh"))

;; Podstawowa konfiguracja EXWM
(setq exwm-workspace-number 4)  ;; Liczba przestrzeni roboczych

;; Tworzenie bufora z nazwą aplikacji jako nazwą bufora
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;; Globalne klawisze, które działają zawsze
(setq exwm-input-global-keys
      `(
        ;; Przełączanie między przestrzeniami roboczymi: s-1, s-2, s-3, s-4
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,(- i 1)))))
                  (number-sequence 1 4))
        
        ;; Uruchomienie rofi jako launchera aplikacji
        (,(kbd "s-p") . exwm-run-rofi)
        
        ;; Przełączanie okien przez rofi
        (,(kbd "s-Tab") . exwm-run-rofi-window)
        
        ;; Reload konfiguracji
        (,(kbd "s-r") . exwm-reset)
        
        ;; Blokowanie ekranu
        (,(kbd "s-l") . (lambda ()
                          (interactive)
                          (start-process-shell-command "slock" nil "slock")))
        
        ;; Uruchomienie terminala
        (,(kbd "s-<return>") . (lambda ()
                                 (interactive)
                                 (start-process-shell-command "terminal" nil "alacritty")))
        
        ;; Zabijanie okna
        (,(kbd "s-q") . (lambda ()
                          (interactive)
                          (if exwm--id
                              (exwm-workspace-delete-window)
                            (delete-window))))
        
        ;; Przełączanie między trybami linii i znaków
        (,(kbd "s-i") . exwm-input-toggle-keyboard)))

;; Tryb pełnoekranowy
(setq exwm-randr-workspace-output-plist '(0 "eDP-1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP-1 --auto")))
(exwm-randr-enable)

;; Integracja z paskiem systemowym
(use-package exwm-systemtray
  :after exwm
  :config
  (setq exwm-systemtray-height 24)  ;; Wysokość paska systemowego
  (exwm-systemtray-enable))

;; Automatyczne przenoszenie aplikacji do konkretnych obszarów roboczych
(setq exwm-manage-configurations
      '(((equal exwm-class-name "Firefox")
         workspace 1)
        ((equal exwm-class-name "Thunderbird")
         workspace 2)))

;; Obsługa natywnych aplikacji X
(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])))

;; Włączenie EXWM
(exwm-enable)

;; Autostart aplikacji przy uruchomieniu
(defun exwm-autostart ()
  "Autostart aplikacji po uruchomieniu EXWM."
  (interactive)
  (start-process-shell-command "autostart" nil "~/.config/exwm/autostart.sh"))

(add-hook 'exwm-init-hook 'exwm-autostart)

;; Eksport zmiennych dla skryptów startowych
(provide 'exwm-config)