;;; exwm-config.el --- Konfiguracja EXWM zgodna z use-package i elpaca

;; === EXWM (główna konfiguracja) ===
(use-package exwm
  :elpaca t
  :config
  (setq exwm-workspace-number 4)

  ;; Skróty globalne
  (setq exwm-input-global-keys
        `(
          ([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-d] . (lambda ()
                       (interactive)
                       (start-process "rofi" nil "rofi -show drun")))
          ;; Kontrola głośności
          ([XF86AudioRaiseVolume] . (lambda ()
                                      (interactive)
                                      (start-process-shell-command "vol-up" nil "pactl set-sink-volume @DEFAULT_SINK@ +5%")))
          ([XF86AudioLowerVolume] . (lambda ()
                                      (interactive)
                                      (start-process-shell-command "vol-down" nil "pactl set-sink-volume @DEFAULT_SINK@ -5%")))
          ([XF86AudioMute] . (lambda ()
                               (interactive)
                               (start-process-shell-command "vol-mute" nil "pactl set-sink-mute @DEFAULT_SINK@ toggle")))
          ))

  ;; Nazwa bufora według klasy/tytułu
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (unless (string= exwm-class-name "Emacs")
                (exwm-workspace-rename-buffer exwm-title))))

  ;; === Autostart aplikacji po uruchomieniu EXWM ===
  (add-hook 'exwm-init-hook
            (lambda ()
              ;; Tapeta (Nitrogen)
              (start-process-shell-command "nitrogen" nil "nitrogen --restore")

              ;; Kompozytor (Picom)
              (start-process-shell-command "picom" nil "picom --config ~/.config/picom/picom.conf")

              ;; D-Bus (opcjonalnie)
              ;; (start-process-shell-command "dbus" nil "eval `dbus-launch --sh-syntax`")

              ;; Aplety systemowe
              (start-process-shell-command "nm-applet" nil "nm-applet")
              (start-process-shell-command "volumeicon" nil "volumeicon")
              (start-process-shell-command "blueman-applet" nil "blueman-applet")
              (start-process-shell-command "udiskie" nil "udiskie")
              (start-process-shell-command "flameshot" nil "flameshot &")
              ))


  ;; Włącz EXWM
  (exwm-enable))

;; === System tray ===
(use-package exwm-systemtray
  :after exwm
  :config
  (exwm-systemtray-enable))

;; === Dynamiczne monitory ===
(use-package exwm-randr
  :after exwm
  :config
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "HDMI-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output HDMI-1 --right-of eDP-1 --auto")))
  (exwm-randr-enable))

;; === Winum – szybkie przełączanie między buforami/windows ===
(use-package winum
  :ensure t
  :config
  (winum-mode)
  (setq winum-auto-assign-0-to-minibuffer t
        winum-format " %s "
        winum-auto-assign-mode-line-keys nil)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4))

;; === Persp-mode – sesje i layouty ===
(use-package persp-mode
  :ensure t
  :init
  (setq persp-keymap-prefix (kbd "C-c M-p"))
  :config
  (persp-mode))

;; === Pasek: doom-modeline + zegar + bateria + pogoda ===
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-time t)
  (doom-modeline-battery t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project))

;; === Zegar + Bateria ===
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode 1)
(display-battery-mode 1)

;; === Pogoda – pogodynka z wttrin (opcja) ===
(defun show-weather ()
  (interactive)
  (start-process-shell-command
   "weather" "*wttrin*" "curl wttr.in?format=3"))

(global-set-key (kbd "C-c w") 'show-weather)

(provide 'exwm-config)
;;; exwm-config.el ends here
