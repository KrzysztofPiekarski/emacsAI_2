;;; exwm-config.el --- Konfiguracja EXWM zgodna z use-package i elpaca

;;; === EXWM (główna konfiguracja) ===
(use-package exwm
  :ensure (:host github :repo "ch11ng/exwm" :files ("exwm-systemtray.el"))
  :config
  (setq exwm-workspace-number 4)

  ;; Globalne skróty klawiszowe
  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-d] . (lambda ()
                       (interactive)
                       (start-process-shell-command "rofi" nil "rofi -show drun")))

          ;; Kontrola głośności
          ([XF86AudioRaiseVolume] . (lambda ()
                                      (interactive)
                                      (start-process-shell-command "vol-up" nil "pactl set-sink-volume @DEFAULT_SINK@ +5%")))
          ([XF86AudioLowerVolume] . (lambda ()
                                      (interactive)
                                      (start-process-shell-command "vol-down" nil "pactl set-sink-volume @DEFAULT_SINK@ -5%")))
          ([XF86AudioMute] . (lambda ()
                               (interactive)
                               (start-process-shell-command "vol-mute" nil "pactl set-sink-mute @DEFAULT_SINK@ toggle")))))

  ;; Nazwa bufora wg klasy / tytułu
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (unless (string= exwm-class-name "Emacs")
                (exwm-workspace-rename-buffer exwm-title))))

  ;; Autostart aplikacji i paneli
  (add-hook 'exwm-init-hook
            (lambda ()
              (start-process-shell-command "nitrogen" nil "nitrogen --restore")
              (start-process-shell-command "picom" nil "picom --config ~/.config/picom/picom.conf")
              (start-process-shell-command "nm-applet" nil "nm-applet")
              (start-process-shell-command "volumeicon" nil "volumeicon")
              (start-process-shell-command "blueman-applet" nil "blueman-applet")
              (start-process-shell-command "udiskie" nil "udiskie")
              (start-process-shell-command "flameshot" nil "flameshot &")
              (start-process-shell-command "trayer" nil "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --tint 0x000000 --height 24")))

  ;; Włącz EXWM
  (exwm-enable))

;;; === System tray ===
(use-package exwm-systemtray
  :after exwm
  :ensure (:host github :repo "ch11ng/exwm" :files ("exwm-systemtray.el"))
  :config
  (exwm-systemtray-enable))

;;; === Dynamiczne monitory (xrandr) ===
(use-package exwm-randr
  :after exwm
  :ensure (:host github :repo "ch11ng/exwm" :files ("exwm-systemtray.el"))
  :config
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "HDMI-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output HDMI-1 --right-of eDP-1 --auto")))
  (exwm-randr-enable))

;;; === Winum – szybkie przełączanie między buforami ===
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

;;; === Zegar i bateria ===
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode 1)
(display-battery-mode 1)

;;; === Pogoda – wttr.in ===
(defun show-weather ()
  "Pokaż pogodę z wttr.in w minibufferze."
  (interactive)
  (start-process-shell-command
   "weather" "*wttrin*" "curl wttr.in?format=3"))

(global-set-key (kbd "C-c w") 'show-weather)

(provide 'exwm-config)
;;; exwm-config.el ends here
