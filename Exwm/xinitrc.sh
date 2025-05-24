#!/bin/bash
# ~/.xinitrc dla EXWM

# Załadowanie zasobów X
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Włączenie wsparcia dla polskich znaków
setxkbmap pl

# Uruchomienie emacsa z konfiguracją EXWM
# Opcja --debug-init jest pomocna przy diagnostyce problemów
exec dbus-launch --exit-with-session emacs