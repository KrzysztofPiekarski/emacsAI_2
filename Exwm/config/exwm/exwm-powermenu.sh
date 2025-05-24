#!/bin/bash
# ~/.config/exwm/powermenu.sh - Menu zasilania z użyciem rofi

options="Zablokuj ekran\nWyloguj\nUśpij\nUruchom ponownie\nWyłącz"

chosen=$(echo -e "$options" | rofi -dmenu -i -p "Zasilanie" -theme ~/.config/rofi/powermenu.rasi)

case "$chosen" in
    "Zablokuj ekran")
        slock
        ;;
    "Wyloguj")
        pkill emacs
        ;;
    "Uśpij")
        systemctl suspend
        ;;
    "Uruchom ponownie")
        systemctl reboot
        ;;
    "Wyłącz")
        systemctl poweroff
        ;;
esac