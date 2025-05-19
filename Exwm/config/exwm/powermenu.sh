#!/bin/bash

choice=$(printf "🔒 Zablokuj\n🕹️ Wyloguj\n🔁 Restart\n⏻ Wyłącz\n❌ Anuluj" | rofi -dmenu -p "Co chcesz zrobić?" -i)

case "$choice" in
  "🔒 Zablokuj") i3lock ;;  # Lub xlock, slock – co masz zainstalowane
  "🕹️ Wyloguj") emacsclient -e "(save-buffers-kill-emacs)" ;;
  "🔁 Restart") systemctl reboot ;;
  "⏻ Wyłącz") systemctl poweroff ;;
  *) exit 0 ;;
esac
