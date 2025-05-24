#!/bin/bash
# ~/.config/exwm/autostart.sh - Skrypt uruchamiający aplikacje po starcie EXWM

# Funkcja do sprawdzania czy program jest zainstalowany
is_installed() {
    command -v "$1" &>/dev/null
}

# Funkcja do uruchamiania programów, tylko jeśli są zainstalowane
run_if_installed() {
    if is_installed "$1"; then
        "$@" &
    fi
}

# Funkcja do logowania zdarzeń
log_event() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> ~/.config/exwm/autostart.log
}

log_event "Rozpoczęcie skryptu autostart.sh"

# Ustawienie tła pulpitu
run_if_installed nitrogen --restore
log_event "Ustawiono tło pulpitu"

# Kompozytor do efektów przezroczystości
if is_installed picom; then
    picom -b --config ~/.config/picom/picom.conf &
    log_event "Uruchomiono picom"
elif is_installed compton; then
    compton -b &
    log_event "Uruchomiono compton"
fi

# Demon powiadomień
run_if_installed dunst &
log_event "Uruchomiono dunst"

# Applety systemowe dla traya
run_if_installed nm-applet &                # Sieć
run_if_installed volumeicon &               # Dźwięk
run_if_installed cbatticon -u 20 &          # Bateria
run_if_installed parcellite &               # Schowek
run_if_installed blueman-applet &           # Bluetooth
run_if_installed system-config-printer-applet &  # Drukarki

log_event "Uruchomiono applety systemowe"

# Ustawienie klawiatury
setxkbmap pl &
log_event "Ustawiono układ klawiatury pl"

# Odblokowanie menedżera kluczy
if is_installed gnome-keyring-daemon; then
    /usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh &
    log_event "Uruchomiono gnome-keyring-daemon"
fi

# Uruchomienie Polybar (opcjonalnie)
if is_installed polybar; then
    # Zabij wszystkie istniejące instancje polybara
    killall -q polybar
    
    # Poczekaj aż procesy zostaną zakończone
    while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
    
    # Uruchomienie polybara
    polybar exwm-bar &
    log_event "Uruchomiono polybar"
fi

# Upewnij się, że skrypty są wykonywalne
chmod +x ~/.config/exwm/powermenu.sh
log_event "Nadano uprawnienia wykonywania dla skryptów"

# Inne aplikacje autostartu
# run_if_installed firefox &
# run_if_installed slack &

log_event "Zakończono skrypt autostart.sh"

exit 0