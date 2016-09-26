#!/bin/sh

# - Target platforms
#  - Arch Linux

# Fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export DefaultIMModule=fcitx

# xfce4
#exec startxfce4

# xmonad
xsetroot -cursor_name left_ptr
setxkbmap -layout jp
nitrogen --restore &
dunst &
xscreensaver &
exec xmonad
