#!/bin/sh

# - Target platforms
#  - Arch Linux

# Fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export DefaultIMModule=fcitx

# Show 2byte chars correctly in libvte
export VTE_CJK_WIDTH=1

# xmonad
xsetroot -cursor_name left_ptr
setxkbmap -layout jp
fcitx &
xfce4-clipman &
dunst &
pulseaudio --daemonize=true --start &
xscreensaver &
exec xmonad
#exec startxfce4
