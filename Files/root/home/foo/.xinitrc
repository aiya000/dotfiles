#!/bin/bash

# - Target platforms
#  - Arch Linux

# Fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export DefaultIMModule=fcitx

# Show 2byte chars correctly in libvte
export VTE_CJK_WIDTH=1

if [ -f ~/.Xmodmap ] ; then
	xmodmap ~/.Xmodmap
fi

## xfce4
#exec startxfce4

## Plasma
#exec startkde

## xmonad (standalone)
#xsetroot -cursor_name left_ptr
#setxkbmap -layout jp
#fcitx &
#xfce4-clipman &
#dunst &
#pulseaudio --daemonize=true --start &
#xscreensaver &
#exec ~/.local/bin/xmonad-config

# xmonad + another window manager
setxkbmap -layout us -option ctrl:swapcaps
exec stack exec xmonad-config
