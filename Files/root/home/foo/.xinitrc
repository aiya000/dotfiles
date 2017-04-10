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

# Use surface pro3 touch screen
# See https://wiki.archlinuxjp.org/index.php/%E3%82%BF%E3%83%83%E3%83%81%E3%82%B9%E3%82%AF%E3%83%AA%E3%83%BC%E3%83%B3#.E3.83.9E.E3.83.AB.E3.83.81.E3.83.87.E3.82.A3.E3.82.B9.E3.83.97.E3.83.AC.E3.82.A4.E3.81.A7.E3.82.BF.E3.83.83.E3.83.81.E3.82.B9.E3.82.AF.E3.83.AA.E3.83.BC.E3.83.B3.E3.82.92.E4.BD.BF.E3.81.86
export MOZ_USE_XINPUT2=1

# Be used by ~/.xmonad/bin/trackpad-toggle.sh
export XMONAD_CONFIG_TOUCHPAD_DEVICE_NAME='Microsoft Surface Type Cover Touchpad'


if [ -f ~/.Xmodmap ] ; then
	xmodmap ~/.Xmodmap
fi

# Start X env
xsetroot -cursor_name left_ptr
setxkbmap -layout us -option ctrl:swapcaps #setxkbmap -layout jp
#fcitx &
#xfce4-clipman &
pulseaudio --daemonize=true --start &
dunst &
#xscreensaver &
#stack exec xmonad-config &
#exec stack exec xmonad-config
# termite -e tmux
# tmux new-window 'nvim +"cd ~/.dotfiles" +terminal'
exec startxfce4
