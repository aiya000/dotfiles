#!/bin/bash

# - Target platforms
#  - Arch Linux

# Fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export DefaultIMModule=fcitx

# Show 2byte chars correctly in libvte
export VTE_CJK_WIDTH=double

# Use surface pro3 touch screen
# See https://wiki.archlinuxjp.org/index.php/%E3%82%BF%E3%83%83%E3%83%81%E3%82%B9%E3%82%AF%E3%83%AA%E3%83%BC%E3%83%B3#.E3.83.9E.E3.83.AB.E3.83.81.E3.83.87.E3.82.A3.E3.82.B9.E3.83.97.E3.83.AC.E3.82.A4.E3.81.A7.E3.82.BF.E3.83.83.E3.83.81.E3.82.B9.E3.82.AF.E3.83.AA.E3.83.BC.E3.83.B3.E3.82.92.E4.BD.BF.E3.81.86
export MOZ_USE_XINPUT2=1

# Be used in under the ~/.xmonad
export XMONAD_CONFIG_TOUCHPAD_DEVICE_NAME='Microsoft Surface Type Cover Touchpad'
export XMONAD_CONFIG_SETXKBMAP_OPTIONS='-option caps:ctrl_modifier'

# For gamepad
#export SDL_GAMECONTROLLERCONFIG='03000000250900006688000000010000,WiseGroup.,Ltd JC-PS102U,platform:Linux,x:b3,a:b2,b:b1,y:b0,back:b9,start:b8,dpleft:a0,dpdown:a1,leftshoulder:b6,lefttrigger:b4,rightshoulder:b7,righttrigger:b5,leftstick:h0.4,rightstick:h0.0,leftx:h0.8,lefty:h0.1,rightx:a2,righty:a3,'

# Start gnome-keyring-daemon daemon for git-credential-gnome-keyring
# Please see https://wiki.archlinux.jp/index.php/GNOME_Keyring for more informations
eval "$(/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)"
export SSH_AUTH_SOCK

# Xorg general
if [ -f ~/.Xmodmap ] ; then
    xmodmap ~/.Xmodmap
fi

PATH=$PATH:$HOME/bin
xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
setxkbmap -layout us "$XMONAD_CONFIG_SETXKBMAP_OPTIONS" #setxkbmap -layout jp

## Start Xfce4 env
##fcitx &
##xfce4-clipman &
#pulseaudio --daemonize=true --start &
#dunst &
##xscreensaver &
## termite -e tmux
## tmux new-window 'nvim +"cd ~/.dotfiles" +terminal'
#~/bin/cvim_server.py &  # For cvim's editWithVim keymap
#exec startxfce4

# Start xmonad
fcitx &
fcitx-imlist --set fcitx-keyboard-us
dunst &
#stack exec xmobar &
#xscreensaver &
~/bin/cvim_server.py &  # For cvim's editWithVim keymap
#redshift &
#nightshift --daemon &
exec ~/.local/bin/xmonad-config
