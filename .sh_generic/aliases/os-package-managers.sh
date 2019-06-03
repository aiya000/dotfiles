#!/bin/bash

if i_have yay ; then
    alias pai='yay -S'
    alias pas='yay -Ss'
    alias par='yay -Rns'
    alias pau='yay -Sy'
elif i_have pacman ; then
    alias pai='sudo pacman -S'
    alias pas='pacman -Ss'
    alias par='sudo pacman -Rns'
    alias pau='sudo pacman -Sy'
elif i_have brew ; then
    alias pai='brew install'
    alias par='brew uninstall'
    alias pas='brew search'
elif i_have apt ; then
    alias pai='sudo apt install'
    alias par='sudo apt purge'
    alias pas='apt search'
    alias pau='sudo apt update'
fi
