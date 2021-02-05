#!/bin/bash

if i_have yay ; then
    alias pai='yay -S'     # Install
    alias pas='yay -Ss'    # Search
    alias par='yay -Rns'   # Remove
    alias pau='yay -Sy'    # Update
    alias pasu='yay -Syu'  # System Update
elif i_have pacman ; then
    alias pai='sudo pacman -S'
    alias pas='pacman -Ss'
    alias par='sudo pacman -Rns'
    alias pau='sudo pacman -Sy'
    alias pasu='sudo pacman -Syu'
elif i_have apt-fast ; then
    alias pai='sudo apt-fast install'
    alias par='sudo apt-fast purge'
    alias pas='apt-fast search'
    alias pau='sudo apt-fast update'
elif i_have apt ; then
    alias pai='sudo apt install'
    alias par='sudo apt purge'
    alias pas='apt search'
    alias pau='sudo apt update'
elif i_have brew ; then
    alias pai='brew install'
    alias pau='brew upgrade'
    alias par='brew uninstall'
    alias pas='brew search'
fi
