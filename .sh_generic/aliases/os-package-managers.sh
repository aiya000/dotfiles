#!/bin/bash

if i_have yay ; then
    alias pai='yay -S'
    alias pas='yay -Ss'
    alias par='yay -Rns'
    alias pau='yay -Sy'
elif i_have pacman ; then
    alias pai='pacman -S'
    alias pas='pacman -Ss'
    alias par='pacman -Rns'
    alias pau='pacman -Sy'
elif i_have apt ; then
    alias pai='apt install'
    alias pas='apt search'
    alias pau='apt purge'
    alias pau='apt update'
fi
