#!/bin/bash

function notify () {
    echo "$1"

    if uname -a | grep Darwin > /dev/null ; then
        terminal-notifier -title 'pomodoro' -message "$1"
    elif command -v wsl.exe > /dev/null ; then
        # TODO
        echo 'NOTICE: notify is never implemented yet for WSL.'
    else  # Linux
        notify-send "$1"
    fi
}

function play () {
    if command -v wsl.exe > /dev/null ; then
        echo 'NOTICE: For WSL, only Music/にぃに.mp3 is used.'
        cd ~/Windows && cmd.exe /c start Music/にぃに.mp3
    elif uname -a | grep Darwin > /dev/null ; then
        mpg123 "$1"
    else  # Linux
        vlc "$1" &
    fi
}
