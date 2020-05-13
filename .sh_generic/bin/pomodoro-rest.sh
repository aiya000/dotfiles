#!/bin/bash

function notify () {
    if uname -a | grep Darwin > /dev/null ; then
        terminal-notifier -title 'pomodoro' -message "$1"
    elif command -v wsl.exe > /dev/null ; then
        : TODO
    else  # Linux
        notify-send "$1"
    fi
}

function play () {
    if command -v wsl.exe > /dev/null ; then
        cd ~/Windows && cmd.exe /c start Music/初手は丸.mp3
    else  # Linux and macOS
        mpg123 ~/Music/初手は丸.mp3
    fi
}

interval=$([[ -z "$1" ]] && echo 5 || echo "$1")

for (( minutes = 0; minutes < "$interval"; minutes++ )) ; do
    echo "$((minutes + 1)) minutes / $interval"
    sleep 1m
done

echo 'the rest time finished!'
notify 'the rest time finished!'

date
play
