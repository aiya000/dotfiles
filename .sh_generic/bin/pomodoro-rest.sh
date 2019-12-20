#!/bin/bash

function notify () {
    if uname -a | grep Darwin > /dev/null ; then
        terminal-notifier -title 'pomodoro' -message "$1"
    elif uname -a | grep Microsoft > /dev/null ; then
        : TODO
    else  # Linux
        notify-send "$1"
    fi
}

# TODO: Take an argument as a file path
function play () {
    if uname -a | grep Microsoft > /dev/null ; then
        cd ~/Desktop && cmd.exe /c start Music/にぃに.mp3
    else  # Linux and macOS
        mpg123 ~/Music/にぃに.mp3 > /dev/null
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
