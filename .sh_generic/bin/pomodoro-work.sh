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

function read_count () {
    ( find /tmp/ 2> /dev/null | grep -Eo '/tmp/pomodoro-([0-9]+)' || echo '/tmp/pomodoro-0' ) | sed -r 's;/tmp/pomodoro-([0-9]+);\1;'
}

function increase_count () {
    local count
    count=$(read_count)

    rm "/tmp/pomodoro-$count" 2> /dev/null
    touch "/tmp/pomodoro-$((count + 1))"
}

interval=$([[ -z "$1" ]] && echo 25 || echo "$1")
count=$(( $(read_count) + 1 )) # 1 based

echo "Let's start the $count-th working"
for (( minutes = 0; minutes < "$interval"; minutes++ )) ; do
    echo "$((minutes + 1)) minutes / $interval"
    sleep 1m
done

date
echo "a $count-th working time finished!"
notify "the $count-th working time finished!"

increase_count
play
