#!/bin/bash

interval=$([[ -z "$1" ]] && echo 5 || echo "$1")

for (( minutes = 0; minutes < "$interval"; minutes++ )) ; do
    echo "$((minutes + 1)) minutes / $interval"
    sleep 1m
done

echo 'the rest time finished!'
which notify-send > /dev/null 2>&1 \
    && notify-send 'the rest time finished!'
date

if uname -a | grep Microsoft > /dev/null ; then
    cd ~/Desktop && cmd.exe /c start Music/初手は丸.mp3
elif [[ $(uname) = 'Darwin' ]] ; then
    open ~/Music/初手は丸.mp3
else
    mpg123 ~/Music/初手は丸.mp3 > /dev/null
fi
