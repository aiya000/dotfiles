#!/bin/bash

# shellcheck disable=SC1090
source "$(dirname "$0")"/pomodoro-utils.sh

interval=$([[ -z "$1" ]] && echo 5 || echo "$1")

for (( minutes = 0; minutes < "$interval"; minutes++ )) ; do
    echo "$((minutes + 1)) minutes / $interval"
    sleep 1m
done

date
notify 'the rest time finished!'
play ~/Music/初手は丸.mp3
