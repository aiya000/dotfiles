#!/bin/bash

# shellcheck disable=SC1090
source "$(dirname "$0")"/pomodoro-utils.sh

function read_count () {
  ( \
    find /tmp/ 2> /dev/null \
    | grep -Eo '/tmp/pomodoro-([0-9]+)' \
    || echo '/tmp/pomodoro-0' \
  ) \
  | sed -r 's;/tmp/pomodoro-([0-9]+);\1;'
}

function increase_count () {
  local count
  count=$(read_count)

  rm "/tmp/pomodoro-$count" 2> /dev/null
  touch "/tmp/pomodoro-$((count + 1))"
}

interval=$([[ -z "$1" ]] && echo 25 || echo "$1")
count=$(( $(read_count) + 1 )) # 1 based

played_sound=$( \
  [[ -f $DOTFILES_SH_POMODORO_NOTIFY_SOUND ]] \
    && echo $DOTFILES_SH_POMODORO_NOTIFY_SOUND \
    || echo ~/.dotfiles/Music/notify.mp3 \
)


echo "Let's start the $count-th working"
for (( minutes = 0; minutes < "$interval"; minutes++ )) ; do
  echo "$((minutes + 1)) minutes / $interval"
  sleep 1m
done

date
notify "the $count-th working time finished!"

increase_count
play ~/Music/にぃに.mp3
