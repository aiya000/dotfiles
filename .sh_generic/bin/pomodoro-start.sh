#!/bin/bash

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

function play () {
  if command -v wsl.exe > /dev/null ; then
    # WSL
    '/mnt/c/Program Files (x86)/Windows Media Player/wmplayer.exe' "$1"
  elif uname -a | grep Darwin > /dev/null ; then
    # macOS
    # TODO: Take the monitor focus forcely
    mpg123 "$1"
  else
    # Linux
    vlc "$1" &
  fi
}

function get_notification_sound () {
  if [[ -f $DOTFILES_SH_POMODORO_NOTIFY_SOUND ]] ; then
    echo "$DOTFILES_SH_POMODORO_NOTIFY_SOUND"
  elif command -v wsl.exe > /dev/null ; then
   wslpath -m ~/Windows/Music/notify.mp3
 else
   echo ~/.dotfiles/Music/notify.mp3
  fi
}

interval=$([[ -z "$1" ]] && echo 25 || echo "$1")
count=$(( $(read_count) + 1 )) # `+ 1` to count by 1 based

echo "Let's start the $count-th working"
for (( minutes = 0; minutes < "$interval"; minutes++ )) ; do
  echo "$((minutes + 1)) minutes / $interval"
  sleep 1m
done

date
echo "the $count-th working time finished!"
increase_count

play "$(get_notification_sound)"
