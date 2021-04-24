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
  if command -v wsl.exe > /dev/null && [[ -f ~/Windows/Music/にぃに.mp3 ]] ; then
    echo 'Using ~/Windows/Music/にぃに.mp3 instead of the specified argument.'
    cd ~/Windows && cmd.exe /c start Music/にぃに.mp3 || exit 1
  elif command -v wsl.exe > /dev/null ; then
    explorer.exe .  # Anyway, notify it.
  elif uname -a | grep Darwin > /dev/null ; then
    mpg123 "$1"
  else  # Linux
    vlc "$1" &
  fi
}
