#!/bin/bash

function notify () {
  echo "$1"

  if uname -a | grep Darwin > /dev/null ; then
    terminal-notifier -title 'pomodoro' -message "$1"
  elif command -v wsl.exe > /dev/null ; then
    : Not necessary because 
  else  # Linux
    notify-send "$1"
  fi
}

function play () {
  if (command -v wsl.exe > /dev/null) && (command -v wslview > /dev/null) ; then
    # WSL2
    wslview "$1"
  elif command -v wsl.exe > /dev/null ; then
    # WSL
    explorer.exe .  # Anyway, notify it.
  elif uname -a | grep Darwin > /dev/null ; then
    # macOS
    # TODO: Take the monitor focus forcely
    mpg123 "$1"
  else
    # Linux
    vlc "$1" &
  fi
}
