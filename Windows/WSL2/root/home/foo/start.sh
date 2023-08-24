#!/bin/bash

rm /tmp/pomodoro-* 2> /dev/null || true
rm /tmp/zsh-has-loaded 2> /dev/null || true

# shellcheck disable=SC1090
source "$HOME/.dotfiles/root/etc/profile.d/zdotroot.sh"

if [[ ! -d /mnt/g/マイドライブ ]] ; then
  sudo mount -t drvfs G: /mnt/g
fi

if [[ $(service cron status) = ' * cron is not running' ]] ; then
  sudo service cron start
fi

cd || exit 1
exec tmux
