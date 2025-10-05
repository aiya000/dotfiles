#!/bin/bash

# Starts WSL environment with tmux and zsh workflow

rm /tmp/pomodoro-* 2> /dev/null || true
rm /tmp/zsh-has-loaded 2> /dev/null || true

# shellcheck disable=SC1090
source ~/.dotfiles/root/etc/profile.d/zdotroot.sh

# if [[ ! -d /mnt/g/マイドライブ ]] ; then
#   sudo mount -t drvfs G: /mnt/g
# fi

if [[ ! -d '/mnt/p/pCloud Backup' ]] ; then
  sudo mount -t drvfs -o noatime,uid=1000,gid=1000 P: /mnt/p
fi

if [[ ! -f '/mnt/z/me.jpg' ]] ; then
  sudo mount -t drvfs -o noatime,uid=1000,gid=1000 Z: /mnt/z
fi

if [[ $(service cron status) = ' * cron is not running' ]] ; then
  sudo service cron start
fi

cd || exit 1
exec tmux
