#!/bin/bash

rm /tmp/pomodoro-* 2> /dev/null || true
rm /tmp/zsh-has-loaded 2> /dev/null || true

# shellcheck disable=SC1090
source "$HOME/.dotfiles/root/etc/profile.d/zdotroot.sh"

# if [[ ! -d /mnt/g/マイドライブ ]] ; then
#   sudo mount -t drvfs G: /mnt/g
# fi
if [[ ! -d '/mnt/p/pCloud Backup' ]] ; then
  sudo mount -t drvfs -o noatime,uid=1000,gid=1000 P: /mnt/p
fi

if [[ $(service cron status) = ' * cron is not running' ]] ; then
  sudo service cron start
fi

cd || exit 1
exec tmux
