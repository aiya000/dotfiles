#!/bin/bash

rm /tmp/pomodoro-* 2> /dev/null || true
rm /tmp/zsh-has-loaded 2> /dev/null || true

# shellcheck disable=SC1090
source "$HOME/.dotfiles/root/etc/profile.d/zdotroot.sh"

cd || exit 1
tmux
