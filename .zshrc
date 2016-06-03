#!/bin/zsh

if [ -f ~/.bash_profile ] ; then
	source ~/.bash_profile
fi

if [ -f ~/.bashrc ] ; then
	source ~/.bashrc
fi

autoload -U colors
colors

setopt inc_append_history
setopt share_history


# added by travis gem
[ -f /home/aiya000/.travis/travis.sh ] && source /home/aiya000/.travis/travis.sh
