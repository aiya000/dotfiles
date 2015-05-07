#!/bin/sh

# I use to command line method scriptalized to this.
# This was simply implemented.

if [ -n "$1" ] ; then
	subname="$1"
else
	subname="`date +'%Y-%m-%d'`"
fi

cd ~/.tmp
file_name="dotfiles-${subname}.tar.gz"

dropbox_uploader.sh download Room/$file_name
tar zxvf $file_name

cd ~/.tmp/.dotfiles
git log | head -100 | vim -c 'setl buftype=nofile' - \
	&& cd ~/.tmp \
	&& rm $file_name \
	&& rm -rf ~/.tmp/.dotfiles
