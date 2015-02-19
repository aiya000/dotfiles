#!/bin/sh

# I use to command line method scriptalized to this.
# This was simply implemented.

cd ~/.tmp
file_name=dotfiles-`date +'%Y-%m-%d'`.tar.gz

dropbox_uploader.sh download Room/$file_name
tar zxvf $file_name

cd ~/.tmp/.dotfiles
git log | head -100 | vim - \
	&& cd ~/.tmp \
	&& rm $file_name \
	&& rm -rf ~/.tmp/.dotfiles
