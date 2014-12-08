#!/bin/sh
DROPBOX_COMMAND=$HOME/bin/dropbox_uploader.sh
DROPBOX_DOTFILES_DIR=dotfiles
TMP_DIR=$HOME/.tmp

echo '>> starting upload dropbox'
echo
if [ ! -d $TMP_DIR ] ; then
	mkdir -p  $TMP_DIR
fi

upload_file="${TMP_DIR}/dotfiles-`date +'%Y-%m-%d'`.tar.gz"
tar zcvf $upload_file ../.dotfiles # > /dev/null
$DROPBOX_COMMAND upload $upload_file $DROPBOX_DOTFILES_DIR \
	&& echo '>> upload finished' \
	|| echo '>> upload failed' 1>&2
