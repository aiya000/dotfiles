#!/bin/sh

##TODO{{{
# - auto merging files [here <-> local-old]
# - show updated files
# - auto vimdiff command many files
# - view new commit messages
##}}}

DROPBOX_COMMAND=$HOME/bin/dropbox_uploader.sh
DROPBOX_DOTFILES_DIR=dotfiles
TMP_DIR=$HOME/.tmp

echo '>> starting upload dropbox'
echo
if [ ! -d $TMP_DIR ] ; then
	mkdir -p  $TMP_DIR
	echo ">> auto created ${TMP_DIR}"
	echo
fi

echo '>> archiving start...'
upload_file="${TMP_DIR}/dotfiles-`date +'%Y-%m-%d'`.tar.gz"
tar zcvf $upload_file ../.dotfiles > /dev/null \
	&& echo '>> done' \
	|| (
		echo '>> happened problems'
		echo '>> exit upload'
		exit
	) 1>&2
echo

echo '>> uploading start...'
$DROPBOX_COMMAND upload $upload_file $DROPBOX_DOTFILES_DIR \
	&& echo '>> upload finished' \
	|| (
		echo '>> upload failed'
		echo '>> exit upload'
	) 1>&2

echo '>> done !'
