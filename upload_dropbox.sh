#!/bin/sh

##TODO{{{
# - auto merging files [here <-> local-old]
# - show updated files
# - auto vimdiff command many files
# - view new commit messages
##}}}


## Script Parameters
# path of dropbox_upload command
DROPBOX_COMMAND=$HOME/bin/dropbox_uploader.sh
# remote upload directory path ( ~cloud_app_dir/$DROPBOX_DOTFILES_DIR )
DROPBOX_DOTFILES_DIR=dotfiles
# path of temporary file output location
TMP_DIR=$HOME/.tmp


### Script Start

echo '>> starting upload dropbox'
echo


# temporary directory auto create
if [ ! -d $TMP_DIR ] ; then
	mkdir -p  $TMP_DIR
	echo ">> auto created ${TMP_DIR}"
	echo
fi


## Archiving dotfiles dir by tar.gz
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


## Uploading archived file
echo '>> uploading start...'
$DROPBOX_COMMAND upload $upload_file $DROPBOX_DOTFILES_DIR \
	&& echo '>> upload finished' \
	|| (
		echo '>> upload failed'
		echo '>> exit upload'
	) 1>&2
echo


## Removing temporary file
echo '>> removing archived file...'
rm $upload_file \
	&& echo '>> removing succeed' \
	|| (
		echo '>> removing failed'
		echo '>> please manually remove a file'
	) 1>&2
echo


# Finished !
echo '>> done !'
