#!/bin/sh

#
# If you want to execute this,
# you must be execute here (dotfiles directory).
#

##TODO{{{
# - auto merging files [here <-> local-old]
# - show updated files
# - auto vimdiff command many files
# - view new commit messages
##}}}


## Script Parameters
# path of dropbox_upload command
DROPBOX_COMMAND=`which dropbox_uploader.sh`

# remote upload directory path ( ~cloud_app_dir/$DROPBOX_DOTFILES_DIR )
DROPBOX_DOTFILES_DIR=Room

# target directory
DOT_DIR_NAME=.dotfiles
DOT_DIR_PATH=$HOME/$DOT_DIR_NAME

# path of temporary file output location
TMP_DIR=$HOME/.tmp

# exclude files by tar
TAR_OPT='--exclude .vim/bundle --exclude .backup'

# if true then do not remove archive
no_remove_archive=0


# Checking an argument
if [ "$1" = "-n" -o "$1" = "--no-remove-archive" ] ; then
	no_remove_archive=1
	echo 'option: no remove archive ... Enabled'
	echo
fi


### Script Start

echo '>> starting upload dropbox'
echo


# temporary directory auto create
if [ ! -d $TMP_DIR ] ; then
	mkdir -p $TMP_DIR
	echo ">> auto created ${TMP_DIR}"
	echo
fi


## Archiving dotfiles dir by tar.gz
echo '>> archiving start...'
upload_file="${TMP_DIR}/dotfiles-`date +'%Y-%m-%d'`.tar.gz"

cd $DOT_DIR_PATH/..
tar zcvf $upload_file ../../.dotfiles $TAR_OPT > /dev/null \
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
if [ $no_remove_archive -eq 0 ] ; then
	echo '>> removing archived file...'
	rm $upload_file \
		&& echo '>> removing succeed' \
		|| (
			echo '>> removing failed'
			echo '>> please manually remove a file'
		) 1>&2
	echo
fi


# Finished !
echo '>> done !'
