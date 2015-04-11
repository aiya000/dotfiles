#!/bin/bash

# Initialize variables

DOT_DIR=$HOME/.dotfiles
BASE_DIR=$DOT_DIR/Scripts

backupdir=$DOT_DIR/.backup/`date +'%Y-%m-%d'`

source $BASE_DIR/utils.sh

ignorefiles=`grepformat_ignorefiles`
dotfiles=`ls -A $DOT_DIR | grep -v -E "^($ignorefiles)$"`

yes_flag=`[ "$1" == "-y" ] && echo 1 || echo 0`


# Define usage
usage() {  #{{{
		echo 'This script is some dotfiles easialy linking to HOME.'
		echo
		echo 'Options :'
		echo '-s, --skip-private   skip linking private files.'
		echo '-h, --help           view this help.'
}  #}}}


# Confirm
if [ $yes_flag -eq 0 ] ; then
	echo 'Realy link some links in $HOME ?(y/n)'
	while true ; do
		read confirm
		if [ "$confirm" = "n" ] ; then
			echo 'Abort.'
			exit 0
		elif [ "$confirm" = "y" ] ; then
			break
		fi
		echo '(y) or (n).'
	done
fi


# Start
echo 'Linking some files.'
echo '-------------------'

# Check directroy for shunt existing file
if [ ! -d $backupdir ] ; then
	mkdir $backupdir
fi


# Linking dotfiles
for fileName in $dotfiles ; do
	fromFile=$DOT_DIR/$fileName
	toFile=$HOME/$fileName
	echo ">> linking [${fromFile}] -> [${toFile}]"

	# if dofiles already exists, avoid overwrite
	if [ -f $toFile ] ; then
		echo "  >> [${toFile}] is already exist ."
		mv $toFile $backupdir
		echo "    >> moved [${toFile}] to [${backupdir}/${file}] ."
	fi
	ln -s $fromFile $HOME &&
		echo ">> succeed ."
	echo
done

echo '-------------------'
echo 'done.'
