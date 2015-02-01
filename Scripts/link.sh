#!/bin/bash
source ./utils.sh

dotdir=$HOME/.dotfiles
backupdir=$dotdir/.backup/`date +'%Y-%m-%d'`
usage() {  #{{{
		echo 'This script is some dotfiles easialy linking to HOME.'
		echo
		echo 'Options :'
		echo '-s, --skip-private   skip linking private files.'
		echo '-h, --help           view this help.'
}  #}}}

# Parse Option
privateFlag=0
for arg in "$@" ; do
	if [ "$arg" = "-s" -o "$arg" = "--skip-private" ] ; then
		privateFlag=1
	elif [ "$arg" = "-h" -o "$arg" = "--help" ] ; then
		usage
		exit 0
	fi
done


# Confirm
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


# Start
echo 'Linking some files.'
echo '-------------------'

# Check directroy for shunt existing file
if [ ! -d $backupdir ] ; then
	mkdir $backupdir
fi


# Linking dotfiles in HOME directory.
if [ "$dotdir" = "." ] ; then
	dotdir=`pwd`
fi

ignorefiles=`formatIgnoreFiles`
if [ $privateFlag -eq 1 ] ; then
	echo "TODO"
fi

dotfiles=`ls -A $dotdir | grep -v -E "^($ignorefiles)$"`

for fileName in $dotfiles ; do
	fromFile=$dotdir/$fileName
	toFile=$HOME/$fileName

	echo ">> linking [${fromFile}] -> [${toFile}]"
	if [ -f "$toFile" ] ; then
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
