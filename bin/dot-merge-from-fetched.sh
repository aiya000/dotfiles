#!/bin/bash

# include
DOT_DIR=$HOME/.dotfiles
BASE_DIR=$DOT_DIR/bin
source $BASE_DIR/utils.sh
unset DOT_DIR BASE_DIR


DIFF_DIR1=~/.dotfiles
DIFF_DIR2=~/.tmp/.dotfiles

files=`find $DIFF_DIR2 -type f | awk -F'.dotfiles/' '{print $2}'`

#TODO: Don't implement by hard coding
#TODO: implement by the array type
IGNORE_FILES='.git/ vim/.netrwhist'

#TODO: view all the different files

# Execute diff files
for file in $files ; do
	ignore_file=`words_contains "$IGNORE_FILES" $file`
	if [ $ignore_file -eq 1 ] ; then
		continue
	fi

	# detected difference
	file1=$DIFF_DIR1/$file
	file2=$DIFF_DIR2/$file
	if [ ! -f $file1 ] ; then
		# confirm copy
		echo "'${file1}' not found"
		read -p "Copy >> ${file2} to ${file1} ? (y/n) (n): " confirm

		if [ "$confirm" = "yes" -o "$confirm" = "y" ] ; then
			cp -r "$file2" "$file1"
		fi
	elif [ -f $file2 -a -n "`diff $file1 $file2`" ] ; then
		# confirm edit
		read -p "Edit >> ${file} ? (y/n) (n): " confirm

		if [ "$confirm" = "yes" -o "$confirm" = "y" ] ; then
			vimdiff $file1 $file2
		fi
		echo
	fi
done
