#!/bin/sh

DIFF_DIR1=~/.dotfiles
DIFF_DIR2=~/.tmp/.dotfiles

files=`find ~/.tmp/.dotfiles/ -type f | awk -F'.dotfiles/' '{print $2}'`

for file in $files ; do
	file1=$DIFF_DIR1/$file
	file2=$DIFF_DIR2/$file
	if [ -f $file2 -a -n "`diff $file1 $file2`" ] ; then
		echo "Edit >> ${file}"
		vimdiff $file1 $file2
		echo
	fi
done
