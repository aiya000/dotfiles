#!/bin/sh

DIFF_DIR1=.
DIFF_DIR2=~/.tmp/.dotfiles

files=`ls -A`

for file in $files ; do
	file1="${DIFF_DIR1}/${file}"
	file2="${DIFF_DIR2}/${file}"
	if [ -f $file -a -n "`diff $file1 $file2`" ] ; then
		echo "Edit >> ${file}"
		vimdiff $file1 $file2
		echo
	fi
done
