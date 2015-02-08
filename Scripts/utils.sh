#!/bin/sh


# Return Formatted of Regex ignore linking files list
DOT_DIR=$HOME/.dotfiles
ignoreListFile=$DOT_DIR/Scripts/link_exclude.txt

grepformat_ignorefiles () {
	ignoreFormat=''

	if [ -f $ignoreListFile ] ; then
		# First one, append "^(" to $ignoreFormat
		ignoreFormat="^(`cat $ignoreListFile | head -1`"

		# Ignored first one
		tailNum=`cat $ignoreListFile | wc -l | xargs -I ? expr ? - 1`
		files=`cat $ignoreListFile | tail -$tailNum`

		# Build format
		for file in $files ; do
			ignoreFormat="${ignoreFormat}|${file}"
		done

		# Close format
		ignoreFormat="${ignoreFormat})$"
	fi
	
	echo $ignoreFormat
}
