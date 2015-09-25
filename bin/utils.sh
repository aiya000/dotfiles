#!/bin/sh -eu

# Return Formatted of Regex ignore linking files list
DOT_DIR="${HOME}/.dotfiles"
IGNORE_LIST_FILE="${DOT_DIR}/bin/link_exclude.txt"

grepformat_ignorefiles () {
	ignoreFormat=''

	if [ -f "$IGNORE_LIST_FILE" ] ; then
		# First one, append "^(" to $ignoreFormat
		ignoreFormat="^(`cat ${IGNORE_LIST_FILE} | head -1`"

		# Ignored first one
		tailNum=`cat $IGNORE_LIST_FILE | wc -l | xargs -I ? expr ? - 1`
		files=`cat $IGNORE_LIST_FILE | tail -$tailNum`

		# Build format
		for file in $files ; do
			ignoreFormat="${ignoreFormat}|${file}"
		done

		# Close format
		ignoreFormat="${ignoreFormat})$"
	fi

	echo $ignoreFormat
}

#TODO: implement by the array type
words_contains () {
	words=$1
	elem=$2

	for word in $words ; do
		if [ -n "`echo ${elem} | grep ${word}`" ] ; then
			echo 1
			return
		fi
	done
	echo 0
}
