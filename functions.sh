#!/bin/sh


# Return Formatted of Regex ignore linking files list
ignoreListFile=./ignore_link.txt

getRXFormatIgnoreFiles () {
	ignoreFormat=''

	if [ -f $ignoreListFile ] ; then
		ignoreFormat="^(`cat $ignoreListFile | head -1`"
		tailNum=`cat $ignoreListFile | wc -l | xargs -I ? expr ? - 1`
		files=`cat $ignoreListFile | tail -$tailNum`

		for file in $files ; do
			ignoreFormat="${ignoreFormat}|${file}"
		done

		ignoreFormat="${ignoreFormat})$"
	fi
	
	echo $ignoreFormat
}

