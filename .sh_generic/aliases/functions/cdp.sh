#!/bin/sh
function cdp () {
	depth=$([ -n "$1" ] && echo "$1" || echo 3)

	# Sort by the depth of the path
	find -maxdepth $depth -type d | awk '{print $1 " / " gsub("/", $1)}' | sed -r 's/(.*) \/ (.*)/\2 \/ \1/g' | sort -n | sed -r 's/(.*) \/ (.*)/\2/g' | eval "fzf $FZF_CASUAL_OPTIONS" | read target_dir

	if [ "$?" -eq 0 ] ; then
		cd $target_dir
	fi
}
