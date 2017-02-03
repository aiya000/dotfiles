#!/bin/sh
function cdp () {
	local depth=$([ -n "$1" ] && echo "$1" || echo 5)

	# 0. Show the directories of under the `pwd`
	# 1. Ignore the files
	# 2. Convert the full path to the relative path
	# 3. Ignore it if its depth is over the $depth
	# and select it
	locate --null --regex "$(pwd)" \
		| xargs --null sh -c 'for i do [ -d "$i" ] && printf "%s\n" "$i"; done' 2> /dev/null \
		| cut -c $(pwd | wc -m | xargs -I _ expr _ + 1 )- \
		| cut -d'/' -f "-$(( ${depth} + 1 ))" | uniq \
		| eval "fzf $FZF_CASUAL_OPTIONS" \
		| read target_dir

	if [ "$?" -eq 0 ] ; then
		cd $target_dir
	fi
}
