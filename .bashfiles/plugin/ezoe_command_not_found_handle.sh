#!/bin/bash

# refer http://cpplover.blogspot.jp/2015/05/blog-post_28.html

alias_function () {
	eval "${1} () $(declare -f ${2} | sed 1d)"
}

alias_function orig_command_not_found_handle command_not_found_handle

command_not_found_handle () {
	command=$1
	shift
	args=( "$@" )

	echo 'コマンドではない。'
	orig_command_not_found_handle "$command" "${args[@]}"
}
