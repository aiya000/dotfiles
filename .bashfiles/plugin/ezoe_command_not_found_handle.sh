#!/bin/bash

# refer http://cpplover.blogspot.jp/2015/05/blog-post_28.html

#FIXME: I cannot use this approach
#alias_function () {
#	eval "${1} () $(declare -f ${2} | sed 1d)"
#}
#
#alias_function _command_not_found_handle command_not_found_handle

# copy from default definition
_command_not_found_handle () {
	if [ -x /usr/lib/command-not-found ]; then
		/usr/lib/command-not-found -- "$1";
		return $?;
	else
		if [ -x /usr/share/command-not-found/command-not-found ]; then
			/usr/share/command-not-found/command-not-found -- "$1";
			return $?;
		else
			printf "%s: command not found\n" "$1" 1>&2;
			return 127;
		fi;
	fi
}

command_not_found_handle () {
	command=$1
	shift
	args=( "$@" )

	echo 'コマンドではない。'
	_command_not_found_handle "$command" "${args[@]}"
}
