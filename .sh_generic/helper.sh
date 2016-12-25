#!/bin/sh

#
# If you use the file in this directory,
# You must load this.
#

# If I have it command, return 0. otherwise return 1.
function i_have () {
	which "$1" > /dev/null 2>&1
}

# If I have a command what it is specified same as alias name,
# define the alias.
# otherwise don't define it.
function alias_of () {
	alias_detail=$1
	name=$(echo $alias_detail | awk -F = '{print $1}')
	i_have "$name" && alias $alias_detail
}
