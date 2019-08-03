#!/bin/bash
# Any scripts depends this
[ -n "$SH_GENERIC_PREMISE_LOADED" ] && return


#NOTE: I should rename this to have_i()
# If I have it command, return 0. otherwise return 1.
function i_have () {
    command -v "$1" > /dev/null 2>&1
}

# If I have a command what it is specified same as alias name,
# define the alias.
# otherwise don't define it.
function alias_of () {
    local alias_detail name
    alias_detail="$1"
    name=$(echo "$alias_detail" | awk -F = '{print $1}')
    i_have "$name" && alias "$alias_detail"
}

function source_if_exists () {
    if [[ -f $1 ]] ; then
        # shellcheck disable=SC1090
        source "$1"
    fi
}

SH_GENERIC_PREMISE_LOADED=LOADED
