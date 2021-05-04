#!/bin/bash

# Load this only once
[[ -n $SH_GENERIC_PREMISE_LOADED ]] && return

# If I have it command, return 0. otherwise return 1.
function i_have () {
  command -v "$1" > /dev/null 2>&1
}

# If I have a command what it is specified same as alias name,
# define the alias.
# otherwise don't define it.
function alias_of () {
  local name=$1 detail=$2

  if i_have "$name" ; then
    eval "alias $name=$detail"
  fi
}

function source_if_exists () {
  if [[ -f $1 ]] ; then
    # shellcheck disable=SC1090
    source "$1"
  fi
}

# Load this only once
SH_GENERIC_PREMISE_LOADED=LOADED
