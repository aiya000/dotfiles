#!/bin/bash

# Export default environment values
if [ -z "$HEREIS_PLACES_FILE" ]; then
	export HEREIS_PLACES_FILE=~/.bashrc_hereis_places
fi

# Register directory path to file, and Easily cd there
if [ -f "$HEREIS_PLACES_FILE" ] ; then
	source "$HEREIS_PLACES_FILE"
fi

# Check alias prefix
if [ -z "$HEREIS_ALIAS_PREFIX" ]; then
	export HEREIS_ALIAS_PREFIX='place_'
fi

# Reload registered names and paths
alias reload-places="source ${HEREIS_PLACES_FILE} && echo 'bash places reloaded'"

# View registered name and paths
alias places="cat ${HEREIS_PLACES_FILE} | awk -F'[= ]' '{print $ 2 \":\t\" $ 4}' | sed s/\'// | sort"

# Edit registered aliases
alias edit-places="vim ${HEREIS_PLACES_FILE} && reload-places"

# Define register command
function hereis () {
	place_name="`echo $1`"
	place_path="\"`pwd`\""
	alias_name="${HEREIS_ALIAS_PREFIX}${place_name}"
	alias_body="${alias_name}='cd ${place_path}'"
	var_name="`echo ${HEREIS_ALIAS_PREFIX}${place_name} | sed s/-/_/g`"
	var_body="${var_name}=${place_path}"

	echo "alias ${alias_body}" >> "$HEREIS_PLACES_FILE"
	echo "${var_body}"         >> "$HEREIS_PLACES_FILE"
	echo "here is '${1}'"
	reload-places
}

## Examples
# $ echo $HOME
#   => /home/aiya000
# $ cd ~ && hereis home-homu
#   => An alias  { place_home-homu='cd "/home/aiya000"' }
#        and
#      A shell variable { place_home_homu="/home/aiya000" }
#      was added to $HEREIS_PLACES_FILE
#
#
# $ export HEREIS_ALIAS_PREFIX='p_'
#
# $ cd ~/foo/bar && hereis foo-bar
#   => An alias  { p_foo-bar='cd "/home/aiya000/foo/bar"' }
#        and
#   => A shell variable { p_foo_bar="/home/aiya000/foo/bar" }
#      was added to $HEREIS_PLACES_FILE
