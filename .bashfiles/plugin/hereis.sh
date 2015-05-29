#!/bin/bash


# Export default environment values
if [ -z "$HEREIS_PLACES_FILE" ]; then
	export $HEREIS_PLACES_FILE=~/.bashrc_hereis_places
fi


# Register directory path to file, and Easily cd there
if [ -f "$HEREIS_PLACES_FILE" ] ; then
	source "$HEREIS_PLACES_FILE"
fi

# Reload registered names and paths
alias reload-places="source ${HEREIS_PLACES_FILE} && echo 'bash places reloaded'"

# View registered name and paths
alias places="cat ${HEREIS_PLACES_FILE} | awk -F'[= ]' '{print $ 2 \":\t\" $ 4}' | sed s/\'// | sort"

# Edit registered aliases
alias edit-places="vim ${HEREIS_PLACES_FILE} && reload-places"

# Define register command
function hereis () {
	place_name=$1
	alias_body="${place_name}='cd \"`pwd`\"'"

	echo "alias ${alias_body}" >> "$HEREIS_PLACES_FILE"
	echo "here is '${1}'"

	reload-places
}
