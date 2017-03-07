#!/bin/env zsh

# Define your favorite options
myFzf () {
	fzf --cycle --bind=ctrl-j:accept,ctrl-k:kill-line
}

list=$(nmcli device wifi)
itemNum=$(echo $list | wc -l)
# Show fzf
selectedItem=$(echo $list | tail -n $(expr $itemNum - 1) | myFzf | sed 's/^\*//' | awk '{print $1, $8}')
# Extract values
selectedAp=$(echo $selectedItem | awk '{print $1}')
passwordNeeded=$(echo $selectedItem | awk '{print $2}')

# Abort if fzf is aborted
if [ -z "$selectedItem" ] ; then
	exit 1
fi

# Require password if needed
if [ "$passwordNeeded" != "--" ] ; then
	echo -n "password: "
	read -s password
	passwordArgument="password $password"
fi

# Apply connecting to selected AP
nmcli device wifi connect ${selectedAp} ${passwordArgument}
