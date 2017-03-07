#!/bin/env zsh

list=$(nmcli device wifi)
itemNum=$(echo $list | wc -l)
selectedItem=$(echo $list | tail -n $(expr $itemNum - 1) | fzf --exit-0 | sed 's/^\*//' | awk '{print $1, $8}')

# If nmcli couldn't find AP
if [ "$?" -ne 0 -o "$selectedItem" = '' ] ; then
	echo 'error :(' > /dev/stderr
	echo 'You may interuppted fzf or hardware wifi switch is disabled' > /dev/stderr
	exit 1
fi

# Extract values
selectedAp=$(echo $selectedItem | awk '{print $1}')
passwordNeeded=$(echo $selectedItem | awk '{print $2}')

# Require password if needed
if [ "$passwordNeeded" != "--" ] ; then
	echo -n "password: "
	read -s password
	passwordArgument="password $password"
fi

# Apply connecting to selected AP
eval "nmcli device wifi connect ${selectedAp} ${passwordArgument}"
