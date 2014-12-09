#!/bin/bash
source ./utils.sh

# Confirm
echo 'Realy remove some links in $HOME ?(y/n)'
while true ; do
	read confirm
	if [ "$confirm" = "n" ] ; then
		echo 'Abort.'
		exit 0
	elif [ "$confirm" = "y" ] ; then
		break
	fi
	echo '(y) or (n).'
done


# Start
echo 'Remove some links.'
echo '-------------------'

# Remove links
dotdir=`dirname ${0}`
ignorefiles=`formatIgnoreFiles`
dotfiles=`ls -A $dotdir | grep -v -E "$ignorefiles"`

for linkName in $dotfiles ; do
	targetLink=$HOME/$linkName

	if [ -e "$targetLink" ] ; then
		echo ">> removing [${targetLink}]"
		rm $targetLink
		if [ $? -eq 0 ] ; then
			echo "  >> removed link [${targetLink}]."
		else
			echo "  >> cannot removing link [${targetLink}]."
		fi
	else
		echo ">> Skip no exist link [${targetLink}]."
	fi
	echo
done

echo '-------------------'
echo 'done.'
