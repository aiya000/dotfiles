#!/bin/bash

# Set default environment values
if [ -z "$FAKE_USER_NAME" ] ; then
	# No Fake UserName
	export FAKE_USER_NAME=$USER
fi
if [ -z "$FAKE_HOST_NAME" ] ; then
	# No Fake HostName
	export FAKE_HOST_NAME=$HOSTNAME
fi
if [ -z "$VIEW_HOST" ] ; then
	export VIEW_HOST=1
fi

# Kawaii
kawaii_ps1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]"

# UserName
kawaii_ps1="${kawaii_ps1}${FAKE_USER_NAME}\[\e[32m\]"

# HostName
if [ $VIEW_HOST -eq 1 ] ; then
	kawaii_ps1="${kawaii_ps1}@\[\e[33m\]${FAKE_HOST_NAME}\[\e[32m\]/ "
else
	kawaii_ps1="${kawaii_ps1}/ "
fi

kawaii_ps1="${kawaii_ps1}\[\e[33m\]\w\[\e[0m\]$ "

# Apply Kawaii
PS1=$kawaii_ps1
unset kawaii_ps1
