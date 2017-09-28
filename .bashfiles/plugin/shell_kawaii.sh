#!/bin/sh -eu

# Set default environment values
if [ ! -v PROMPT_SHELL_KAWAII ] ; then
    # Kawaii visit on the prompt
    export PROMPT_SHELL_KAWAII=0
fi
if [ ! -v FAKE_USER_NAME ] ; then
    # No Fake UserName
    export FAKE_USER_NAME=$USER
fi
if [ ! -v FAKE_HOST_NAME ] ; then
    # No Fake HostName
    export FAKE_HOST_NAME=$HOSTNAME
fi
if [ ! -v VIEW_HOST ] ; then
    export VIEW_HOST=1
fi

# UserName
kawaii_ps1="${FAKE_USER_NAME}\[\e[32m\]"

# HostName
if [ $VIEW_HOST -eq 1 ] ; then
    kawaii_ps1="${kawaii_ps1}@\[\e[33m\]${FAKE_HOST_NAME}\[\e[32m\]"
fi

# Visit kawaii and surround slash
if [ $PROMPT_SHELL_KAWAII -eq 1 ] ; then
    kawaii_ps1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]${kawaii_ps1}/ "
else
    kawaii_ps1="\[\e[33m\]${kawaii_ps1} "
fi

# Close colors
kawaii_ps1="${kawaii_ps1}\[\e[33m\]\w\[\e[0m\]$ "

# Apply Kawaii
PS1=$kawaii_ps1
unset kawaii_ps1
