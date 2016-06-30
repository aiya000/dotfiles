#!/bin/sh -eu

# Set default values
if [ -z "$PROMPT_SHELL_KAWAII" ] ; then
	# Kawaii visit on the prompt
	export PROMPT_SHELL_KAWAII=0
fi
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

# Set fake host name
if [ $VIEW_HOST -eq 1 ] ; then
	kawaii_info="%{$fg[yellow]%}${FAKE_USER_NAME}%{$fg[green]%}@%{$fg[yellow]%}${FAKE_HOST_NAME}%{$reset_color%}"
fi

# Show kawaii character
if [ $PROMPT_SHELL_KAWAII -eq 1 ] ; then
	kawaii_face="%{$fg[green]%}(*^-^)</%{$fg[yellow]%}${kawaii_info}%{$fg[green]%}/%{$reset_color%}"
else
	kawaii_face="${kawaii_info}"
fi

# Show current directory
kawaii_current_dir="%{$fg[yellow]%}%~%{$reset_color%}"

# Apply Kawaii
PS1="${kawaii_face} ${kawaii_current_dir}%(!.#.$)%{$reset_color%} "
