#!/bin/bash

#TODO: organize conditions ( yokei-na-mono delete )

# Set default environment values
if [ -z "$SHELL_KAWAII" ] ; then
	export SHELL_KAWAII=1
fi
if [ -z "$VIEW_FAKE" ] ; then
	export VIEW_FAKE=1
fi
if [ -z "$FAKE_USER_NAME" ] ; then
	export FAKE_USER_NAME=$USER
fi
if [ -z "$FAKE_HOST_NAME" ] ; then
	export FAKE_HOST_NAME=$HOSTNAME
fi
if [ -z "$VIEW_HOST" ] ; then
	export VIEW_HOST=1
fi


if [ $SHELL_KAWAII -eq 1 ] ; then
	[ -n "$VIEW_FAKE" ] && if [ $VIEW_FAKE -eq 1 ] ; then
		[ -n "$VIEW_HOST" ] && [ $VIEW_HOST -eq 1 ] \
			&& PS1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]${FAKE_USER_NAME}\[\e[32m\]@\[\e[33m\]${FAKE_HOST_NAME}\[\e[32m\]/ \[\e[33m\]\w\[\e[0m\]$ " \
			|| PS1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]${FAKE_USER_NAME}\[\e[32m\]/ \[\e[33m\]\w\[\e[0m\]$ "
	else
		[ -n "$VIEW_HOST" ] && [ $VIEW_HOST -eq 1 ] \
			&& PS1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]\u\[\e[32m\]@\[\e[33m\]\h\[\e[32m\]/ \[\e[33m\]\w\[\e[0m\]$ " \
			|| PS1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]\u\[\e[32m\]/ \[\e[33m\]\w\[\e[0m\]$ "
	fi
fi
