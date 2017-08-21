#!/bin/bash

# Use git-completion
if [ -f /usr/share/git/completion/git-completion.zsh ] && [ ! -f "$ZDOTDIR/_git" ] ; then
	cp /usr/share/git/completion/git-completion.zsh "$ZDOTDIR/_git"
fi

# Use travis-completion
if [ -f ~/.travis/travis.sh ] ; then
	# shellcheck disable=SC1090
	source ~/.travis/travis.sh
fi

# Use stack-completion
type stack > /dev/null 2>&1
if [ "$?" -eq 0 ] ; then
	# This completion needs compinit and bashcompinit function
	# > autoload -U compinit     && compinit
	# > autoload -U bashcompinit && bashcompinit
	eval "$(stack --bash-completion-script stack)"
fi
