#!/bin/bash

# Use git-completion
if [ -n "$ZDOTDIR" ] ; then
    if [ -f /usr/share/git/completion/git-completion.zsh ] && [ ! -f "$ZDOTDIR/_git" ] ; then
        cp /usr/share/git/completion/git-completion.zsh "$ZDOTDIR/_git"
    fi
else # Assume the shell is bash
    if [ -f "$HOME/.bashfiles/git-completion.bash" ] ; then
        # shellcheck disable=SC1090
        source "$HOME/.bashfiles/git-completion.bash"
    fi
fi


# Use travis-completion
if [ -f ~/.travis/travis.sh ] ; then
    # shellcheck disable=SC1090
    source ~/.travis/travis.sh
fi

#FIXME: Avoid to load that is failure on the cygwin
if [[ $(uname | grep -i cygwin) == '' ]] ; then
    # Use stack-completion
    if type stack > /dev/null 2>&1 ; then
        # This completion needs compinit and bashcompinit function
        # > autoload -U compinit     && compinit
        # > autoload -U bashcompinit && bashcompinit
        eval "$(stack --bash-completion-script stack)"
    fi
fi
