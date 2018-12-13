#!/bin/bash

# shellcheck disable=SC1090
source ~/.sh_generic/aliases.sh

###################
# Check .zprofile #
###################
# The counterplan for if .bash_profile never loaded
if ! alias | grep -q pr_loaded ; then
    source ~/.bash_profile
fi

############################################
# Configure bash with the local conditions #
############################################
# Set the bash options {{{

set -o ignoreeof  # Disable logoff by Ctrl + D
set -o vi         # Set vi style keymapping mode
stty stop  undef  # unbind C-s that is stop viewing inputs to screen
stty start undef  # unbind C-q that is start viewing inputs to screen

# }}}
# Set the bash key-mappings {{{

# Vim nize
bind -m vi-command '"_": beginning-of-line'
bind -m vi-insert  '"\C-\\\C-n": "\e"'

# Emacs nize
bind -m vi-insert  '"\C-n": next-history'
bind -m vi-insert  '"\C-p": previous-history'
bind -m vi-insert  '"\C-a": beginning-of-line'
bind -m vi-insert  '"\C-e": end-of-line'
bind -m vi-insert  '"\C-b": backward-char'
bind -m vi-insert  '"\C-f": forward-char'
bind -m vi-insert  '"\C-k": kill-line'
bind -m vi-insert  '"\C-d": delete-char'

# My taste
bind -m vi-insert  '"\C-l": "\e"'
bind -m vi-insert  '"\C-]": clear-screen'
bind -m vi-command -x '"\C-k\C-r": . ~/.bashrc && echo ">> bash source reloaded"'

# }}}
# Set language tools {{{

# cabal
if [ -d ~/.cabal ] ; then
    PATH=$PATH:$HOME/.cabal/bin \
        && PATH=$PATH:./.cabal-sandbox/bin
fi

# pkgsrc
if [ -d ~/pkg ] ; then
    PATH=$PATH:$HOME/pkg/bin:$HOME/pkg/sbin
fi

# rbenv
if [ -d ~/.rbenv ] ; then
    PATH=$PATH:$HOME/.rbenv/bin \
        && PATH=$PATH:$HOME/.rbenv/versions/$(cat ~/.rbenv/version)/bin \
        && eval "$("$HOME/.rbenv/bin/rbenv" init -)"
fi

# ruby-build
if [ -d ~/.rbenv/plugins/ruby-build/bin ] ; then
    PATH=$PATH:$HOME/.rbenv/plugins/ruby-build/bin
fi

# virtualenv with virtualenvwrapper
if which virtualenvwrapper.sh > /dev/null 2>&1 ; then
    export WORKON_HOME=$HOME/.virtualenvs \
        && source "$(which virtualenvwrapper.sh)"
fi

# anything
[ -d ~/.local ] \
    && PATH=$PATH:$HOME/.local/bin

# }}}


###################
# Define Commands #
###################
# Define specified aliases for bash {{{

# Bash Short Cuts
alias reload='. ~/.bashrc && . ~/.bash_profile && echo ">> the bash configurations are reloaded"'

# }}}

# For each environment
case $(uname) in
Linux )
    source ~/.sh_generic/linux.sh
    ;;
CYGWIN_NT-10.0 )
    source ~/.sh_generic/cygwin.sh
    PATH=$PATH:/cygdrive/c/Windows/system32:/cygdrive/c/Windows
    ;;
esac

# If it exists, load environment config
if [ -f ~/.bashrc_env ] ; then
    source ~/.bashrc_env
fi

# Export Loaded Archive
alias rc_loaded='echo "rc_loaded"'
