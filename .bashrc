#!/bin/bash
source ~/.sh_generic/completion.sh
source ~/.sh_generic/aliases.sh

###################
# Check .zprofile #
###################
# The counterplan for if .bash_profile never loaded
if [ -z "`alias | grep pr_loaded`" ] ; then
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
[ -d ~/.cabal ] \
    && PATH=$PATH:$HOME/.cabal/bin \
    && PATH=$PATH:./.cabal-sandbox/bin

# pkgsrc
[ -d ~/pkg ] \
    && PATH=$PATH:$HOME/pkg/bin:$HOME/pkg/sbin

# rbenv
[ -d ~/.rbenv ] \
    && PATH=$PATH:$HOME/.rbenv/bin \
    && PATH=$PATH:$HOME/.rbenv/versions/`cat ~/.rbenv/version`/bin \
    && eval "$($HOME/.rbenv/bin/rbenv init -)"

# ruby-build
[ -d ~/.rbenv/plugins/ruby-build/bin ] \
    && PATH=$PATH:$HOME/.rbenv/plugins/ruby-build/bin

# virtualenv with virtualenvwrapper
which virtualenvwrapper.sh > /dev/null 2>&1
[ "$?" -eq 0 ] \
    && export WORKON_HOME=$HOME/.virtualenvs \
    && source $(which virtualenvwrapper.sh)

# anything
[ -d ~/.local ] \
    && PATH=$PATH:$HOME/.local/bin

# }}}


###################
# Define Commands #
###################
# Override default {{{

alias vim=vime
alias nvim=nvime

# }}}
# Define specified aliases for bash {{{

# Bash Short Cuts
alias reload='. ~/.bashrc && . ~/.bash_profile && echo ">> the bash configurations are reloaded"'

# }}}


##################
# Manage Plugins #
##################
# Load the local plugins {{{

plugin_dir=~/.bashfiles/plugin
local_plugins=( \
    hereis.sh \
    shell_kawaii.sh \
    ezoe_command_not_found_handle.sh \
    tovim.sh \
)

for (( i = 0; i < ${#local_plugins[@]}; ++i )) ; do
    source "${plugin_dir}/${local_plugins[$i]}"
done

unset local_plugins plugin_dir

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

# Load plugins
source ~/.bashfiles/plugin/shell_kawaii.sh
source ~/.bashfiles/plugin/hereis.sh
source ~/.bashfiles/plugin/tovim.sh

# Export Loaded Archive
alias rc_loaded='echo "rc_loaded"'
