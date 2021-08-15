#!/bin/bash

#######################
# Check .bash_profile #
#######################
# Load it if it is never loaded
if ! alias | grep -q pr_loaded ; then
  source ~/.bash_profile
fi

##################################
# Configure bash with conditions #
##################################
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


###################
# Define Commands #
###################
# Define specified aliases for bash {{{

alias reload='source ~/.bashrc && source ~/.bash_profile && echo ">> the bash configurations are reloaded"'
alias rel=reload

# }}}

if [ -f ~/.bashrc_env ] ; then
  source ~/.bashrc_env
fi

# Expose to mark .bashrc is loaded
alias rc_loaded='echo "rc_loaded"'
