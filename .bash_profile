#!/bin/bash

#############################
# Configure the environment #
#############################
# Set bash variables {{{

export ZDOTDIR=~/.zsh
export HISTSIZE=10000
export HISTTIMEFORMAT='%Y/%m/%d %H:%M '
export HISTIGNORE="*.bash_history*:*mount*-o*password=*"

# }}}
# Add paths to $PATH {{{

PATH=$HOME/bin:$HOME/sbin:$HOME/.dotfiles/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/opt/bin:/opt/sbin:/opt/local/sbin:/opt/local/bin:$PATH

# zsh completions
PATH=$PATH:$ZDOTDIR/zsh_completions

#}}}
# Others {{{

# My favorite editor
export EDITOR=vim
export VISUAL=vim

# }}}

# Expose to mark .bash_profile is loaded
alias pr_loaded='echo "pr_loaded"'
