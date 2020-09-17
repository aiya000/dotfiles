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
PATH=$PATH:$GOPATH/bin

# zsh completions
PATH=$PATH:$ZDOTDIR/zsh_completions

#}}}
# Others {{{

# My favorite editor
export EDITOR=vim
export VISUAL=vim

# To use 'vim-pager' and 'vim-manpager'
export PAGER='vim - +PAGER -c "setl nonu nornu ft=vim-pager | only"'
export MANPAGER='vim - +MANPAGER -c "setl nonu nornu ft=vim-pager | only"'

# Languages
export GOPATH=~/.GoPath

# Others
export CCACHE_DISABLE=1

# }}}


##################
# Manage plugins #
##################
# Define plugin variables {{{

# sh-hereis
export HEREIS_ALIAS_PREFIX=p-

# }}}


# Expose to mark .bash_profile is loaded
alias pr_loaded='echo "pr_loaded"'
