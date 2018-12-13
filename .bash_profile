#!/bin/bash

##########################
# Config the environment #
##########################
# Set the bash variables {{{

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
export EDITOR=nvim
export VISUAL=nvim

# Use 'vim-pager' and 'vim-manpager'
export PAGER='nvim - +PAGER -c "setl nonu nornu ft=vim-pager | only"'
export MANPAGER='nvim - +MANPAGER -c "setl nonu nornu ft=vim-pager | only"'

# Languages
export GOPATH=~/.GoPath

# Others
export CCACHE_DISABLE=1

# }}}


###################
# Manage Plugins  #
###################
# Configurate the plugin variables {{{

# sh-hereis
export HEREIS_ALIAS_PREFIX=p-

# }}}


###########################
# Mark the end of loading #
###########################
# Export Loaded Archive
alias pr_loaded='echo "pr_loaded"'
