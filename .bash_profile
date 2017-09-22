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

# Lice copylight user
export LICE_USER=aiya000

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
export HEREIS_ALIAS_PREFIX='p_'

# zsh-shell-kawaii
export SHELL_KAWAII_HER_VISIBILITY=1
export SHELL_KAWAII_HOST_VISIBILITY=1
export SHELL_KAWAII_FAKE_USERNAME='aiya_000'
export SHELL_KAWAII_FAKE_HOSTNAME='Arch'

# }}}


###########################
# Mark the end of loading #
###########################
# Export Loaded Archive
alias pr_loaded='echo "pr_loaded"'
