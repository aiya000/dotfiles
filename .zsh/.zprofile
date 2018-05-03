#!/usr/bin/env zsh

# Configure environment variables without any plugins

##########################
# Config the environment #
##########################
# Set the zsh variables {{{

export ZDOTDIR=~/.zsh
export HISTFILE=$ZDOTDIR/history
export HISTSIZE=1000000
export HISTIGNORE="*mount*-o*password=*"
export SAVEHIST=1000000
export TIMEFMT='%Y/%m/%d %H:%M '
export FPATH=$FPATH:$HOME/.zsh/complete

# }}}
# Add paths to $PATH {{{

# Mine
PATH=$HOME/bin:$HOME/sbin:$PATH
PATH=$PATH:$HOME/.dotfiles/bin

# Basics
PATH=$PATH:/bin:/sbin
PATH=$PATH:/usr/local/bin:/usr/local/sbin
PATH=$PATH:/usr/bin:/usr/sbin
PATH=$PATH:/opt/bin:/opt/sbin:/opt/local/sbin:/opt/local/bin
PATH=$PATH:$GOPATH/bin

# Load haskell-cabal and etc
PATH=$PATH:$HOME/.local/bin

# zsh completions
PATH=$PATH:$ZDOTDIR/zsh_completions

# git-credential-gnome-keyring
PATH=$PATH:/usr/lib/git-core

#}}}
# Others {{{

# Favorite editor
export EDITOR=nvim
export VISUAL=nvim
export BROWSER=vivaldi-stable

# Using 'vim-pager' and 'vim-manpager'
export PAGER="$EDITOR - +PAGER -c 'setl nonu nornu ft=vim-pager | only'"
export MANPAGER="$EDITOR - +MANPAGER -c 'setl nonu nornu ft=vim-pager | only'"

# Others
export GOPATH=~/.GoPath
export LICE_USER=aiya000
export FZF_DEFAULT_OPTS='--tac --cycle --bind=ctrl-j:accept,ctrl-k:kill-line'
export UNCO_ESPEAK_OPTIONS='-s 150 -v +fex'
export CCACHE_DISABLE=1
export THEMIS_HOME="$HOME/.vim/bundle/repos/github.com/thinca/vim-themis"

# }}}


###########################
# Mark the end of loading #
###########################
alias zsh_pr_loaded='echo "pr_loaded"'
