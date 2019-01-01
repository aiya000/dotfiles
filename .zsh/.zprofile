#!/usr/bin/env zsh

# Configure environment variables without any plugins

##########################
# Config the environment #
##########################
# Set the zsh variables {{{

export ZDOTDIR=~/.zsh
export HISTFILE=$ZDOTDIR/history
export HISTSIZE=1000000
export HISTORY_IGNORE=''
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

# yarn
PATH=$PATH:$HOME/.yarn/bin

# zsh completions
PATH=$PATH:$ZDOTDIR/zsh_completions

# git-credential-gnome-keyring
PATH=$PATH:/usr/lib/git-core

#}}}
# Others {{{

export BROWSER=vivaldi-stable
export EDITOR=vim
export GIT_EDITOR=vim
export VISUAL=vim
export PAGER='vim - +PAGER -c "setl nonu nornu ft=vim-pager | only"' # vim-pager
export MANPAGER='vim - +MANPAGER -c "setl nonu nornu ft=vim-pager | only"' # vim-manpager

export GOPATH=~/.GoPath
export LICE_USER=aiya000
export FZF_DEFAULT_OPTS='--tac --cycle --bind=ctrl-j:accept,ctrl-k:kill-line'
export CCACHE_DISABLE=1
export THEMIS_HOME="$HOME/.vim/bundle/repos/github.com/thinca/vim-themis"
export NVIM_PYTHON_LOG_FILE=$HOME/.vim/.log/nvim_python.log

# Mine
export MY_GUI_EDITOR=gvim

# Plugins
export HEREIS_ALIAS_PREFIX=p-
export ENHANCD_COMMAND=cde

# }}}


###########################
# Mark the end of loading #
###########################
alias zsh_pr_loaded='echo "pr_loaded"'
