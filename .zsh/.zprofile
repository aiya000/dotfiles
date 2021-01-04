#!/usr/bin/env zsh

# Configure environment variables without any plugins

##########################
# Config the environment #
##########################
# Set the zsh variables {{{

export ZDOTDIR=~/.zsh
export HISTFILE=$ZDOTDIR/history
export HISTORY_IGNORE=''
export HISTSIZE=1000000
export SAVEHIST=1000000
export TIMEFMT='%Y/%m/%d %H:%M '
export FPATH=$FPATH:$HOME/.zsh/complete

# }}}
# Others {{{

export BROWSER=vivaldi-stable
export EDITOR=vim
export GIT_EDITOR=vim
export VISUAL=vim
export PAGER='vim - +PAGER' # vim-pager
export MANPAGER='vim - +MANPAGER -c "setl nonu nornu ft=vim-pager | only"' # vim-manpager

export GOPATH=~/.GoPath
export LICE_USER=aiya000
export CCACHE_DISABLE=1
export THEMIS_HOME="$HOME/.vim/bundle/repos/github.com/thinca/vim-themis"
export NVIM_PYTHON_LOG_FILE=$HOME/.vim/.log/nvim_python.log
export PYENV_ROOT=$HOME/.pyenv
export NVM_DIR=$HOME/.nvm

# Mine
export MY_GUI_EDITOR=gvim

if [[ $ZSHRC_PROMPT_GIT_STATUS_DISABLE != '' ]] ; then
    export ZSHRC_PROMPT_GIT_STATUS_DISABLE
else
    export ZSHRC_PROMPT_GIT_STATUS_DISABLE=0
fi

# Plugins
export HEREIS_ALIAS_PREFIX=p-
export ENHANCD_COMMAND=ecd
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

# }}}
# Add paths to $PATH {{{

export PATH

# High
PATH=$HOME/bin:$HOME/sbin:$PATH
PATH=$HOME/.dotfiles/bin:$PATH
PATH=$PYENV_ROOT/bin:$PATH

# Basics
PATH=$PATH:/bin:/sbin
PATH=$PATH:/usr/local/bin:/usr/local/sbin
PATH=$PATH:/usr/bin:/usr/sbin
PATH=$PATH:/opt/bin:/opt/sbin:/opt/local/sbin:/opt/local/bin

PATH=$PATH:$GOPATH/bin
PATH=$PATH:$HOME/.yarn/bin
PATH=$PATH:$HOME/.npm-prefix/bin
PATH=$PATH:$HOME/.cargo/bin
PAHT=$PATH:$HOME/.ghcup/bin
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:$HOME/.linuxbrew/bin


# completions
PATH=$PATH:$ZDOTDIR/zsh_completions
PATH=$PATH:$NVM_DIR  # ~/.nvm/bash_completion

# git-credential-gnome-keyring
PATH=$PATH:/usr/lib/git-core

#}}}


###########################
# Mark the end of loading #
###########################
alias zsh_pr_loaded='echo "pr_loaded"'
