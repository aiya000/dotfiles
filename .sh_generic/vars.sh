#!/bin/bash

export EDITOR=vim
export GIT_EDITOR=vim
export VISUAL=vim
export PAGER='vim - +PAGER' # vim-pager
export MANPAGER='vim - +MANPAGER -c "setl nonu nornu ft=vim-pager | only"' # vim-manpager

export VIM_SESSION=$HOME/.backup/vim-backup/session
export DUSTBOX=$HOME/.backup/dustbox
export STORE_TEMPORARY=$HOME/.backup/store-temporary  # Please see bin/store-temporary

export GOPATH=~/.GoPath
export LICE_USER=aiya000
export CCACHE_DISABLE=1
export THEMIS_HOME=$HOME/.vim/bundle/repos/github.com/thinca/vim-themis
export NVIM_PYTHON_LOG_FILE=$HOME/.vim/.log/nvim_python.log
export PYENV_ROOT=$HOME/.pyenv
export NVM_DIR=$HOME/.nvm
export DENO_INSTALL=$HOME/.deno
export ANDROID_HOME=$HOME/Android/Sdk

# Mine
export MY_GUI_EDITOR=gvim
export DOTFILES_GIT_COMMIT_PREFIX_IMPROVEMENT=:sparkles:
export DOTFILES_GIT_COMMIT_PREFIX_UPDATE=:up:
export DOTFILES_GIT_COMMIT_PREFIX_BUGFIX=:bug:
export DOTFILES_GIT_COMMIT_PREFIX_REFACTOR=:recycle:
export DOTFILES_BATCAT_DEFAULT_OPTIONS='--number'

if [[ $ZSHRC_PROMPT_GIT_STATUS_DISABLE != '' ]] ; then
  export ZSHRC_PROMPT_GIT_STATUS_DISABLE
else
  export ZSHRC_PROMPT_GIT_STATUS_DISABLE=0
fi

export HEREIS_ALIAS_PREFIX=p-
