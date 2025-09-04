#!/bin/bash

export EDITOR=nvim
export GIT_EDITOR=nvim
export VISUAL=nvim
export MANPAGER='nvim - +MANPAGER -c "setl nonu nornu nolist ft=man | only"' # vim-manpager
export VIM_SESSION=$HOME/.backup/vim-backup/session
export GOPATH=~/.GoPath
export LICE_USER=aiya000
export CCACHE_DISABLE=1
export THEMIS_HOME=$HOME/.vim/bundle/repos/github.com/thinca/vim-themis
export NVIM_PYTHON_LOG_FILE=$HOME/.config/nvim/.log/nvim_python.log
export PYENV_ROOT=$HOME/.pyenv
export NVM_DIR=$HOME/.nvm
export DENO_INSTALL=$HOME/.deno
export ANDROID_HOME=$HOME/Android/Sdk

# Mine
export MY_GUI_EDITOR=gvim
export DOTFILES_BATCAT_DEFAULT_OPTIONS=--number
export DOTFILES_ZSHRC_AUTO_LOADED_ENVS=()

if [[ $ZSHRC_PROMPT_GIT_STATUS_DISABLE == '' ]] ; then
  ZSHRC_PROMPT_GIT_STATUS_DISABLE=0
fi
export ZSHRC_PROMPT_GIT_STATUS_DISABLE

export HEREIS_ALIAS_PREFIX=p-
