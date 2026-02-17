#!/bin/bash

# Unified configurations for any shell environment

# Neovim
export EDITOR=nvim
export GIT_EDITOR=nvim
export VISUAL=nvim
export MANPAGER='nvim - +MANPAGER -c "setl nonu nornu nolist ft=man | only"' # vim-manpager
export NVIM_PYTHON_LOG_FILE=$HOME/.config/nvim/.log/nvim_python.log

# Vim
export VIM_SESSION=$HOME/.backup/vim-backup/session
export THEMIS_HOME=$HOME/.vim/bundle/repos/github.com/thinca/vim-themis

# Development
export ANDROID_HOME=$HOME/Android/Sdk
export CCACHE_DISABLE=1
export DENO_INSTALL=$HOME/.deno
export GOPATH=~/.GoPath
export LICE_USER=aiya000
export NVM_DIR=$HOME/.nvm
export PYENV_ROOT=$HOME/.pyenv

# For my scripts and other configuration files {{{

export BASH_TOYS_INTERACTIVE_FILTER=peco-reverse  # To display the most recently started process nearby with kill-list command
export BASH_TOYS_RESTORE_KEEP=1
export BASH_TOYS_WHEN_POMODORO_TIMER_FINISHED="notify Pomodoro-Timer The-time-finished! $HOME/.dotfiles/bash-toys/assets/notification-impact.mp3"
export BASH_TOYS_POMODORO_DEFAULT_INTERVAL=60

export DOTFILES_BATCAT_DEFAULT_OPTIONS=--number
export DOTFILES_ZSHRC_AUTO_LOADED_ENVS=()

if [[ $ZSHRC_PROMPT_GIT_STATUS_DISABLE == '' ]] ; then
  ZSHRC_PROMPT_GIT_STATUS_DISABLE=0
fi
export ZSHRC_PROMPT_GIT_STATUS_DISABLE

export HEREIS_ALIAS_PREFIX=p-

# }}}

PATH=$PATH:~/.GoPath/bin

# export NODE_OPTIONS=--max-old-space-size=16384
