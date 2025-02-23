#!/usr/bin/env zsh

# Configure environment variables without any plugins

##########################
# Config the environment #
##########################
# Set the zsh variables {{{

export ZDOTDIR=~/.zsh
export HISTFILE=$ZDOTDIR/history
export HISTORY_IGNORE=''
export HISTSIZE=1000000000
export SAVEHIST=1000000000
export FPATH=$FPATH:$HOME/.zsh/complete

# For a command 'time'
export TIMEFMT=$'
========================
Program : %J
CPU     : %P
user    : %*Us
system  : %*Ss
total   : %*Es
========================
'

# }}}
# Plugins {{{

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

# }}}
# Others {{{

# For variables that is not related to zsh, see ~/.dotfiles/.sh_generic/vars.sh

# }}}
# Add paths to $PATH {{{

export PATH

# Higher order
PATH=$PYENV_ROOT/bin:$PATH
PATH=$HOME/.rbenv/bin:$PATH
PATH=$HOME/.dotfiles/bin:$PATH
PATH=$HOME/bin:$HOME/sbin:$PATH
# PATH=$DENO_INSTALL/bin:$PATH  # Disabling to unknown errors on npm projects

# Basics
PATH=$PATH:/bin:/sbin
PATH=$PATH:/usr/local/bin:/usr/local/sbin
PATH=$PATH:/usr/bin:/usr/sbin
PATH=$PATH:/opt/bin:/opt/sbin:/opt/local/sbin:/opt/local/bin

PATH=$PATH:$GOPATH/bin
PATH=$PATH:$HOME/.yarn/bin
PATH=$PATH:$HOME/.npm-prefix/bin
PATH=$PATH:$HOME/.cargo/bin
PATH=$PATH:$HOME/.ghcup/bin
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:$HOME/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/bin

# Plugins
PATH=$PATH:$ZDOTDIR/plugin/sh-git-lfs-install-append

# completions
PATH=$PATH:$ZDOTDIR/zsh_completions
PATH=$PATH:$NVM_DIR  # for ~/.nvm/bash_completion

# git-credential-gnome-keyring
PATH=$PATH:/usr/lib/git-core

# AndroidStudio
PATH=$PATH:$ANDROID_HOME/platform-tools

#}}}


###########################
# Mark the end of loading #
###########################
alias zsh_pr_loaded='echo "pr_loaded"'
