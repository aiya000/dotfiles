#!/usr/bin/env zsh

###############
# Config envs #
###############
# Prepare constant {{{

function find_name_by_uname () { #{{{
	uname=`uname -a`
	if [ -n "`echo $uname | grep ${1}`" ] ; then
		echo 1
	else
		echo 0
	fi
} #}}}

#export IS_UBUNTU=`find_name_by_uname Ubuntu`
#export IS_CYGWIN=`find_name_by_uname Cygwin`

# for RPS[12]
VIM_NORMAL="%{$bg[red]%}[NORMAL]%{$reset_color%}"
VIM_INSERT="%{$bg[blue]%}[INSERT]%{$reset_color%}"

# }}}
# Set zsh variables {{{

# Environment Variables
export ZDOTDIR=~/.zsh
export HISTFILE=$ZDOTDIR/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
export HISTTIMEFORMAT='%Y/%m/%d %H:%M '
export HISTIGNORE='ls:jobs:history*:*hibernate'
export HISTIGNORE="${HISTIGNORE}:*.bash_history*:*mount*-o*password=*"
export RPS1="${${KEYMAP/vicmd/$VIM_NORMAL}/(main|viins)/$VIM_INSERT}"
export RPS2=$RPS1


# }}}
# Reset $PATH {{{

# Set PATH with priority
new_path=$HOME/bin:$HOME/sbin
new_path=$new_path:$HOME/.dotfiles/bin

# stack
[ -d ~/.stack ] \
	&& new_path=$new_path:$HOME/.stack/programs/x86_64-linux/ghc-7.8.4/bin
# cabal
[ -d ~/.cabal ] \
	&& new_path=$new_path:$HOME/.cabal/bin \
	&& new_path=$new_path:./.cabal-sandbox/bin
# pkgsrc
[ -d ~/pkg ] \
	&& new_path=$new_path:$HOME/pkg/bin:$HOME/pkg/sbin
# rbenv
[ -d ~/.rbenv ] \
	&& new_path=$new_path:$HOME/.rbenv/bin \
	&& new_path=$new_path:$HOME/.rbenv/versions/`cat ~/.rbenv/version`/bin \
	&& eval "$($HOME/.rbenv/bin/rbenv init -)"  # Oh, my cygwin... why I cannot use shims...
# ruby-build
[ -d ~/.rbenv/plugins/ruby-build/bin ] \
	&& new_path=$new_path:$HOME/.rbenv/plugins/ruby-build/bin
# pyenv
[ -d ~/.pyenv ] \
	&& export PYENV_ROOT=~/.pyenv \
	&& new_path=$new_path:$PYENV_ROOT/bin \
	&& eval "$($HOME/.pyenv/bin/pyenv init -)"
# some binaries from some languages
[ -d ~/.local ] \
	&& new_path=$new_path:$HOME/.local/bin

# and basics
new_path=$new_path:/bin:/sbin
new_path=$new_path:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin
new_path=$new_path:/opt/bin:/opt/sbin:/opt/local/sbin:/opt/local/bin
new_path=$new_path:$GOPATH/bin

# With OS
if [ $IS_CYGWIN -eq 1 ] ; then
	export HOME=/home/$USER
	new_path=$new_path:/cygdrive/c/Windows/system32:/cygdrive/c/Windows
fi

# Apply to PATH
export PATH=$new_path:$PATH
unset new_path

#}}}
# Others {{{

export EDITOR=vim
export VTE_CJK_WIDTH=1

# Use 'vim-pager' and 'vim-manpager'
export PAGER='vim - +PAGER -c "setl nonu nornu | setf vim-pager"'
export MANPAGER='vim - +MANPAGER -c "setl nonu nornu | setf vim-pager"'

# Languages
export GOPATH=~/.GoPath

# }}}


##################
# Manage Plugins #
##################
# Set plugins configuration {{{

# zplug
export ZPLUG_HOME=$ZDOTDIR/zplug

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
alias zsh_pr_loaded='echo "pr_loaded"'
