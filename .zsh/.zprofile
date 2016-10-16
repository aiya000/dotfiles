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

export IS_UBUNTU=`find_name_by_uname Ubuntu`
export IS_CYGWIN=`find_name_by_uname Cygwin`

# }}}
# Set zsh variables {{{

# For $RPROMPT
vi_normal="%{$bg[red]%}[NORMAL]%{$reset_color%}"
vi_insert="%{$bg[blue]%}[INSERT]%{$reset_color%}"

# Environment Variables
export ZDOTDIR=~/.zsh
export RPROMPT="${${KEYMAP/vicmd/$vi_normal}/(main|viins)/$vi_insert}"
export HISTFILE=$ZDOTDIR/.zsh_history
export HISTSIZE=1000000
export SAVEHIST=1000000
export HISTTIMEFORMAT='%Y/%m/%d %H:%M '
export HISTIGNORE="*.zsh_history*:*mount*-o*password=*"


# }}}
# Reset $PATH {{{

# Mine
new_path=$HOME/bin:$HOME/sbin
new_path=$new_path:$HOME/.dotfiles/bin

# zsh completions
new_path=$new_path:$ZDOTDIR/zsh_completions

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

# some languages binary
[ -d ~/.local ] \
	&& new_path=$new_path:$HOME/.local/bin

# Basic paths
new_path=$new_path:/bin:/sbin
new_path=$new_path:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin
new_path=$new_path:/opt/bin:/opt/sbin:/opt/local/sbin:/opt/local/bin
new_path=$new_path:$GOPATH/bin

# Using windows's command
if [ $IS_CYGWIN -eq 1 ] ; then
	export HOME=/home/$USER
	new_path=$new_path:/cygdrive/c/Windows/system32:/cygdrive/c/Windows
fi

# Apply to PATH
export PATH=$new_path:$PATH
unset new_path

#}}}
# Others {{{

# Favorite editor
export EDITOR=vim
export VISUAL=vim

# Show 2byte chars rightly for terminal-emulators
export VTE_CJK_WIDTH=1

# Using 'vim-pager' and 'vim-manpager'
export PAGER='vim - +PAGER -c "setl nonu nornu ft=vim-pager | only"'
export MANPAGER='vim - +MANPAGER -c "setl nonu nornu ft=vim-pager | only"'

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
