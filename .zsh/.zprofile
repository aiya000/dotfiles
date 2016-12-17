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
PATH=$HOME/bin:$HOME/sbin
PATH=$PATH:$HOME/.dotfiles/bin
PATH=$PATH:$HOME/.dotfiles/aacceessoorryy/bin

# Basics
PATH=$PATH:/bin:/sbin
PATH=$PATH:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin
PATH=$PATH:/opt/bin:/opt/sbin:/opt/local/sbin:/opt/local/bin
PATH=$PATH:$GOPATH/bin

# zsh completions
PATH=$PATH:$ZDOTDIR/zsh_completions

# Use windows's command
if [ $IS_CYGWIN -eq 1 ] ; then
	PATH=$PATH:/cygdrive/c/Windows/system32:/cygdrive/c/Windows
fi


#}}}
# Others {{{

# Reset HOME
if [ $IS_CYGWIN -eq 1 ] ; then
	export HOME=/home/$USER
fi

# Favorite editor
export EDITOR=vim
export VISUAL=vim

# Using 'vim-pager' and 'vim-manpager'
export PAGER='vim - +PAGER -c "setl nonu nornu ft=vim-pager | only"'
export MANPAGER='vim - +MANPAGER -c "setl nonu nornu ft=vim-pager | only"'

# Lice copylight user
export LICE_USER=aiya000

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
