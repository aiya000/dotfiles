#!/usr/bin/env zsh

###################
# Check .zprofile #
###################
# The counterplan for if .zprofile never loaded
if [ -z "`alias | grep zsh_pr_loaded`" ] ; then
	source $ZDOTDIR/.zprofile
fi


# Load premised commands
source ~/.sh_generic/aliases.sh

###########################################
# Configure zsh with the local conditions #
###########################################
# Set zsh options {{{

# Use modules
autoload -U colors       && colors
autoload -U compinit     && compinit
autoload -U bashcompinit && bashcompinit

# Use select menu in the completion
zstyle ':completion:*' menu select
# Case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
# list-colors uses $LS_COLORS's colors
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Set opts
setopt hist_ignore_dups
setopt extended_history
setopt hist_reduce_blanks
setopt share_history
setopt transient_rprompt
setopt ignore_eof

# Don't use screen stopping
stty stop  undef
stty start undef

# }}}
# Set language tools {{{

# stack
[ -d ~/.stack ] \
	&& PATH=$PATH:$HOME/.stack/programs/x86_64-linux/ghc-7.8.4/bin

# cabal
[ -d ~/.cabal ] \
	&& PATH=$PATH:$HOME/.cabal/bin \
	&& PATH=$PATH:./.cabal-sandbox/bin

# pkgsrc
[ -d ~/pkg ] \
	&& PATH=$PATH:$HOME/pkg/bin:$HOME/pkg/sbin

# rbenv
[ -d ~/.rbenv ] \
	&& PATH=$PATH:$HOME/.rbenv/bin \
	&& PATH=$PATH:$HOME/.rbenv/versions/`cat ~/.rbenv/version`/bin \
	&& eval "$($HOME/.rbenv/bin/rbenv init -)"

# ruby-build
[ -d ~/.rbenv/plugins/ruby-build/bin ] \
	&& PATH=$PATH:$HOME/.rbenv/plugins/ruby-build/bin

# virtualenv with virtualenvwrapper
[ -n $(which virtualenvwrapper.sh) ] \
	&& export WORKON_HOME=$HOME/.virtualenvs \
	&& source $(which virtualenvwrapper.sh)

# anything
[ -d ~/.local ] \
	&& PATH=$PATH:$HOME/.local/bin

# }}}
# Load completion scripts {{{

source ~/.sh_generic/completion.sh

# }}}


###################
# Define Commands #
###################
# Shortcut {{{

alias nvim=nvime
alias cdp=cd-finddir

# }}}
# Others {{{

# The bash like history
alias history='fc -l 1'

# Reload the zsh configrations
alias reload=". $ZDOTDIR/.zshrc && . $ZDOTDIR/.zprofile && echo '>> the zsh configrations are reloaded'"

# }}}


##################
# Manage Plugins #
##################
# Prepare zplug {{{

# Install automatically
if [ ! -d $ZPLUG_HOME ] ; then
	git clone https://github.com/zplug/zplug $ZPLUG_HOME
fi
source $ZPLUG_HOME/init.zsh

# }}}
# Before settings {{{

# zsh-shell-kawaii {{{

# for $SHELL_KAWAII_MORE_PROMPT_COMMAND
function show_cmdline_states () {
	function get_git_changes () {
		# Subtract a head line minute
		local changes=$(( $(git status --short 2> /dev/null | wc -l) - 1 ))
		if [ "$changes" -ge 1 ] ; then
			echo "%{$bg[white]$fg[black]%}[change:${changes}]%{$reset_color%}"
		fi
	}

	function get_git_commits () {
		local commits
		commits=$(git status --short 2> /dev/null | head -1 | grep -o '\[.*\]')
		if [ "$?" -eq 0 ] ; then
			echo "%{$bg[red]$fg[black]%}${commits}%{$reset_color%}"
		fi
	}

	function get_stash_status () {
		local item_num=$({git stash list 2> /dev/null || echo -n ''} | wc -l)
		if [ "$item_num" -ge 1 ] ; then
			echo "%{$bg[cyan]$fg[black]%}[stash:${item_num}]%{$reset_color%}"
		fi
	}

	function get_branch_name () {
		local branches
		branches=$(git branch 2> /dev/null)
		if [ "$?" -ne 0 ] ; then
			echo '[NO REPO]'
			exit
		fi
		local branch_name=$(echo $branches | grep '\*\s.*' | awk '{print $2}')
		echo "%{$bg[green]%}[${branch_name}]%{$reset_color%}"
	}

	function get_afu_vi_mode () {
		local cmd_mode_name='afu-vicmd'
		local keymap_name="$(echo $KEYMAP | sed -r 's/^(.)/\U\1/')"
		local color; [ "$KEYMAP" = "$cmd_mode_name" ] && color=red || color=blue
		echo "%{$bg[${color}]%}[${keymap_name}]%{$reset_color%}"
	}

	# Result
	echo " | $(get_git_changes)$(get_git_commits)$(get_stash_status)$(get_branch_name)$(get_afu_vi_mode)"
}

# }}}

# }}}
# Load zsh plugins {{{

# Start zplug
source $ZPLUG_HOME/init.zsh

# Plugin list
source ~/Repository/zsh-shell-kawaii/zsh-shell-kawaii.zsh  #zplug 'aiya000/zsh-shell-kawaii'
zplug 'aiya000/sh-hereis', use:'{init.sh,hereis.sh,place.sh,edit-places.sh,reload-places.sh}'
zplug 'aiya000/sh-tovim', as:command, use:tovim
zplug 'zsh-users/zsh-syntax-highlighting'
zplug 'joel-porquet/zsh-dircolors-solarized'
zplug 'hchbaw/auto-fu.zsh'; source $ZPLUG_HOME/repos/hchbaw/auto-fu.zsh/auto-fu.zsh  #NOTE: Can I execute source through zplug ?

# Load plugins
zplug load

#}}}
# Set plugin prefs {{{

# zsh-dircolors-solarized {{{

# Use dircolors.ansi-light thema
setupsolarized dircolors.ansi-light

# }}}

# }}}


# Do keymapping
source $ZDOTDIR/.zshrc.keymap

# For each environment
case $(uname) in
Linux )
	source ~/.sh_generic/linux.sh
	;;
Cygwin )
	source ~/.sh_generic/cygwin.sh
	HOME=/home/$USER
	PATH=$PATH:/cygdrive/c/Windows/system32:/cygdrive/c/Windows
	;;
esac

# If it exists, load environment config
if [ -f ~/.zshrc_env ] ; then
	source ~/.zshrc_env
fi

# Export Loaded Archive
alias zsh_rc_loaded='echo "rc_loaded"'
