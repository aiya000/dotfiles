#!/usr/bin/env zsh

###################
# Check .zprofile #
###################
# The counterplan for if .zprofile never loaded
if [ -z "$(alias | grep zsh_pr_loaded)" ] ; then
	source $ZDOTDIR/.zprofile
fi


# Load premised commands
source ~/.sh_generic/aliases.sh

###########################################
# Configure zsh with the local conditions #
###########################################
# Set zsh options {{{

autoload -U colors       && colors       # Use color variables (Ex: $bg and $fg)
autoload -U compinit     && compinit     # Use zsh standard completion
autoload -U bashcompinit && bashcompinit # Use bash compatible completion

zstyle ':completion:*' menu select                        # Highlight selecting item in the menu
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # Case insensitive completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}     # list-colors uses $LS_COLORS's colors

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
# Load completion scripts {{{

source ~/.sh_generic/completion.sh

# }}}


###################
# Define Commands #
###################
# {{{

alias nvim=nvime
alias cdp=cd-finddir
alias psf=ps-fzf
alias ki=killing-art
alias history='fc -li 1'
alias reload=". $ZDOTDIR/.zshrc && . $ZDOTDIR/.zprofile && echo '>> the zsh configrations are reloaded'"

# }}}


##################
# Manage Plugins #
##################
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

	function get_git_stash_status () {
		local item_num=$({git stash list 2> /dev/null || echo -n ''} | wc -l)
		if [ "$item_num" -ge 1 ] ; then
			echo "%{$bg[cyan]$fg[black]%}[stash:${item_num}]%{$reset_color%}"
		fi
	}

	function get_git_branch_name () {
		local branches
		branches=$(git branch 2> /dev/null)
		if [ "$?" -ne 0 ] ; then
			echo '[NO REPO]'
			exit
		fi
		local branch_name=$(echo $branches | grep '\*\s.*' | awk '{print $2}')
		echo "%{$bg[green]%}[${branch_name}]%{$reset_color%}"
	}

	function get_zle_mode () {
		local expected_normal_mode='vicmd'
		local keymap_name="$(echo $KEYMAP | sed -r 's/^(.)/\U\1/')"
		local color ; [ "$KEYMAP" = "$expected_normal_mode" ] && color=red || color=blue
		echo "%{$bg[${color}]%}[${keymap_name}]%{$reset_color%}"
	}

	function get_virtualenv_availability () {
		if [ -n "$VIRTUAL_ENV" ] ; then
			local env_name=$(echo "$VIRTUAL_ENV" | sed -r 's;^/.*/(.*)/\.venv$;\1;')
			echo "%{$bg[yellow]$fg[black]%}[${env_name}]%{$reset_color%}"
		elif [ -d "$(pwd)/.venv" ] ; then
			echo "%{$bg[red]$fg[black]%}[./.venv was found]%{$reset_color%}"
		fi
	}

	# Result
	echo " | $(get_git_changes)$(get_git_commits)$(get_git_stash_status)$(get_git_branch_name)$(get_zle_mode)$(get_virtualenv_availability)"
}

# }}}

# }}}
# Load zsh plugins {{{

source ~/.zsh/zsh-zapack/zapack.zsh

#}}}
# Set plugin prefs {{{

# zsh-dircolors-solarized {{{

# Use dircolors.ansi-light theme
setupsolarized dircolors.ansi-light

# }}}

# }}}


# Do keymapping
source $ZDOTDIR/.zshrc.keymap

# Load environment by default
load-my-env haskell

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
