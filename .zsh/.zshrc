#!/usr/bin/env zsh
source ~/.sh_generic/helper.sh

###################
# Check .zprofile #
###################
# The counterplan for if .zprofile never loaded
if [ -z "`alias | grep zsh_pr_loaded`" ] ; then
	source $ZDOTDIR/.zprofile
fi


#############
# Configure #
#############
# Define global variables {{{

in_linux=$(is_your_os_name Linux)
in_cygwin=$(is_your_os_name Cygwin)

# }}}
# Set zsh options {{{

# Use modules
autoload -U colors            && colors
autoload -U compinit          && compinit
autoload -U bashcompinit      && bashcompinit
autoload -U edit-command-line && zle -N edit-command-line

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

# Prompt visual
function zle-line-init zle-keymap-select {
	# Detect vi-mode
	local vi_normal="%{$bg[red]%}[NORMAL]%{$reset_color%}"
	local vi_insert="%{$bg[blue]%}[INSERT]%{$reset_color%}"
	local vi_status="${${KEYMAP/vicmd/$vi_normal}/(main|viins)/$vi_insert}"

	# Detect git branch
	local branch=$({git branch --contains 2> /dev/null || echo ' NO REPO'} | cut -d' ' -f2- | xargs -I x echo \[x\])
	local git_branch="%{${${branch/\[NO REPO\]/}/${branch}/$bg[green]}%}${branch}%{$reset_color%}"

	# Result
	RPROMPT="${git_branch}${vi_status}"
	zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# }}}
# Set zsh key-mappings {{{

# Prepare {{{

alias __fzf_tmux_cmd='fzf-tmux --no-sort --tac --cycle --bind=ctrl-j:accept,ctrl-k:kill-line'
alias __fzf_cmd='fzf --no-sort --tac --cycle --bind=ctrl-j:accept,ctrl-k:kill-line'

# history-incremental-search-backward uses fzf
function fzf-history-incremental-search-backward () {
	selected=$(fc -ln 1 | __fzf_tmux_cmd)
	BUFFER="$selected"
	CURSOR=${#BUFFER}
}
zle -N fzf-history-incremental-search-backward

# Select file on the fzf
function fzf-file-finder-expand () {
	selected=$(find . | __fzf_cmd --multi)
	zle redisplay
	BUFFER="${BUFFER}${selected}"
	CURSOR=${#BUFFER}
}
zle -N fzf-file-finder-expand

# }}}

# Use viins
bindkey -v

# Vim nize
bindkey -M vicmd '_'  vi-first-non-blank
bindkey -M vicmd 'g_' vi-end-of-line

# Emacs nize
bindkey -M viins '^r' fzf-history-incremental-search-backward
bindkey -M viins '^n' down-history
bindkey -M viins '^p' up-history
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins '^b' backward-char
bindkey -M viins '^f' forward-char
bindkey -M viins '^k' kill-line
bindkey -M viins '^u' backward-kill-line
bindkey -M viins '^d' delete-char

# My taste
bindkey -M vicmd '^v'   edit-command-line
bindkey -M viins '^l'   vi-cmd-mode
bindkey -M viins '^]'   clear-screen
bindkey -M viins '^x^f' fzf-file-finder-expand

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


###################
# Define Commands #
###################
# Load general aliases {{{

source ~/.sh_generic/aliases.sh

# }}}
# Override default {{{

alias vim=vime
alias nvim=nvime

# }}}
# Others {{{

# bash like history
alias history='fc -l 1'

# Reload zsh configrations
alias reload=". $ZDOTDIR/.zshrc && . $ZDOTDIR/.zprofile && echo '>> zsh configrations reloaded'"

# }}}


##################
# Manage Plugins #
##################
# Prepare zplug {{{

# Install automatically
if [ ! -d $ZPLUG_HOME ] ; then
	git clone https://github.com/zplug/zplug $ZPLUG_HOME
fi
source $ZPLUG_HOME/zplug

# }}}
# Load zsh plugins {{{

# Start zplug
source $ZPLUG_HOME/init.zsh

# Plugin list
zplug 'aiya000/sh-hereis', use:'{init.sh,hereis.sh,place.sh,edit-places.sh,reload-places.sh}'
zplug 'aiya000/zsh-shell-kawaii'
zplug 'aiya000/sh-tovim', as:command, use:tovim
zplug 'zsh-users/zsh-syntax-highlighting'
zplug 'joel-porquet/zsh-dircolors-solarized'

# Load plugins
zplug load

#}}}
# Plugin settings {{{

# zsh-dircolors-solarized {{{

# Use dircolors.ansi-light thema
setupsolarized dircolors.ansi-light

# }}}

# }}}


# For each environment
if [ $in_linux -eq 1 ] ; then
	source ~/.sh_generic/linux.sh
elif [ $in_cygwin -eq 1 ] ; then
	source ~/.sh_generic/cygwin.sh
	HOME=/home/$USER
	PATH=$PATH:/cygdrive/c/Windows/system32:/cygdrive/c/Windows
fi

# If it exists, load environment config
if [ -f ~/.zshrc_env ] ; then
	source ~/.zshrc_env
fi

# Export Loaded Archive
alias zsh_rc_loaded='echo "rc_loaded"'
