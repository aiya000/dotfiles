#!/usr/bin/env zsh

###################
# Check .zprofile #
###################
# The counterplan for if .zprofile never loaded
if [ -z "`alias | grep zsh_pr_loaded`" ] ; then
	source $ZDOTDIR/.zprofile
fi


##############
# Config zsh #
##############
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

# Use standard style history
setopt hist_ignore_dups
setopt extended_history
setopt hist_reduce_blanks

# Other opts
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

	# Detect git repository
	local branch=$({git branch 2> /dev/null || echo ' NO REPO'} | cut -d' ' -f2- | xargs -I x echo \[x\])
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


###################
# Define Commands #
###################
# Define aliases and functions {{{

# Prepare {{{

function dotfile_config () {
	if [ -f "${HOME}/.dotfiles/${1}" ] ; then
		"$EDITOR" "${HOME}/.dotfiles/${1}"
	else
		"$EDITOR" "${HOME}/${1}"
	fi
}

# }}}
# Overrides {{{

alias ls='ls --color=auto --group-directories-first'
alias history='fc -l 1'

# }}}
# I'm a coward {{{

alias mv='mv -i'
alias cp='cp -i'

# }}}
# Laziness {{{

## git
alias g='git'
alias ga='git add'
alias gaa='git add -A'
alias gc='git commit'
alias gcm='git commit -m'
alias gd='git diff'
alias gl='git log'
alias gs='git status'
alias gss='git stash'

## shell
alias la='ls -a --color=auto --group-directories-first'
alias ll='ls -l --color=auto --group-directories-first'
alias llh='ls -lh --color=auto --group-directories-first'

# }}}
# Vim Utils {{{

alias vi='vim -u NONE --noplugin'
alias gvi='gvim -u NONE -U NONE --noplugin'
alias vimless='vim - -R -c "setl nolist | nnoremap <buffer> Q :<C-u>q<CR>"'
alias runvim='vim -N -c :quitall! -u'
alias vime='vim -c ":bufdo tab split"'
alias vim-record-startup='vim --startuptime vim_startup_time +q && vim -c "set bt=nofile ft=vim | r vim_startup_time | call system(\"rm vim_startup_time\") | normal! gg3dd"'
alias vimclearview='rm ~/.backup/vim_backup/view/*'
alias vimclearswp='rm ~/.backup/vim_backup/swp/*'
alias vimclearundo='rm ~/.backup/vim_backup/undo/*'
alias vimclearcache='vimclearview ; vimclearundo ; vimclearswp'
function vimls () { ls "$1" | vim - -R -c "setl nolist | nnoremap <buffer> Q :<C-u>q<CR>" }

alias vimconfig='dotfile_config .vimrc'
alias gvimconfig='dotfile_config .gvimrc'
alias vimshconfig='dotfile_config .vimshrc'
alias vim-bashrc='dotfile_config .bashrc && [ -f ~/.bashrc ] && ( source ~/.bashrc && echo ">> .bashrc loaded" )'
alias vim-bashpr='dotfile_config .bash_profile && [ -f ~/.bashrc ] && ( source ~/.bash_profile && echo ">> .bash_profile loaded" )'
alias vim-zshrc="dotfile_config .zsh/.zshrc && [ -f $ZDOTDIR/.zshrc ] && ( source $ZDOTDIR/.zshrc && echo '>> .zshrc loaded' )"
alias vim-zshpr="dotfile_config .zsh/.zprofile && [ -f $ZDOTDIR/.zshrc ] && ( source $ZDOTDIR/.zprofile && echo '>> .zprofile loaded' )"

alias vimshell='vim +VimShell'
alias vimconsole='vim +VimConsoleOpen'
alias twitter='vim +TweetVimHomeTimeline'
alias tweet='vim +TweetVimSay'
alias twitter-public='vim +TwitterPublic'
alias tweet-public='vim +TweetPublic'
alias adrone='vim +AdroneHome'
alias gstatus='vim -c "Gita status"'
function vim-session () { vim -c "UniteSessionLoad ${1}" }

alias vim-build-configure-ubuntu='./configure --with-features=huge --enable-gui=gnome2 --enable-perlinterp --enable-rubyinterp --enable-luainterp --enable-fail-if-missing'
alias vim-build-make-mingw32='cd src && mingw32-make.exe -f Make_ming.mak GUI=yes IME=yes MBYTE=yes ICONV=yes DEBUG=no'

# }}}
# Shell Utils {{{

# Reload zsh configrations
alias reload=". $ZDOTDIR/.zshrc && . $ZDOTDIR/.zprofile && echo '>> zsh configrations reloaded'"

# Console output pipe to clipboard
if [ $IS_CYGWIN -eq 1 ] ; then
	alias pbcopy='tee /dev/clipboard > /dev/null'
else
	alias pbcopy='xsel --clipboard --input'
fi

# Toggle file extensions
function bak () {
	if [ -z "$1" ] ; then
		echo 'error: require 1 argument' 1>&2
		return 1
	fi
	if [ ! -e "$1" ] ; then
		echo "error: not found file '${1}'" 1>&2
		return 1
	fi
	if [ -n "`echo \"${1}\" | grep 'bak$'`" ] ; then
		# Remove extension '.bak'
		mv "$1" "${1%.*}"
	else
		# Append extension '.bak'
		mv "$1" "${1}.bak"
	fi
}

# }}}
# Environment Conditions {{{

if  [ $IS_CYGWIN -eq 1 ] ; then
	alias cygrunsrv='cocot cygrunsrv'
	alias csc='cocot csc'
	alias ifconfig='cocot ipconfig'
	alias ping='cocot ping'
	alias traceroute='cocot tracert'
	alias route='cocot route'
	alias netstat='cocot netstat'
	alias nslookup='cocot nslookup'
	alias updatedb='updatedb --localpaths="/bin /dev /etc /home /lib /usr /var /opt" --prunepaths="/usr/tmp /var/tmp"'
	alias mysql='mysql --pager="less -r -S -n -i -F -X" --protocol=TCP'
else
	# $IS_UBUNTU or others
	alias ssleep='sudo pm-suspend'
	alias hibernate='sudo pm-hibernate'
fi

# }}}
# Development supports {{{

# Generate items for autotools
alias autofiles='touch AUTHORS COPYING ChangeLog INSTALL NEWS README'

# git
# <Warn> fully change git commit author and email
function git-fully-change-author-and-email() { #{{{
	git_user_name="$1"
	git_email="$2"
	git filter-branch -f --env-filter \
		"GIT_AUTHOR_NAME='${git_user_name}'; GIT_AUTHOR_EMAIL='${git_email}'; GIT_COMMITTER_NAME='${git_user_name}'; GIT_COMMITTER_EMAIL='${git_email}';" \
		HEAD
	unset git_user_name git_email
} #}}}

# Set casual user.name and user.email at local
alias git-set-casual-name='git config --local user.name aiya000 && git config --local user.email aiya000.develop@gmail.com ; git config --local user.name ; git config --local user.email'

# Do merge, branch -d and delete remote branch
function git-seq-merge-bd-push_bd() { #{{{
	target_remote="$1"
	target_branch="$2"
	git merge "$target_branch" && \
		git branch -d "$target_branch" && \
		git push -u "$target_remote" ":${target_branch}"
} #}}}

# }}}
# Another aliases {{{

alias mysql='mysql --pager="less -r -S -n -i -F -X"'
alias docker-rm-archives='sudo docker rm `sudo docker ps -a -q`'
alias ctags-r='ctags --tag-relative --recurse --sort=yes'
alias date-simple='date +"%Y-%m-%d"'

# Notify end of cli task
# example$ somecommand ; enotify
function enotify () {
	if [ $? -eq 0 ] ; then
		espeak 'Succeed!' 2> /dev/null
	else
		espeak 'Done with the error' 2> /dev/null
	fi
}

# }}}

# }}}
# Use each completions {{{

# Use git-completion
if [ -f /usr/share/git/completion/git-completion.zsh -a ! -f $ZDOTDIR/_git ] ; then
	cp /usr/share/git/completion/git-completion.zsh $ZDOTDIR/_git
fi

# Use travis-completion
if [ -f ~/.travis/travis.sh ] ; then
	source ~/.travis/travis.sh
fi

# Use stack-completion
if [ -s "`which stack`" ] ; then
	# This completion needs compinit and bashcompinit function
	# > autoload -U compinit     && compinit
	# > autoload -U bashcompinit && bashcompinit
	eval "$(stack --bash-completion-script stack)"
fi

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


# If it exists, load environment config
if [ -f ~/.zshrc_env ] ; then
	source ~/.zshrc_env
fi

# Export Loaded Archive
alias zsh_rc_loaded='echo "rc_loaded"'
