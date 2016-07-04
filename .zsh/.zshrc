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

# Use color
autoload colors
colors

# Use history
setopt HIST_IGNORE_DUPS
setopt EXTENDED_HISTORY
setopt SHARE_HISTORY
setopt HIST_REDUCE_BLANKS

# Don't use screen stopping
stty stop  undef
stty start undef

# Prompt visual
function zle-line-init zle-keymap-select {
	VIM_NORMAL="%{$bg[red]%}[NORMAL]%{$reset_color%}"
	VIM_INSERT="%{$bg[blue]%}[INSERT]%{$reset_color%}"
	RPS1="${${KEYMAP/vicmd/$VIM_NORMAL}/(main|viins)/$VIM_INSERT}"
	RPS2=$RPS1
	zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# }}}
# Set zsh key-mappings {{{

# Use viins
bindkey -v

# Vim nize
bindkey -M vicmd '_' vi-first-non-blank

# Emacs nize
bindkey -M viins '^r' history-incremental-search-backward
bindkey -M viins '^s' history-incremental-search-forward
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
bindkey -M viins '^l' vi-cmd-mode
bindkey -M viins '^]' clear-screen

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
# I'm a coward {{{

alias mv='mv -i'
alias cp='cp -i'

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

alias vimconfig='dotfile_config .vimrc'
alias gvimconfig='dotfile_config .gvimrc'
alias vimshconfig='dotfile_config .vimshrc'
alias vim-bashrc='dotfile_config .bashrc && [ -f ~/.bashrc ] && ( source ~/.bashrc && echo ">> .bashrc loaded" )'
alias vim-bashpr='dotfile_config .bash_profile && [ -f ~/.bashrc ] && ( source ~/.bash_profile && echo ">> .bash_profile loaded" )'
alias vim-zshrc="dotfile_config .zshrc && [ -f $ZDOTDIR/.zshrc ] && ( source $ZDOTDIR/.zshrc && echo '>> .zshrc loaded' )"
alias vim-zshpr="dotfile_config .zprofile && [ -f $ZDOTDIR/.zshrc ] && ( source $ZDOTDIR/.zprofile && echo '>> .zprofile loaded' )"

alias vimshell='vim +VimShell'
alias vimconsole='vim +VimConsoleOpen'
alias twitter='vim +TweetVimHomeTimeline'
alias tweet='vim +TweetVimSay'
alias twitter-public='vim +TwitterPublic'
alias tweet-public='vim +TweetPublic'
alias adrone='vim +AdroneHome'
alias gstatus='vim -c "Gita status"'

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

# Basic backup method
function bak() {
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

if [ $IS_UBUNTU -eq 1 ] ; then
	alias ssleep='sudo pm-suspend'
	alias hibernate='sudo pm-hibernate'
elif [ $IS_CYGWIN -eq 1 ] ; then
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

# Use fzf-completion
#if [ -f /usr/share/fzf/key-bindings.zsh ] ; then
#	source /usr/share/fzf/key-bindings.zsh
#fi

# Use stack-completion
if [ -s "`which stack`" ] ; then
	autoload -U +X compinit && compinit
	autoload -U +X bashcompinit && bashcompinit
	eval "$(stack --bash-completion-script stack)"
fi

# }}}


##################
# Manage Plugins #
##################
# Prepare zplug {{{

if [ ! -d $ZPLUG_HOME ] ; then
	git clone https://github.com/zplug/zplug $ZPLUG_HOME
fi
source $ZPLUG_HOME/zplug

# }}}
# Load zsh plugins {{{

# Start zplug
source $ZPLUG_HOME/init.zsh

zplug 'aiya000/sh-hereis', use:'{init.sh,hereis.sh,place.sh,edit-places.sh,reload-places.sh}'
zplug 'aiya000/zsh-shell-kawaii'
zplug 'zsh-users/zsh-syntax-highlighting'

# Load plugins
zplug load --verbose

#plugin_dir=$ZDOTDIR/plugin
#
#if [ ! -d "$plugin_dir" ] ; then
#	mkdir "$plugin_dir"
#fi
#
#local_plugins=( \
#	shell_kawaii.sh \
#	ezoe_command_not_found_handle.sh \
#	tovim.sh \
#)
#
#for (( i = 0; i < ${#local_plugins[@]}; ++i )) ; do
#	source "${plugin_dir}/${local_plugins[$i]}"
#done
#
#unset local_plugins plugin_dir

#}}}


# Export Loaded Archive
alias zsh_rc_loaded='echo "rc_loaded"'
