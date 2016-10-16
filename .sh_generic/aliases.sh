#!/bin/sh

#
# This file define the aliases and functions
# You can sourcing this from .zshrc
# and you can sourcing .bashrc or another shell rc file, maybe
#

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
alias mv='mv -i'
alias cp='cp -i'
alias yi='yi --as=vim'

# }}}
# Laziness {{{

## git
alias g='git'
alias ga='git add'
alias gaa='git add -A'
alias gb='git branch'
alias gc='git commit'
alias gcam='git commit --amend'
alias gcm='git commit -m'
alias gco='git checkout'
alias gd='git diff'
alias gds='git diff --staged'
alias gl='git log'
alias gs='git status'
alias gss='git stash'
alias gdh='git diff HEAD~..HEAD'

## shell
alias la='ls -a --color=auto --group-directories-first'
alias ll='ls -l --color=auto --group-directories-first'
alias llh='ls -lh --color=auto --group-directories-first'
alias lla='ls -la --color=auto --group-directories-first'

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
alias vimls='vim -c "read! ls" -c "nnoremap <buffer> Q :<C-u>q<CR> | setl nolist buftype=nofile | normal! Gddgg"'

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
function twitter-usertimeline() {
	vim -c "TweetVimUserTimeline ${1}"
}
function tweet-say() {
	vim -c "TweetVimCommandSay ${1}" +q
}
function tweet-public-say() {
	vim -c "TweetVimSwitchAccount public_ai000ya" \
		-c "TweetVimCommandSay ${1}" \
		+q
}
alias adrone='vim +AdroneHome'
alias gstatus='vim -c "Gita status"'
function vim-session () {
	vim -c "UniteSessionLoad ${1}"
}

alias vim-build-configure-ubuntu='./configure --with-features=huge --enable-gui=gnome2 --enable-perlinterp --enable-rubyinterp --enable-luainterp --enable-fail-if-missing'
alias vim-build-make-mingw32='cd src && mingw32-make.exe -f Make_ming.mak GUI=yes IME=yes MBYTE=yes ICONV=yes DEBUG=no'

# }}}
# Shell Utils {{{

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

# git {{{
#
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

# Push current state local repository temporary
function git-push-temporary () {
	local temp_branch=deprecated_unsafe_temporary_branch
	if [ $(git status --short | wc -l) -ne 1 ] ; then
		git add -A
		git stash save
		git checkout -b $temp_branch
		git stash pop
		git add -A
		git commit
	else
		git checkout -b $temp_branch
	fi
	git push -uf origin $temp_branch
}

# }}}

# haskell-stack {{{

alias stack-build='stack build --ghc-options="-W"'

# }}}

# }}}
# Another aliases {{{

alias mysql='mysql --pager="less -r -S -n -i -F -X"'
alias docker-rm-all-containers='sudo docker rm `sudo docker ps -a -q`'
alias ctags-r='ctags --tag-relative --recurse --sort=yes'
alias date-simple='date +"%Y-%m-%d"'
alias cp-with-progress='rsync --partial --progress'

# Notify end of cli task
# example$ somecommand ; enotify
function _espeak () {
	espeak "$1" -s 150 -v +fex 2> /dev/null || \
	espeak "$1" -s 150 2> /dev/null
}
function enotify () {
	local exit_code=$?
	if [ $exit_code -eq 0 ] ; then
		_espeak 'Succeed!'
	else
		_espeak 'Exit with the error'
	fi
	return $exit_code
}

# }}}

# }}}
# Use each completions {{{

#TODO: Branch zsh env and bash env
# Use git-completion
if [ -f /usr/share/git/completion/git-completion.zsh -a ! -f $ZDOTDIR/_git ] ; then
	cp /usr/share/git/completion/git-completion.zsh $ZDOTDIR/_git
fi

# Use travis-completion
if [ -f ~/.travis/travis.sh ] ; then
	source ~/.travis/travis.sh
fi

# Use stack-completion
if [ -s $(which stack) ] ; then
	# This completion needs compinit and bashcompinit function
	# > autoload -U compinit     && compinit
	# > autoload -U bashcompinit && bashcompinit
	eval "$(stack --bash-completion-script stack)"
fi

# }}}
