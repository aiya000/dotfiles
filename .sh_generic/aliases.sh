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

# Overrides {{{

alias ls='ls --color=auto --group-directories-first'
alias mv='mv -i'
alias cp='cp -i'
alias mysql='mysql --pager="less -r -S -n -i -F -X"'

# }}}
# Utililty {{{

## git
alias g='git'
alias ga='git add'
alias gaa='git add -A'
alias gap='git add -p'
alias gb='git branch'
alias gbd='git branch --delete'
alias gc='git commit'
alias gcam='git commit --amend'
alias gcm='git commit -m'
alias gco='git checkout'
alias gd='git diff'
alias gdh='git diff HEAD~..HEAD'
alias gds='git diff --staged'
alias gl='git log'
alias gr='git rebase'
alias grc='git rebase --continue'
alias gri='git rebase --interactive'
alias gs='git status'
alias gss='git stash'

## shell
alias la='ls -a --color=auto --group-directories-first'
alias ll='ls -l --color=auto --group-directories-first'
alias llh='ls -lh --color=auto --group-directories-first'
alias lla='ls -la --color=auto --group-directories-first'

# Start cmd without stdout and stderr in background
function startbg () {
	$@ > /dev/null 2>&1 &
}

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
fi

# }}}
# Development supports {{{

# Generate items for autotools
alias autofiles='touch AUTHORS COPYING ChangeLog INSTALL NEWS README'

# git {{{

# Set casual user.name and user.email at local
alias git-set-casual-name='git config --local user.name aiya000 && git config --local user.email aiya000.develop@gmail.com ; git config --local user.name ; git config --local user.email'

# <Warn> fully change git commit author and email
function git-unsafe-commiter-changer () {
	git_user_name="$1"
	git_email="$2"
	git filter-branch -f --env-filter \
		"GIT_AUTHOR_NAME='${git_user_name}'; GIT_AUTHOR_EMAIL='${git_email}'; GIT_COMMITTER_NAME='${git_user_name}'; GIT_COMMITTER_EMAIL='${git_email}';" \
		HEAD
	unset git_user_name git_email
}

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

# }}}
# Another aliases {{{

alias docker-rm-all-containers='sudo docker rm `sudo docker ps -a -q`'
alias ctags-r='ctags --tag-relative --recurse --sort=yes'
alias date-simple='date +"%Y-%m-%d"'
alias cp-with-progress='rsync --partial --progress'
alias wifi-hardware-check='watch -n1 rfkill list all'

# Notify end of cli task
# Example)
#     prompt$ cp -r foo bar ; enotify
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
# Vim {{{

basedir=$(dirname $0)
source $basedir/vim_utils.sh

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
