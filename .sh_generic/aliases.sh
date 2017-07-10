#!/bin/bash

# shellcheck disable=SC1090
source ~/.sh_generic/premise.sh

#
# This file define the aliases and functions
# You can load this from .zshrc
# and you can load .bashrc or another shell rc file, maybe
#

__aliases_basedir=$(dirname "$0")

# Override existed name {{{

alias ls='ls --color=auto --group-directories-first'
alias mv='mv -i'
alias cp='cp -i'
alias dd='dd status=progress'
alias df='df -h'
alias du='du -h'
alias_of sudo='sudo '  # Enable aliases on sudo
alias_of mysql='mysql --pager="less -r -S -n -i -F -X"'
alias_of hoe='stack exec --silent -- hoe'

# }}}
# Load ./aliases/** {{{

for script in $(ls $__aliases_basedir/aliases/functions/*.sh) ; do
	source "$script"
done
source $__aliases_basedir/aliases/vim.sh

# }}}
# Git {{{

if i_have git ; then
	# Short hands
	git_taking_limit=100
	alias _gr='git reset'
	alias _grh='git reset --hard'
	alias _grs='git reset --soft'
	alias _grs_HEAD-='git reset --soft HEAD~'
	alias g='git'
	alias ga='git add'
	alias gaa='git add -A'
	alias gap='git add -p'
	alias gb='git branch'
	alias gbd='git branch --delete'
	alias _gbdf='git branch --delete --force'
	alias gbm='git branch -m'
	alias gc='git commit --verbose'
	alias gcam='git commit --verbose --amend'
	alias gcm='git commit -m'
	alias gco='git checkout'
	alias gcob='git checkout -b'
	alias gd='git diff'
	alias gdh='git diff HEAD~..HEAD'
	alias gds='git diff --staged'
	alias gl="git log --name-only -$git_taking_limit"
	alias glo="git log --oneline -$git_taking_limit"
	alias glp="git log --patch -$git_taking_limit"
	alias gmv='git mv'
	alias gr='git rebase'
	alias gra='git rebase --abort'
	alias grc='git rebase --continue'
	alias gri='git rebase --interactive'
	alias grm='git rm'
	alias grmc='git rm --cached'
	alias gs='git status'
	alias gss='git stash'
	alias gsss='git stash save'
	alias gssl='git stash list | xargs -I {} echo {}'
	alias gmt='git mergetool'
	alias gsm='git submodule'
	alias gsma='git submodule add'
	alias gsmd='git submodule deinit'
	alias gcherry='git cherry-pick'
	alias gclean='git clean -f'
	alias gret='git return'  # See .gitconfig
	alias gp='git push'
	unset git_taking_limit

	# Set casual user.name and user.email at local
	alias git-set-casual-name='git config --local user.name aiya000 && git config --local user.email aiya000.develop@gmail.com ; git config --local user.name ; git config --local user.email'
fi

# }}}
# Others {{{

# Short hands
alias la='ls -a --color=auto --group-directories-first'
alias ll='ls -l --color=auto --group-directories-first'
alias llh='ls -lh --color=auto --group-directories-first'
alias lla='ls -la --color=auto --group-directories-first'

alias x=xdg-open
alias t=tmux
alias e=nvim

alias ei=exit

alias nt=nterminal
alias ngs=vimgs

alias sb='stack build'
alias se='stack exec --'
alias st='stack test'
alias srunghc='stack runghc --'
alias sghci='stack ghci --'

alias date-simple='date +"%Y-%m-%d"'
alias mount4u.ntfs="sudo mount -o user=$(whoami),uid=$(id -u),gid=$(id -g),iocharset=utf8"
alias mount4u.vfat=mount4u.ntfs
alias mount4u.ext2='sudo mount -o iocharset=utf8'
alias mount4u.ext3=mount4u.ext2
alias mount4u.ext4=mount4u.ext2

i_have docker && alias docker-rm-all-containers='sudo docker rm `sudo docker ps -a -q`'
i_have rsync  && alias cp-with-progress='rsync --partial --progress'
i_have watch  && alias wifi-hardware-check='watch -n1 rfkill list all'
i_have ctags  && alias ctags-r='ctags --tag-relative --recurse --sort=yes'
i_have tmux   && alias tmuxa='tmux attach'
i_have nmcli  && alias nmcli-connect-wifi='nmcli device wifi connect'

# Generate items for autotools
alias autofiles='touch AUTHORS COPYING ChangeLog INSTALL NEWS README'

function virtualenv-activate () {
	if [ -f ./.venv/bin/activate ] ; then
		. ./.venv/bin/activate
	else
		echo './.venv/bin/activate was not found, please load it yourself...' > /dev/stderr
		return 1
	fi
}

# }}}

export PATH=$PATH:~/.sh_generic/bin
