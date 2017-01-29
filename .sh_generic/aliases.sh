#!/bin/sh

#
# This file define the aliases and functions
# You can load this from .zshrc
# and you can load .bashrc or another shell rc file, maybe
#

# Override existed name {{{

alias ls='ls --color=auto --group-directories-first'
alias mv='mv -i'
alias cp='cp -i'
alias_of mysql='mysql --pager="less -r -S -n -i -F -X"'
alias_of fzf='fzf --no-sort --tac --cycle --bind=ctrl-j:accept,ctrl-k:kill-line'
alias_of fzf-tmux='fzf-tmux --no-sort --tac --cycle --bind=ctrl-j:accept,ctrl-k:kill-line'

# }}}
# Vim and NeoVim {{{

basedir=$(dirname $0)
source $basedir/vim_utils.sh
source $basedir/neovim_utils.sh

alias e=nvim

# }}}
# Git {{{

if i_have git ; then
	# Short hands
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
	alias gcob='git checkout -b'
	alias gd='git diff'
	alias gdh='git diff HEAD~..HEAD'
	alias gds='git diff --staged'
	alias gl='git log'
	alias gmv='git mv'
	alias gr='git rebase'
	alias gra='git rebase --abort'
	alias grc='git rebase --continue'
	alias gri='git rebase --interactive'
	alias grm='git rm'
	alias gs='git status'
	alias gss='git stash'

	# Set casual user.name and user.email at local
	alias git-set-casual-name='git config --local user.name aiya000 && git config --local user.email aiya000.develop@gmail.com ; git config --local user.name ; git config --local user.email'
fi

# }}}
# The completions {{{

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
# Others {{{

# Short hands
alias la='ls -a --color=auto --group-directories-first'
alias ll='ls -l --color=auto --group-directories-first'
alias llh='ls -lh --color=auto --group-directories-first'
alias lla='ls -la --color=auto --group-directories-first'

alias date-simple='date +"%Y-%m-%d"'
alias mount4u="sudo mount -o user=$(whoami),uid=1000,gid=1000,iocharset=utf8"

i_have docker && alias docker-rm-all-containers='sudo docker rm `sudo docker ps -a -q`'
i_have rsync  && alias cp-with-progress='rsync --partial --progress'
i_have watch  && alias wifi-hardware-check='watch -n1 rfkill list all'
i_have ctags  && alias ctags-r='ctags --tag-relative --recurse --sort=yes'

# Generate items for autotools
alias autofiles='touch AUTHORS COPYING ChangeLog INSTALL NEWS README'

# }}}
