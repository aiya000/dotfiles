#!/bin/bash
# This makes the aliases with both vim and neovim

function dotfile_config () {
	if [ -f "${HOME}/.dotfiles/${1}" ] ; then
		nvim "${HOME}/.dotfiles/${1}"
	else
		nvim "${HOME}/${1}"
	fi
}

# Vim
alias vi='vim -u NONE --noplugin'
alias vime='vim -c ":bufdo tab split" +q'
function vim-session () {
	vim -c "UniteSessionLoad ${1}"
}


# NeoVim
alias nvime='nvim -c ":bufdo tab split" +q'
function nvim-session () {
	nvim -c "UniteSessionLoad ${1}"
}

# :SessionSaveInGitBranch compatibled command. (it is defined in dotfiles/.vim/plugin/vimrc.vim)
# Open the nvim session which is associated the current git branch
function nvim-session-current () {
	local git_root_dir
	local branch
	local branch_
	local session_name

	#TODO: Detect git root directory
	git_root_dir=$(pwd | sed -r 's;.*/(.*);\1;')
	branches=$(git branch 2> /dev/null)
	# shellcheck disable=SC2181
	if [ "$?" -ne 0 ] ; then
		echo 'is here the git branch ?' > /dev/stderr
		exit
	fi

	branch="$(echo "$branches" | grep '\*\s.*' | awk '{print $2}')"
	branch_="$(echo "$branch" | sed -r 's;/;-;g')"
	session_name="${git_root_dir}-${branch_}.vim"
	nvim-session "$session_name"
}

alias nvimdiff='\nvim -d'

alias vimless='nvim - -c "setl buftype=nofile nolist | nnoremap <buffer> Q :<C-u>q<CR>"'
alias vimls='nvim -c "read! ls" -c "nnoremap <buffer> Q :<C-u>q<CR> | setl nolist buftype=nofile | normal! Gddgg"'
function vimman () {
	nvim -c "Man ${1}" +only
}

alias vimgs='nvim +"Gina status" +only'

# Plugin
alias vimshell='nvim +VimShell'
alias vimconsole='nvim +VimConsoleOpen'
alias adrone='nvim +AdroneHome'
function lingr () {
	nvim -c "J6uil ${1}"
}

## Twitter
alias twitter='nvim +TweetVimHomeTimeline'
alias tweet='nvim +TweetVimSay'
alias twitter-public='nvim +TwitterPublic'
alias tweet-public='nvim +TweetPublic'

function twitter-usertimeline() {
	nvim -c "TweetVimUserTimeline ${1}"
}

function tweet-say() {
	yes | vim -c "TweetVimCommandSay ${1}" +q
}

function tweet-public-say() {
	yes | vim \
		-c "TweetVimSwitchAccount public_ai000ya" \
		-c "TweetVimCommandSay ${1}" \
		+q
}

## :terminal
alias nweechat='nvim -c ":terminal weechat"'
alias nterminal='nvim +terminal +"setf term-zsh"'
