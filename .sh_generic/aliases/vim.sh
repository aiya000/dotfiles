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

alias nvimdiff='\nvim -d'

alias vimless='nvim - -c "setl buftype=nofile nolist | nnoremap <buffer> Q :<C-u>q<CR>"'
alias vimls='nvim -c "read! ls" -c "nnoremap <buffer> Q :<C-u>q<CR> | setl nolist buftype=nofile | normal! Gddgg"'
function vimman () {
	nvim -c "Man ${1}" +only
}

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
alias nterminal='nvim +terminal'
