#!/bin/bash

function dotfile_config () {
	if [ -f "${HOME}/.dotfiles/${1}" ] ; then
		nvim "${HOME}/.dotfiles/${1}"
	else
		nvim "${HOME}/${1}"
	fi
}

# Startup
alias vi='nvim -u NONE --noplugin'
alias vime='nvim -c ":bufdo tab split" +q'
alias vimless='nvim - -c "setl buftype=nofile nolist | nnoremap <buffer> Q :<C-u>q<CR>"'
alias vimls='nvim -c "read! ls" -c "nnoremap <buffer> Q :<C-u>q<CR> | setl nolist buftype=nofile | normal! Gddgg"'
function vimman () {
	nvim -c "Man ${1}" +only
}

# Config
alias vimconfig='dotfile_config .vim/init.vim'

# Plugin
alias vimshell='nvim +VimShell'
alias vimconsole='nvim +VimConsoleOpen'
alias adrone='nvim +AdroneHome'

function vim-session () {
	nvim -c "UniteSessionLoad ${1}"
}

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

## :terminal
alias weechat='nvim -c ":terminal weechat"'
alias nterminal='nvim +terminal'
