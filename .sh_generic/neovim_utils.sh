#!/bin/sh

function ndotfile_config () {
	if [ -f "${HOME}/.dotfiles/${1}" ] ; then
		nvim "${HOME}/.dotfiles/${1}"
	else
		nvim "${HOME}/${1}"
	fi
}

# Startup
alias nvi='nvim -u NONE --noplugin'
alias nvime='nvim -c ":bufdo tab split"'
alias nvimless='nvim - -R -c "setl nolist | nnoremap <buffer> Q :<C-u>q<CR>"'
alias nvimls='nvim -c "read! ls" -c "nnoremap <buffer> Q :<C-u>q<CR> | setl nolist buftype=nofile | normal! Gddgg"'
function vimman () {
	nvim -c "Man ${1}" +only
}

# Config
alias nvimconfig='ndotfile_config .vim/init.vim'

# Plugin
alias nvimconsole='nvim +VimConsoleOpen'
alias nadrone='nvim +AdroneHome'

function nvim-session () {
	nvim -c "UniteSessionLoad ${1}"
}


## Twitter
alias ntwitter='nvim +TweetVimHomeTimeline'
alias ntweet='nvim +TweetVimSay'
alias ntwitter-public='nvim +TwitterPublic'
alias ntweet-public='nvim +TweetPublic'

function ntwitter-usertimeline() {
	nvim -c "TweetVimUserTimeline ${1}"
}

## :terminal
alias nweechat='nvim -c ":terminal weechat"'
alias nterminal='nvim +terminal'
