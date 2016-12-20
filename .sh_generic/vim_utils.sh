#!/bin/sh

function dotfile_config () {
	if [ -f "${HOME}/.dotfiles/${1}" ] ; then
		"$EDITOR" "${HOME}/.dotfiles/${1}"
	else
		"$EDITOR" "${HOME}/${1}"
	fi
}

# Startup
alias vi='vim -u NONE --noplugin'
alias gvi='gvim -u NONE -U NONE --noplugin'
alias vime='vim -c ":bufdo tab split"'

# Pipe
alias vimless='vim - -R -c "setl nolist | nnoremap <buffer> Q :<C-u>q<CR>"'
alias vimls='vim -c "read! ls" -c "nnoremap <buffer> Q :<C-u>q<CR> | setl nolist buftype=nofile | normal! Gddgg"'
function vimman () {
	vim -c "Man ${1}" +only
}

# Config
alias vimconfig='dotfile_config .vimrc'

# Plugin
alias vimshell='vim +VimShell'
alias vimconsole='vim +VimConsoleOpen'
alias adrone='vim +AdroneHome'

function vim-session () {
	vim -c "UniteSessionLoad ${1}"
}

function lingr () {
	vim -c "J6uil ${1}"
}

## Twitter
alias twitter='vim +TweetVimHomeTimeline'
alias tweet='vim +TweetVimSay'
alias twitter-public='vim +TwitterPublic'
alias tweet-public='vim +TweetPublic'

function twitter-usertimeline() {
	vim -c "TweetVimUserTimeline ${1}"
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

# Build
alias vim-build-configure-ubuntu='./configure --with-features=huge --enable-gui=gnome2 --enable-perlinterp --enable-rubyinterp --enable-luainterp --enable-fail-if-missing'
alias vim-build-make-mingw32='cd src && mingw32-make.exe -f Make_ming.mak GUI=yes IME=yes MBYTE=yes ICONV=yes DEBUG=no'

# Other
alias vim-record-startup='vim --startuptime vim_startup_time +q && vim -c "set bt=nofile ft=vim | r vim_startup_time | call system(\"rm vim_startup_time\") | normal! gg3dd"'
