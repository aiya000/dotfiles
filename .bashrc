####################
#  bash configure  #
####################
# Load scripts {{{

# Counterplan for didn't loading .bash_profile
if [ -z "`alias | grep pr_loaded`" ] ; then
	source ~/.bash_profile
fi

# Obey how to use git-completion.bash
if [ -f /usr/share/bash-completion/completions/git -a ! -f ~/.bash_completion_git ] ; then
	# Ubuntu
	cp /usr/share/bash-completion/completions/git ~/.bash_completion_git
elif [ -f /etc/bash_completion.d/git -a ! -f ~/.bash_completion_git ] ; then
	# Cygwin
	cp /etc/bash_completion.d/git ~/.bash_completion_git
fi

# Use git-completion
if [ -f ~/.bash_completion_git ] ; then
	source ~/.bash_completion_git
fi

# }}}
# Set options {{{

set -o ignoreeof  # Disable logoff by Ctrl + D
stty stop  undef  # unbind C-s that is stop viewing inputs to screen
stty start undef  # unbind C-q that is start viewing inputs to screen

# }}}


#############
#  aliases  #
#############
# Prepare function {{{

dotfile_config () {
	if [ -f "${HOME}/.dotfiles/${1}" ] ; then
		"$EDITOR" "${HOME}/.dotfiles/${1}"
	else
		"$EDITOR" "${HOME}/${1}"
	fi
}

# }}}
# Shell support {{{

# Bash Short Cuts
alias reload='source ~/.bash_profile && source ~/.bashrc && echo "bash source reloaded"'

# I'm a coward {{{

alias mv='mv -i'
alias cp='cp -i'

# }}}
# Vim Utils {{{

alias vi='vim -u NONE --noplugin'
alias gvi='gvim -u NONE -U NONE --noplugin'
alias vimless='vim - -R -c "setl nolist | nnoremap <buffer> Q :<C-u>q<CR>"'
alias vimshell='vim +VimShell'
alias vimconsole='vim +VimConsoleOpen'
alias vim-record-startup='vim --startuptime vim_startup_time +q && vim -c "set bt=nofile ft=vim | r vim_startup_time | call system(\"rm vim_startup_time\") | normal! gg3dd"'
alias vimclearview='rm ~/.backup/vim_backup/view/*'
alias vimclearswp='rm ~/.backup/vim_backup/swp/*'
alias vimclearundo='rm ~/.backup/vim_backup/undo/*'
alias vimclearcache='vimclearview ; vimclearundo ; vimclearswp'
alias twitter='vim +TweetVimHomeTimeline'
alias tweet='vim +TweetVimSay'
alias twitter-public='vim +TwitterPublic'
alias tweet-public='vim +TweetPublic'
alias adrone='vim +AdroneHome'

alias vimconfig='dotfile_config .vimrc'
alias gvimconfig='dotfile_config .gvimrc'
alias vimshconfig='dotfile_config .vimshrc'
alias vim-bashrc='dotfile_config .bashrc && [ -f ~/.bashrc ] && ( source ~/.bashrc && echo ">> .bashrc loaded" )'
alias vim-bashpr='dotfile_config .bash_profile && [ -f ~/.bashrc ] && ( source ~/.bash_profile && echo ">> .bash_profile loaded" )'

alias vim-build-configure-ubuntu='./configure --with-features=huge --enable-gui=gnome2 --enable-perlinterp --enable-rubyinterp --enable-luainterp --enable-fail-if-missing'
alias vim-build-make-mingw32='cd src && mingw32-make.exe -f Make_ming.mak GUI=yes IME=yes MBYTE=yes ICONV=yes DEBUG=no'

# with conditions {{{

# .bash_profile specified environment
if [ -f ~/.bash_profile_env ] ; then
	alias vim-bashpr-env='vim ~/.bash_profile_env && source ~/.bash_profile_env && echo ">> .bash_profile_env loaded"'
fi

# }}}

# }}}
# NeoVim Utils {{{

alias nvimconfig='nvim ~/.nvimrc'
alias nvim-bashrc='nvim ~/.bashrc && source ~/.bashrc && echo ">> .bashrc loaded"'
alias nvim-bashpr='nvim ~/.bash_profile && source ~/.bash_profile && echo ">> .bash_profile loaded"'

alias nvi='nvim -u NONE --noplugin'  # is not 'new vi', HaHaHa
alias nvim-terminal='nvim +terminal'
alias nvimconsole='nvim +VimConsoleOpen'
alias ntwitter='nvim +TweetVimHomeTimeline'
alias ntweet='nvim +TweetVimSay'
alias ntwitter-public='nvim +TwitterPublic'
alias ntweet-public='nvim +TweetPublic'

# }}}
# Shell Utils {{{

# Console output pipe to clipboard
if [ $IS_UBUNTU -eq 1 ] ; then
	alias pbcopy='xsel --clipboard --input'
elif [ $IS_CYGWIN -eq 1 ] ; then
	alias pbcopy='tee /dev/clipboard > /dev/null'
fi

# }}}
# Media Utils {{{

# Capture size optimized for Ubuntu Unity Desktop on My-bluemc PC
[ -n "`which byzanz-record 2> /dev/null`" ] && \
	alias byzanz-optimized-mini-record='byzanz-record --delay=0 -d 600 -x 55 -y 25 -w 720 -h 465'

# }}}
# Others {{{

alias mysql='mysql --pager="less -r -S -n -i -F -X"'
alias docker-rm-archives='sudo docker rm `sudo docker ps -a -q`'

# }}}
# Environment Conditions {{{

if [ $IS_UBUNTU -eq 1 ] ; then
	#NOTE: Ubuntu 15.04 removed this
	#alias ssleep='dbus-send --print-reply --system --dest=org.freedesktop.UPower /org/freedesktop/UPower org.freedesktop.UPower.Suspend'
	alias ssleep='sudo pm-suspend'
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
fi

# }}}

# }}}
# Development support {{{

# develop environment {{{

# The aliases shunted to other file
if [ -f ~/.bashrc_develop ] ; then
	source ~/.bashrc_develop
	alias vim-bashrc-dev='vim ~/.bashrc_develop && source ~/.bashrc && echo ">> .bashrc_develop loaded"'
fi

# Generate items for autotools
alias autofiles='touch AUTHORS COPYING ChangeLog INSTALL NEWS README'

# pull stackage's cabal.config
alias stackage-kurekure='wget https://www.stackage.org/lts/cabal.config'

#TODO: Test this
# <Warn> fully change git commit author and email
# git-fully-change-author-and-email {{{
function git-fully-change-author-and-email() {
	git_user_name=$1
	git_email=$2
	git filter-branch -f --env-filter \
		"GIT_AUTHOR_NAME='${git_user_name}'; GIT_AUTHOR_EMAIL='${git_email}'; GIT_COMMITTER_NAME='${git_user_name}'; GIT_COMMITTER_EMAIL='${git_email}';" \
		HEAD
	unset git_user_name git_email
}
# }}}

#}}}
# plugins {{{

# hereis {{{

# value for hereis plugin
export HEREIS_PLACES_FILE=~/.bashrc_places

# }}}

# loads {{{

plugin_dir=~/.bashfiles/plugin
local_plugins=( \
	hereis.sh \
	shell_kawaii.sh \
	ezoe_command_not_found_handle.sh \
)

for (( i = 0; i < ${#local_plugins[@]}; ++i )) ; do
	source "${plugin_dir}/${local_plugins[$i]}"
done

unset local_plugins plugin_dir

# }}}

#}}}

# }}}


# Export Loaded Archive
alias rc_loaded='echo "rc_loaded"'
