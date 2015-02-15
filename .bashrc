
###########################
#  Shell support aliases  #
###########################

# Counterolan of do not loading
if [ -z "`alias | grep pr_loaded`" ] ; then
	source ~/.bash_profile
fi

# Bourne Shell Configure
set -o ignoreeof
stty stop  undef  # unbind C-s that is stop viewing inputs to screen
stty start undef  # unbind C-q that is start viewing inputs to screen


# Bash Short Cuts
alias reload='source ~/.bash_profile && source ~/.bashrc && echo "bash source reloaded"'

# I'm a coward {{{

alias mv='mv -i'
alias cp='cp -i'

# }}}
# Vim Utils {{{

alias vi='vim -u NONE --noplugin'
alias gvi='gvim -u NONE -U NONE --noplugin'
alias vimshell='vim -c VimShell'
alias vimdebug='vim -c VimConsoleOpen'
alias vim-js="vim -c 'VimShellInteractive js'"
alias vim-record-startup="f=`mktemp` ; vim --startuptime vtime -- $f && rm $f"
alias vimclearview='rm ~/.backup/vim_backup/view/*'
alias vimclearswp='rm ~/.backup/vim_backup/swp/*'
alias vimclearundo='rm ~/.backup/vim_backup/undo/*'
alias vimclearcache='vimclearview ; vimclearundo ; vimclearswp'
alias vim-memo='vim ~/.tmp/memo.txt'
alias twitter='vim -c TweetVimHomeTimeline'
alias tweet='vim -c TweetVimSay'
alias twitter-public='vim -c TwitterPublic'
alias tweet-public='vim -c TweetPublic'
alias adrone='vim -c AdroneOpen'
alias themis='themis --runtimepath ".."'

alias vimconfig='vim ~/.vimrc'
alias gvimconfig='vim ~/.gvimrc'
alias gvim-vimconfig='gvim ~/.gvimrc'
alias vim-bashrc='vim ~/.bashrc && source ~/.bashrc && echo ">> .bashrc loaded"'
alias vim-bashpr='vim ~/.bash_profile && source ~/.bash_profile && echo ">> .bash_profile loaded"'

# with conditions {{{

# .bash_profile specified environment
if [ -f ~/.bash_profile_env ] ; then
	alias vim-bashpr-env='vim ~/.bash_profile_env && source ~/.bash_profile_env && echo ">> .bash_profile_env loaded"'
fi

# }}}

# }}}
# Shell Utils {{{

# Upload the pictures to gyazo
alias gyazo='ruby ~/.vim/bundle/vim-gyazo/gyazo/gyazo.rb'

# Console output pipe to clipboard
if [ $isUbuntu -eq 1 ] ; then
	alias pbcopy='xsel --clipboard --input'
elif [ $isCygwin -eq 1 ] ; then
	alias pbcopy='tee /dev/clipboard'
fi

# }}}
# Others {{{

alias ghci-hi='ghci 2>&1 | HsColour -tty'
alias mysql="mysql -E --pager='less -r -S -n -i -F -X'"
alias git-push-master='git push -u origin master'

if [ -n "`which git 2> /dev/null`" ] ; then
	alias git-myhelp='
		echo "New Branch:";
		echo "	git branch newBranchName commitId";
		echo ;
		echo "Branch List:";
		echo "	git branch -a";
		echo ;
		echo "Delete Branch:";
		echo "	git branch -d branchName";
		echo ;
		echo "Stash local and Stash List:";
		echo "	git stash save";
		echo "	git stash list";
		echo ;
		echo "Fixing recent local commit:";
		echo "	git commit --amend -m \"hoge\"";
		echo ;
		echo "RollBack previous commit:";
		echo "	git revert \"commit id\"";
		echo ;
		echo "Merge recent remote commit:";
		echo "	1. git pull";
		echo "		warn: would be overwritten...";
		echo "	2. git commit -am \"merging hoge file.\"";
		echo "	3. git mergetool";
		echo ;
		echo "No real file removal [git rm]:";
		echo "	git rm --cached file"'
fi


if [ -n "`which tmux 2> /dev/null`" ] ; then
	alias tmux-myhelp='
		echo "Window rename:";
		echo "	<Prefix> ,";
		echo ;
		echo "Start session with name:";
		echo "	tmux new -s name";
		echo ;
		echo "Start session with config file:";
		echo "	tmux -f file";'
fi

# }}}
# Environment Conditions {{{

if [ $isUbuntu -eq 1 ] ; then
	alias alternatives='update-alternatives'
	alias ssleep='dbus-send --print-reply --system --dest=org.freedesktop.UPower /org/freedesktop/UPower org.freedesktop.UPower.Suspend'
	alias pic_view='eog'
elif [ $isCygwin -eq 1 ] ; then
	alias cygrunsrv='cocot cygrunsrv'
	alias csc='cocot csc'
	alias ifconfig='cocot ipconfig'
	alias ping='cocot ping'
	alias traceroute='cocot tracert'
	alias route='cocot route'
	alias netstat='cocot netstat'
	alias updatedb='updatedb --localpaths="/bin /dev /etc /home /lib /usr /var /opt" --prunepaths="/usr/tmp /var/tmp"'
	alias mysql="mysql -E --pager='less -r -S -n -i -F -X' --protocol=TCP"
fi

# }}}


#############################
#  Develop support aliases  #
#############################
# develop environment {{{

# The aliases shunted to other file
if [ -f ~/.bashrc_develop ] ; then
	source ~/.bashrc_develop
	alias vim-bashrc-dev='vim ~/.bashrc_develop && source ~/.bashrc && echo ">> .bashrc_develop loaded"'
fi

# Generate items for autotools
alias autofiles='touch AUTHORS COPYING ChangeLog INSTALL NEWS README'

#}}}
# here aliases {{{

# Register directory path to file, and Easily cd there
if [ -f ~/.bashrc_places ] ; then
	source ~/.bashrc_places
fi

# Reload it
alias reload-places='source ~/.bashrc_places && echo "bash places reloaded"'

# View it
alias places="cat ~/.bashrc_places | awk -F'[= ]' '{print $ 2 \":\t\" $ 4}' | sed s/\'// | sort"

# Edit it
alias edit-places='vim ~/.bashrc_places && reload-places'

# Register it
function hereis () {
	place_name=$1
	alias_detail="${place_name}='cd `pwd`'"

	echo "alias ${alias_detail}" >> ~/.bashrc_places
	echo "here is '${1}'"

	reload-places
}

#}}}

#############################################
#                                           #
#############################################

# Export Loaded Archive
alias rc_loaded='echo "rc_loaded"'

