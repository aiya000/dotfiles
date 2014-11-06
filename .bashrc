
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

# Vim Utils {{{

alias vi='vim -u NONE --noplugin'
alias gvi='gvim -u NONE -U NONE --noplugin'
alias vimshell='vim -c VimShell'
alias conshell='vim -c "ConqueTerm bash"'
alias vim-record-startup="f=`mktemp` ; vim --startuptime vtime -- $f && rm $f"
alias vimclearview='rm ~/.backup/vim_backup/view/* > /dev/null 2>&1'
alias vimclearswp='rm ~/.backup/vim_backup/swp/* > /dev/null 2>&1'
alias vimclearundo='rm ~/.backup/vim_backup/undo/* > /dev/null 2>&1'
alias vimclearcache='vimclearview && vimclearundo && vimclearswp > /dev/null 2>&1'
alias vim-memo='vim ~/.tmp/memo.txt'
alias twitter='vim -c TweetVimHomeTimeline'
alias tweet='vim -c TweetVimSay'
alias twitter-public='vim -c TwitterPublic'
alias tweet-public='vim -c TweetPublic'

alias vimconfig='vim ~/.vimrc'
alias vim-bash='vim ~/.bashrc && source ~/.bashrc && echo ">> .bashrc loaded"'
alias vim-bashprofile='vim ~/.bash_profile && source ~/.bash_profile && echo ">> .bash_profile loaded"'

# }}}
# Shell Utils {{{

# Tuide(Benri)
alias gyazo='ruby ~/.vim/bundle/vim-gyazo/gyazo/gyazo.rb'

# MacOS Like command
alias pbcopy='xsel --clipboard --input'

# }}}
# Others {{{

alias mysql="mysql -E --pager='less -S -n -i -F -X'"

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
	alias mysql="mysql -E --pager='less -S -n -i -F -X' --protocol=TCP"
fi

# }}}


#############################
#  Develop support aliases  #
#############################
#{{{

# The aliases shunted to other file
if [ -f ~/.bashrc_develop ] ; then
	source ~/.bashrc_develop
	alias vimdev='vim ~/.bashrc_develop && source ~/.bashrc && echo ">> .bashrc_develop loaded"'
fi
alias autofiles='touch AUTHORS COPYING ChangeLog INSTALL NEWS README'

#}}}


#############################################
#                                           #
#############################################

# Export Loaded Archive
alias rc_loaded='echo "rc_loaded"'

