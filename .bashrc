
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

# Opening Vim Short Cuts {{{

alias vimbash='vim ~/.bashrc && source ~/.bashrc && echo ">> .bashrc loaded"'
alias vimpro='vim ~/.bash_profile && source ~/.bash_profile && echo ">> .bash_profile loaded"'
alias vimconfig='vim ~/.vimrc'
alias vimpconfig='vim ~/.vimrc_private'
alias vimshconfig='vim ~/.vimshrc'
alias vimshdebug='vim -c VimConsoleOpen'
alias scrconfig='vim ~/.screenrc'
alias sshconfig='vim ~/.ssh/config'
alias sshknowns='vim ~/.ssh/known_hosts'

#}}}
# Vim Util Alias {{{

alias vi='vim -u NONE -U NONE --noplugin'
alias vimshell='vim -c VimShell'
alias conshell='vim -c "ConqueTerm bash"'
alias vimclearview='rm ~/.backup/vim_backup/view/*'
alias vimclearswp='rm ~/.backup/vim_backup/swp/*'
alias vimclearundo='rm ~/.backup/vim_backup/undo/*'
alias vimmemo='vim ~/.tmp/memo.txt'
alias gmail='vim -c Gmail'
alias twitter='vim -c TweetVimHomeTimeline'
alias tweet='vim -c TweetVimSay'
alias twitterPublic='vim -c "call TwitterPublicFunc()"'
alias tweetPublic='vim -c "call TweetPublicFunc()"'

# }}}
# Shell Util Alias {{{

alias gyazo='ruby ~/.vim/bundle/vim-gyazo/gyazo/gyazo.rb'

# MacOS Like command
alias pbcopy='xsel --clipboard --input'

# }}}
# Sub Alias (for refer) {{{

alias learn-vimscript='vim -c help learn-vimscript'
alias lingr-momonga='vim -c "J6uil momonga"'
alias lingr-cpp='vim -c "J6uil cpp"'
alias lingr-lpp='vim -c "J6uil LanguagesPlusPlus"'
alias lingr-vim='vim -c "J6uil vim"'
[ $isUbuntu -eq 1 ] && alias vncserver_kill="vncserver -kill hostname:${1}"  # Destop Number

if [ -n `which git` ] ; then
	alias git-myhelp='
		echo "New Branch:";
		echo "	git branch newBranchName commitId";
		echo "Branch List:";
		echo "	git branch -a";
		echo "Delete Branch:";
		echo "	git branch -d branchName";
		echo "Stash local and Stash List:";
		echo "	git stash save";
		echo "	git stash list";
		echo "Fixing recent local commit:";
		echo "	git commit --amend -m \"hoge\"";
		echo "RollBack previous commit:";
		echo "	git revert \"commit id\"";
		echo "";
		echo "Merge recent remote commit:";
		echo "	1. git pull";
		echo "		warn: would be overwritten...";
		echo "	2. git commit -am \"merging hoge file.\"";
		echo "	3. git mergetool";
		echo "";
		echo "No real file removal [git rm]:";
		echo "	git rm --cached file"'
fi
# }}}
# With OS {{{

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
	alias mysql='mysql --protocol=TCP'
fi

# }}}


#############################
#  Develop support aliases  #
#############################
#{{{

# The aliases shunted to other file
source ~/.bashrc_develop
alias vimdev='vim ~/.bashrc_develop && source ~/.bashrc && echo ">> .bashrc_develop loaded"'
alias autofiles='touch AUTHORS COPYING ChangeLog INSTALL NEWS README'

#}}}


#############################################
#                                           #
#############################################

# Export Loaded Archive
alias rc_loaded='echo "rc_loaded"'
