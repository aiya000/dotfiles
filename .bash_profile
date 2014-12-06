

#############################################
#                                           #
#############################################


# Load Meta Config
if [ -f ~/.bash_meta_profile ] ; then
	source ~/.bash_meta_profile
else
	export shell_kawaii=0
fi


################
#  Parameters  #
################
#----------------------------------------
# {- Hints -}
# @Unsupported => 完全に動作するか未確定
# @Unchecked   => 未検査です
#----------------------------------------
#{{{
backIFS=$IFS ; IFS='EOF'

function isArgOS() { #{{{
	uname=`uname -a`
	if [ -n "`echo $uname | grep ${1}`" ] ; then
		echo 1
	else
		echo 0
	fi
} #}}}
export isLinux=`isArgOS Linux`
export isUbuntu=`isArgOS Ubuntu`
export isCygwin=`isArgOS Cygwin`

IFS=$backIFS ; unset backIFS
#}}}


# Environment Variables
#--- PS1 ---# {{{

function fakeUser() { #{{{
	username="$1"
	hostname="$2"
	[ -n "$hostname" ] \
		&& PS1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]${username}\[\e[32m\]@\[\e[33m\]${hostname}\[\e[32m\]/ \[\e[33m\]\w\[\e[0m\]$ " \
		|| PS1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]${username}\[\e[32m\]/ \[\e[33m\]\w\[\e[0m\]$ "
	unset username hostname
} #}}}

PS1='\[\e]0;\w\a\]\n\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]$ '
if [ $shell_kawaii -eq 1 ] ; then
	[ -n "$view_fake" ] && if [ $view_fake -eq 1 ] ; then
		[ -n "$view_host" ] && [ $view_host -eq 1 ] \
			&& PS1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]${fake_user_name}\[\e[32m\]@\[\e[33m\]${fake_host_name}\[\e[32m\]/ \[\e[33m\]\w\[\e[0m\]$ " \
			|| PS1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]${fake_user_name}\[\e[32m\]/ \[\e[33m\]\w\[\e[0m\]$ "
	else
		[ -n "$view_host" ] && [ $view_host -eq 1 ] \
			&& PS1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]\u\[\e[32m\]@\[\e[33m\]\h\[\e[32m\]/ \[\e[33m\]\w\[\e[0m\]$ " \
			|| PS1="\[\e]0;\w\a\]\n\[\e[32m\](*^-^)</\[\e[33m\]\u\[\e[32m\]/ \[\e[33m\]\w\[\e[0m\]$ "
	fi
fi

unset fake_user_name
# }}}
export PS1
export HISTSIZE=10000
export HISTTIMEFORMAT='%Y/%m/%d %H:%M '
export HISTIGNORE='ls:jobs:history*:*hibernate:twitter:tweet'
export HISTIGNORE="${HISTIGNORE}:*.bash_history*:*mount*-o*password=*"
export EDITOR=vim


###################
# ReConfig PATHes #
###################
new_path=/bin:/sbin
new_path=$new_path:$HOME/bin:$HOME/sbin
new_path=$new_path:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin
new_path=$new_path:/opt/bin:/opt/sbin:/opt/local/sbin:/opt/local/bin
# For Cabal
[ -d ~/.cabal ] \
	&& new_path=$new_path:$HOME/.cabal/bin
# For pkgsrc
[ -d ~/pkg ] \
	&& new_path=$new_path:$HOME/pkg/bin:$HOME/pkg/sbin

# With OS
if [ $isUbuntu -eq 1 ] ; then
	# Fix Recently Build GCC $ @Unchecked
	[ -n "`which g++4.9`"] \
		&& export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:/usr/include/c++/4.9:/usr/include/c++/4.9/x86_64-linux-gnu \
		&& export CPLUS_LIBRARY_PATH=$CPLUS_LIBRARY_PATH:/usr/lib/lib64
elif [ $isCygwin -eq 1 ] ; then
	export HOME=/home/$USER
	new_path=$new_path:/opt/local/bin/exec
	new_path=$new_path:/opt/local/ghc/bin
	new_path=$new_path:/opt/local/bin/java_wrapper

	# Auto detect JDK PATH
	if [ -d /cygdrive/c/Program\ Files\ \(x86\)/ ] ; then
		jPath=/cygdrive/c/Program\ Files\ \(x86\)/Java/
		jdkPath=`ls "$jPath" | grep 'jdk' | sort -r | head -1`
		new_path=$new_path:"$jPath/$jdkPath/bin"

		new_path=$new_path:/cygdrive/c/Program\ Files/pleiades/java/6/bin/
	else
		jPath=/cygdrive/c/Program\ Files/Java/
		jdkPath=`ls "$jPath" | grep 'jdk' | sort -r | head -1`
		new_path=$new_path:"$jPath/$jdkPath/bin"

		new_path=$new_path:/cygdrive/c/Program\ Files\ \(x86\)/pleiades/java/6/bin/
	fi
	export JAVA_HOME=/opt/local/share/jdk_home

	new_path=$new_path:/cygdrive/c/Windows/system32:/cygdrive/c/Windows
fi

# YukiTask
new_path=$new_path:$HOME/bin/yukitask
source ~/bin/yukitask/command_aliases
source ~/bin/yukitask/here_aliases

# Apply to PATH
export PATH=$new_path:$PATH
unset new_path


#############################################
#                                           #
#############################################

# Export Loaded Archive
alias pr_loaded='echo "pr_loaded"'

# Counterolan of do not loading
if [ -z "`alias | grep rc_loaded`" ] ; then
	source ~/.bashrc
	export HISTIGNORE="${HISTIGNORE}:twitter*:tweet*:lingr*:"
fi

