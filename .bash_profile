#----------------------------------------
# {- Hints -}
# @Bugs         => This hoge has the bugs.
# @Incomplete   => This is not completed making.
# @Unchecked    => This was not unchecked that is operate right
# @Unsupported  => Do not supported functions when now.
# @Unknown      => I don't know why this functioned.
# @Unused       => Not used this yet now, needs inquires deleting this.
# @Deprecated   => Deprecated This vimrc Version.
# @Experiment   => This is experimental implementation.
# @Marked       => I have eye on this.
# @See          => Referred URL, Saw Document, and etc...
# @Code         => A sample code using it
#-------------------
# Designating the target platform.
# @Hoge{Win,Ubuntu}  : This Hint for Win and Ubuntu.
# @Hoge!{Mac}        : This Hint for other than Mac.
#----------------------------------------


################
#  Parameters  #
################
#{{{

backIFS=$IFS ; IFS='EOF'

function find_name_by_uname() { #{{{
	uname=`uname -a`

	if [ -n "`echo $uname | grep ${1}`" ] ; then
		echo 1
	else
		echo 0
	fi
} #}}}

export IS_UBUNTU=`find_name_by_uname Ubuntu`
export IS_CYGWIN=`find_name_by_uname Cygwin`

IFS=$backIFS ; unset backIFS

#}}}


# Environment Variables
export HISTSIZE=10000
export HISTTIMEFORMAT='%Y/%m/%d %H:%M '
export HISTIGNORE='ls:jobs:history*:*hibernate:twitter*:tweet*:lingr*'
export HISTIGNORE="${HISTIGNORE}:*.bash_history*:*mount*-o*password=*"
export EDITOR=vim

# depends 'vim-pager' and 'vim-manpager'
export PAGER='vim - +PAGER -c "setl nonu nornu | setf vim-pager"'
export MANPAGER='vim - +MANPAGER -c "setl nonu nornu | setf vim-pager"'

# Plugin prefs
export HEREIS_ALIAS_PREFIX='p_'
export HEREIS_PLACES_FILE=~/.bashrc_places


###################
# ReConfig PATHes #
###################
# set GOROOT {{{

export GOPATH=~/.GoPath

# }}}
# make $PATH {{{
# set PATH with priority
new_path=$HOME/bin:$HOME/sbin
new_path=$new_path:$HOME/.dotfiles/bin

# stack
[ -d ~/.stack ] \
	&& new_path=$new_path:$HOME/.stack/programs/x86_64-linux/ghc-7.8.4/bin
# cabal
[ -d ~/.cabal ] \
	&& new_path=$new_path:$HOME/.cabal/bin \
	&& new_path=$new_path:./.cabal-sandbox/bin
# pkgsrc
[ -d ~/pkg ] \
	&& new_path=$new_path:$HOME/pkg/bin:$HOME/pkg/sbin
# rbenv
[ -d ~/.rbenv ] \
	&& new_path=$new_path:$HOME/.rbenv/bin \
	&& new_path=$new_path:$HOME/.rbenv/versions/`cat ~/.rbenv/version`/bin \
	&& eval "$($HOME/.rbenv/bin/rbenv init -)"  # Oh, my cygwin... why I cannot use shims...
# ruby-build
[ -d ~/.rbenv/plugins/ruby-build/bin ] \
	&& new_path=$new_path:$HOME/.rbenv/plugins/ruby-build/bin
# pyenv
[ -d ~/.pyenv ] \
	&& export PYENV_ROOT=~/.pyenv \
	&& new_path=$new_path:$PYENV_ROOT/bin \
	&& eval "$($HOME/.pyenv/bin/pyenv init -)"
# some binaries from some languages
[ -d ~/.local ] \
	&& new_path=$new_path:$HOME/.local/bin

# and basics
new_path=$new_path:/bin:/sbin
new_path=$new_path:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin
new_path=$new_path:/opt/bin:/opt/sbin:/opt/local/sbin:/opt/local/bin
new_path=$new_path:$GOPATH/bin

# With OS
if [ $IS_CYGWIN -eq 1 ] ; then
	export HOME=/home/$USER
	new_path=$new_path:/cygdrive/c/Windows/system32:/cygdrive/c/Windows
fi

# Apply to PATH
export PATH=$new_path:$PATH
unset new_path

#}}}


#############################################
#                                           #
#############################################

# Load Env Config
if [ -f ~/.bash_profile_env ] ; then
	# if you want that define plugin prefs in this file
	# see ~/.bashfiles/plugin/*.sh
	source ~/.bash_profile_env
fi

# Export Loaded Archive
alias pr_loaded='echo "pr_loaded"'

# Counterolan of do not loading
if [ -z "`alias | grep rc_loaded`" ] ; then
	source ~/.bashrc
fi
