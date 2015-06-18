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
alias vimshell='vim +VimShell'
alias vimconsole='vim +VimConsoleOpen'
alias vim-record-startup="f=`mktemp` ; vim --startuptime vim_startup_time -- $f && rm $f"
alias vimclearview='rm ~/.backup/vim_backup/view/*'
alias vimclearswp='rm ~/.backup/vim_backup/swp/*'
alias vimclearundo='rm ~/.backup/vim_backup/undo/*'
alias vimclearcache='vimclearview ; vimclearundo ; vimclearswp'
alias twitter='vim +TweetVimHomeTimeline'
alias tweet='vim +TweetVimSay'
alias twitter-public='vim +TwitterPublic'
alias tweet-public='vim +TweetPublic'

alias vimconfig='vim ~/.vimrc'
alias gvimconfig='vim ~/.gvimrc'
alias vimshconfig='vim ~/.vimshrc'
alias vim-bashrc='vim ~/.bashrc && source ~/.bashrc && echo ">> .bashrc loaded"'
alias vim-bashpr='vim ~/.bash_profile && source ~/.bash_profile && echo ">> .bash_profile loaded"'

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

alias nvi='nvim -u NONE --noplugin'  # (not a original nvi, hahaha)
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

alias mysql='mysql -E --pager="less -r -S -n -i -F -X"'
alias docker-rm-archives='sudo docker rm `sudo docker ps -a -q`'

# }}}
# Environment Conditions {{{

if [ $IS_UBUNTU -eq 1 ] ; then
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

#############################################
#                                           #
#############################################

# Export Loaded Archive
alias rc_loaded='echo "rc_loaded"'
