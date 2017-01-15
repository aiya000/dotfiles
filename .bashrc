#!/bin/bash
source ~/.sh_generic/helper.sh

###################
# Check .zprofile #
###################
# The counterplan for if .bash_profile never loaded
if [ -z "`alias | grep pr_loaded`" ] ; then
	source ~/.bash_profile
fi

############################################
# Configure bash with the local conditions #
############################################
# Define global variables {{{

in_linux=$(is_your_os_name Linux)
in_cygwin=$(is_your_os_name Cygwin)

# }}}
# Set the bash options {{{

set -o ignoreeof  # Disable logoff by Ctrl + D
set -o vi         # Set vi style keymapping mode
stty stop  undef  # unbind C-s that is stop viewing inputs to screen
stty start undef  # unbind C-q that is start viewing inputs to screen

# }}}
# Set the bash key-mappings {{{

# Vim nize
bind -m vi-command '"_": beginning-of-line'
bind -m vi-insert  '"\C-\\\C-n": "\e"'

# Emacs nize
bind -m vi-insert  '"\C-n": next-history'
bind -m vi-insert  '"\C-p": previous-history'
bind -m vi-insert  '"\C-a": beginning-of-line'
bind -m vi-insert  '"\C-e": end-of-line'
bind -m vi-insert  '"\C-b": backward-char'
bind -m vi-insert  '"\C-f": forward-char'
bind -m vi-insert  '"\C-k": kill-line'
bind -m vi-insert  '"\C-d": delete-char'

# My taste
bind -m vi-insert  '"\C-l": "\e"'
bind -m vi-insert  '"\C-]": clear-screen'
bind -m vi-command -x '"\C-k\C-r": . ~/.bashrc && echo ">> bash source reloaded"'

# }}}
# Set language tools {{{

# stack
[ -d ~/.stack ] \
	&& PATH=$PATH:$HOME/.stack/programs/x86_64-linux/ghc-7.8.4/bin

# cabal
[ -d ~/.cabal ] \
	&& PATH=$PATH:$HOME/.cabal/bin \
	&& PATH=$PATH:./.cabal-sandbox/bin

# pkgsrc
[ -d ~/pkg ] \
	&& PATH=$PATH:$HOME/pkg/bin:$HOME/pkg/sbin

# rbenv
[ -d ~/.rbenv ] \
	&& PATH=$PATH:$HOME/.rbenv/bin \
	&& PATH=$PATH:$HOME/.rbenv/versions/`cat ~/.rbenv/version`/bin \
	&& eval "$($HOME/.rbenv/bin/rbenv init -)"

# ruby-build
[ -d ~/.rbenv/plugins/ruby-build/bin ] \
	&& PATH=$PATH:$HOME/.rbenv/plugins/ruby-build/bin

# virtualenv with virtualenvwrapper
[ -n $(which virtualenvwrapper.sh) ] \
	&& export WORKON_HOME=$HOME/.virtualenvs \
	&& source $(which virtualenvwrapper.sh)

# anything
[ -d ~/.local ] \
	&& PATH=$PATH:$HOME/.local/bin


# }}}
# Load the scripts of some software {{{

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

# Use travis-completion
if [ -f ~/.travis/travis.sh ] ; then
	source ~/.travis/travis.sh
fi

# Use fzf-completion
#if [ -f /usr/share/fzf/key-bindings.bash ] ; then
#	source /usr/share/fzf/key-bindings.bash
#fi

# Use stack-compleetion
if [ -s "`which stack`" ] ; then
	eval "$(stack --bash-completion-script stack)"
fi

# }}}


###################
# Define Commands #
###################
# Load general aliases {{{

source ~/.sh_generic/aliases.sh

# }}}
# Override default {{{

alias vim=vime
alias nvim=nvime

# }}}
# Define specified aliases for bash {{{

# Bash Short Cuts
alias reload='. ~/.bashrc && . ~/.bash_profile && echo ">> the bash configurations are reloaded"'

# }}}


##################
# Manage Plugins #
##################
# Load the local plugins {{{

plugin_dir=~/.bashfiles/plugin
local_plugins=( \
	hereis.sh \
	shell_kawaii.sh \
	ezoe_command_not_found_handle.sh \
	tovim.sh \
)

for (( i = 0; i < ${#local_plugins[@]}; ++i )) ; do
	source "${plugin_dir}/${local_plugins[$i]}"
done

unset local_plugins plugin_dir

# }}}


# For each environment
if [ $in_linux -eq 1 ] ; then
	source ~/.sh_generic/linux.sh
elif [ $in_cygwin -eq 1 ] ; then
	source ~/.sh_generic/cygwin.sh
	HOME=/home/$USER
	PATH=$PATH:/cygdrive/c/Windows/system32:/cygdrive/c/Windows
fi

# If it exists, load environment config
if [ -f ~/.bashrc_env ] ; then
	source ~/.bashrc_env
fi

# Export Loaded Archive
alias rc_loaded='echo "rc_loaded"'
