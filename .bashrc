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
# Set options {{{

set -o ignoreeof  # Disable logoff by Ctrl + D
set -o vi         # Set vi style keymapping mode
stty stop  undef  # unbind C-s that is stop viewing inputs to screen
stty start undef  # unbind C-q that is start viewing inputs to screen

# }}}
# Key mappings {{{

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


#############
#  aliases  #
#############
# Load general aliases {{{

source ~/.sh_generic/aliases.sh

# }}}
# Define specified aliases for bash {{{

# Bash Short Cuts
alias reload='. ~/.bashrc && echo ">> .bashrc reloaded"'

# }}}


###########
# plugins #
###########
# Load {{{

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


# .bash_profile specified environment
if [ -f ~/.bash_profile_env ] ; then
	alias vim-bashpr-env='vim ~/.bash_profile_env && source ~/.bash_profile_env && echo ">> .bash_profile_env loaded"'
fi

# Export Loaded Archive
alias rc_loaded='echo "rc_loaded"'
