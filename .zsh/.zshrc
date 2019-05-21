#!/usr/bin/env zsh
#zmodload zsh/zprof && zprof

# The order of the loading
# 1. ~/.zsh/.zprofile
# 2. ~/.zsh/.zshrc
# 3. ~/.zshrc_env
# 4. ~/.zshrc_private

###################
# Check .zprofile #
###################
# The counterplan for if .zprofile never loaded
if [ -z "$(alias | grep zsh_pr_loaded)" ] ; then
    source $ZDOTDIR/.zprofile
fi

source ~/.sh_generic/vars.sh

# Load premised commands
source ~/.sh_generic/aliases.sh

# For each environment
case $(uname) in
Linux )
    source ~/.sh_generic/linux.sh
    ;;
Cygwin )
    source ~/.sh_generic/cygwin.sh
    HOME=/home/$USER
    PATH=$PATH:/cygdrive/c/Windows/system32:/cygdrive/c/Windows
    ;;
Darwin )
    source ~/.sh_generic/macOS.sh
esac

###########################################
# Configure zsh with the local conditions #
###########################################
# Configure options {{{

autoload -U colors       && colors       # Use color variables (Ex: $bg and $fg)
autoload -U compinit     && compinit -C  # Use zsh standard completion
autoload -U bashcompinit && bashcompinit # Use bash compatible completion

zstyle ':completion:*' menu select                        # Highlight selecting item in the menu
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # Case insensitive completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}     # list-colors uses $LS_COLORS's colors

# Set opts
setopt hist_ignore_dups
setopt extended_history
setopt hist_reduce_blanks
setopt share_history
setopt transient_rprompt
setopt ignore_eof
setopt auto_cd

# Don't use screen stopping
stty stop  undef
stty start undef

# Redraw $PROMPT with viins and vicmd
function zle-line-init zle-keymap-select {
    # This function maybe loaded at below procedures with ~/.zsh/zshrc/prompt.sh
    zshrc::prompt::main
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# }}}
# PROMPTs {{{

source ~/.zsh/zshrc/prompt.sh

# }}}


###################
# Define Commands #
###################
# {{{

alias history='fc -li 1'
alias hist=history
alias reload=". $ZDOTDIR/.zshrc && . $ZDOTDIR/.zprofile && echo '>> the zsh configrations are reloaded'"
alias rel=reload

# }}}


##################
# Manage Plugins #
##################
# Load zsh plugins {{{

fpath=(~/.zsh/plugin/zsh-completions/src $fpath)
source ~/.zsh/plugin/zsh-dircolors-solarized/zsh-dircolors-solarized.zsh
source ~/.zsh/plugin/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/plugin/enhancd/init.sh

for file in ~/.zsh/plugin/sh-hereis/{edit-places,hereis,place,reload-places,sh-hereis}.sh ; do
    source "$file"
done

#}}}
# zsh-dircolors-solarized {{{

# Use dircolors.ansi-light theme
setupsolarized dircolors.ansi-light

# }}}


# Load keymaps
source $ZDOTDIR/.zshrc.keymap

# Don't export
force-unexport TMUX

# If it exists, load environment config
if [ -f ~/.zshrc_env ] ; then
    source ~/.zshrc_env
fi

if [ -f ~/.zshrc_private ] ; then
    source ~/.zshrc_private
fi

# Export Loaded Archive
alias zsh_rc_loaded='echo "rc_loaded"'

#if (which zprof > /dev/null) ;then
#  zprof | less
#fi
