#!/usr/bin/env zsh

# To enable zprof, comment out this and a 'if' function of a bottom.
# zmodload zsh/zprof && zprof

# The order of loading
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
setopt no_beep

# Don't use screen stopping
stty stop  undef
stty start undef

# Redraw $PROMPT with viins and vicmd
function zle-line-init zle-keymap-select {
  # This function maybe loaded at below procedures with ~/.zsh/zshrc/prompt.sh
  zshrc::prompt::main

  if [[ "$VIM_TERMINAL" ]] ; then
    printf '\e]51;["call","Tapi_SyncTermCwd","%s"]\x07' "$PWD"
  fi

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
# Aliases {{{

alias history='fc -li 1'
alias hist=history
alias reload=". $ZDOTDIR/.zshrc && . $ZDOTDIR/.zprofile && echo '>> the zsh configrations are reloaded'"
alias rel=reload

# }}}
# General plugins {{{

# I don't load heavy plugins at here.
#   e.g. nvm, rbenv, etc.

# Please see ~/.sh_generic/aliases/functions/load-my-env.sh

# }}}


##################
# Manage Plugins #
##################
# Load zsh plugins {{{

fpath=(~/.zsh/plugin/zsh-completions/src $fpath)
source ~/.zsh/plugin/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/plugin/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/plugin/enhancd/init.sh

for file in ~/.zsh/plugin/sh-hereis/{edit-places,hereis,place,reload-places,sh-hereis}.sh ; do
  source "$file"
done

#}}}


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

# if (which zprof > /dev/null) ; then
#   zprof >! ~/.dotfiles/zprof.log
# fi
