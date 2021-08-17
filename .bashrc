#!/bin/bash

#######################
# Check .bash_profile #
#######################
# Load it if it is never loaded
if ! alias | grep -q pr_loaded ; then
  # shellcheck disable=SC1090
  source ~/.bash_profile
fi

# Options {{{

set -o ignoreeof  # Disable logoff by Ctrl + D
set -o vi         # Set vi style keymapping mode
stty stop  undef  # unbind C-s that is stop viewing inputs to screen
stty start undef  # unbind C-q that is start viewing inputs to screen

# }}}
# Keymappings {{{

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

# From https://qiita.com/comuttun/items/f54e755f22508a6c7d78
function peco-select-history () {
  declare l
  l=$( \
    HISTTIMEFORMAT=$( \
      history \
      | sort -k1,1nr \
      | perl -ne 'BEGIN { my @lines = (); } s/^\s*\d+\s*//; $in=$_; if (!(grep {$in eq $_} @lines)) { push(@lines, $in); print $in; }' \
      | peco --layout=bottom-up --initial-filter Regexp --select-1 --query "$READLINE_LINE" \
    ) \
  )
  READLINE_LINE="$l"
  READLINE_POINT=${#l}
}
# bind -x '"\C-r": peco-select-history'

# }}}
# Aliases {{{

alias ll='ls -l'
alias la='ls --all'

alias reload='source ~/.bashrc && source ~/.bash_profile && echo ">> bash prefs reloaded"'
alias rel=reload

# }}}

if [[ -f ~/.bashrc_env ]] ; then
  # shellcheck disable=SC1090
  source ~/.bashrc_env
fi

# Expose to mark .bashrc is loaded
alias rc_loaded='echo "rc_loaded"'
