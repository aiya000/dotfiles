#!/usr/bin/env zsh

##########################
# Config the environment #
##########################
# Set the zsh variables {{{

export ZDOTDIR=~/.zsh
export RPROMPT="${${KEYMAP/vicmd/$vi_normal}/(main|viins)/$vi_insert}"
export HISTFILE=$ZDOTDIR/.zsh_history
export HISTSIZE=1000000
export SAVEHIST=1000000
export HISTTIMEFORMAT='%Y/%m/%d %H:%M '
export HISTIGNORE="*.zsh_history*:*mount*-o*password=*"

# Set $RPROMPT
vi_normal="%{$bg[red]%}[NORMAL]%{$reset_color%}"
vi_insert="%{$bg[blue]%}[INSERT]%{$reset_color%}"

# }}}
# Reset $PATH {{{

# Mine
PATH=$HOME/bin:$HOME/sbin
PATH=$PATH:$HOME/.dotfiles/bin
PATH=$PATH:$HOME/.dotfiles/aacceessoorryy/bin

# Basics
PATH=$PATH:/bin:/sbin
PATH=$PATH:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin
PATH=$PATH:/opt/bin:/opt/sbin:/opt/local/sbin:/opt/local/bin
PATH=$PATH:$GOPATH/bin

# zsh completions
PATH=$PATH:$ZDOTDIR/zsh_completions

#}}}
# Others {{{

# Favorite editor
export EDITOR=nvim
export VISUAL=nvim

# Using 'vim-pager' and 'vim-manpager'
export PAGER='nvim - +PAGER -c "setl nonu nornu ft=vim-pager | only"'
export MANPAGER='nvim - +MANPAGER -c "setl nonu nornu ft=vim-pager | only"'

# Lice copylight user
export LICE_USER=aiya000

# Languages
export GOPATH=~/.GoPath

# Inject to fzf in .sh_generic/*
export FZF_CASUAL_OPTIONS='--tac --cycle --bind=ctrl-j:accept,ctrl-k:kill-line'

# }}}


##################
# Manage Plugins #
##################
# Configurate the plugin variables {{{

# zplug
export ZPLUG_HOME=$ZDOTDIR/zplug

# sh-hereis
export HEREIS_ALIAS_PREFIX='p_'

# zsh-shell-kawaii
export SHELL_KAWAII_HER_VISIBILITY=1
export SHELL_KAWAII_HOST_VISIBILITY=1
export SHELL_KAWAII_FAKE_USERNAME='☆ミ'    # Dancing stars
export SHELL_KAWAII_FAKE_HOSTNAME='aiya000' # ミ☆   on me!
export SHELL_KAWAII_CLI_TO_LOW=1
export SHELL_KAWAII_USER_DELIMITER='>>>'
export SHELL_KAWAII_MORE_PROMPT_COMMAND=echo_statuses  # echo_statuses will be defined by .zshrc
export SHELL_KAWAII_AUTO_SET_ZLE_WIDGETS=0

# auto-fu.zsh
export AUTO_FU_NOCP=1

# }}}


###########################
# Mark the end of loading #
###########################
alias zsh_pr_loaded='echo "pr_loaded"'
