#!/bin/bash

# shellcheck disable=SC1090
source ~/.dotfiles/bash-toys/source-all.sh

#
# This file define the aliases and functions
# You can load this from .zshrc
# and you can load .bashrc or another shell rc file, maybe
#

# Common Utifity Functions {{{

function eval-with-echo () {
  echo "$*"
  eval "$*"
}

# }}}
# Override existed names {{{

alias cp='cp -i'
alias dd='dd status=progress'
alias df='df -h'
alias dmesg='dmesg --ctime'
alias free='free -h'
alias ls='ls --color=auto --group-directories-first'
alias mv='mv -i'
alias sl=ls
alias rm=rm-dust  # Aaaaaaaaaaaaaaaa!!

if i-have batcat ; then
  alias batcat-with-default-options='batcat $DOTFILES_BATCAT_DEFAULT_OPTIONS' # あれ？ これってオプション変数、ここで展開しなくていいんだっけ？
  alias bat=batcat-with-default-options
  alias cat=batcat-with-default-options
elif i-have bat ; then
  alias bat-with-default-options='bat $DOTFILES_BATCAT_DEFAULT_OPTIONS'
  alias cat=bat-with-default-options
fi

i-have btop && alias top=btop

alias-of rg 'rg --hidden'

function aliases::define_fd () {
  local fd

  if i-have fdfind ; then
    fd=fdfind
  elif i-have fd ; then
    fd=fd
  fi

  if [[ $fd != '' ]] ; then
    # shellcheck disable=SC2139
    alias fd="$fd --hidden --ignore-case --no-ignore" # --hidden to include '.' prefixed files
  fi
}
aliases::define_fd

if i-have dust ; then
  alias du=dust
else
  alias du='du -h'
  alias du-sum='du -hs'
  function du-sort () {
    local paths=${1:-.}
    du -h -d 1 "$paths" | sort -h
  }
fi

# NOTE: Who did define the original - ?
function - () {
  # shellcheck disable=SC2164
  cd -
}

alias-of sudo 'sudo '  # Enable aliases on sudo
alias-of mysql 'mysql --pager="less -r -S -n -i -F -X"'
alias-of yay 'yay --color always'

# }}}
# Load ./aliases/** {{{

source ~/.shells/aliases/build-tools.sh
source ~/.shells/aliases/os-package-managers.sh
source ~/.shells/aliases/git.sh
source ~/.shells/aliases/neovim.sh

# }}}
# AI {{{

if i-have claude ; then
  alias c=claude
  alias claude-resume='claude --resume'
  alias claude-continue='claude --continue'
  alias claude-commit='claude "/git-commit"'
  alias git-commit-claude=claude-commit
fi

if i-have copilot ; then
  alias copilot='copilot --allow-tool write --allow-tool "shell(notify)" --allow-tool "shell(git log)" --allow-tool "shell(git show)" --allow-tool "shell(git diff)" --allow-tool "shell(git status)" --allow-tool "shell(git reflog)"'
  alias copilot-commit='copilot -p "~/.claude/commands/git-commit.md を読んで、git commitをして。" --allow-tool "shell(git:*)" --deny-tool "shell(git push)" --deny-tool "shell(git add)'
  alias git-commit-copilot=copilot-commit
fi

# }}}
# For programming languages eco systems  {{{

# Load the specified version of Node.js or the latest version
# Please also see ~/.zshrc_env
function dotfiles::find_nodejs_to_load () {
  if \
    [[ -n $DOTFILES_ZSHRC_NVM_NODE_VERSION ]] \
    && ls "$NVM_DIR/versions/node/$DOTFILES_ZSHRC_NVM_NODE_VERSION" > /dev/null 2>&1
  then
    # If the specified version node.js is found
    node_version=$DOTFILES_ZSHRC_NVM_NODE_VERSION
  elif [[ -n $DOTFILES_ZSHRC_NVM_NODE_VERSION ]] ; then
    # If the specified version node.js is not found
    # TODO: Don't disable shellcheck
    # shellcheck disable=SC2012
    node_version=$(ls "$NVM_DIR/versions/node" | sort | tail -1)
    echo "Node.js version $DOTFILES_ZSHRC_NVM_NODE_VERSION not found. Using $node_version instead."
  fi

  if [[ -n $node_version ]] ; then
    nvm use "$node_version"
  else
    echo 'No Node.js versions found.'
    return 1
  fi
}

i-have luarocks && alias luarocks-local-5.1='luarocks --local --lua-version=5.1'

# }}}
# Archive file utilities {{{

function zip-create () {
  : '
  Creates a .zip archive.
  - Single argument:    zip-create <dir>        → <dir>.zip
  - Multiple arguments: zip-create <output> ... → runs zip -r as-is
  '

  if [[ $# -eq 1 ]] ; then
    zip -r "${1%/}.zip" "$1"
  else
    zip -r "$@"
  fi
}
alias zip-extract='unzip'
alias zip-show-files='unzip -l'

function tar-xz-create () {
  : '
  Creates a .tar.xz archive.
  - Single argument:    tar-xz-create <dir>        → <dir>.tar.xz
  - Multiple arguments: tar-xz-create <output> ... → runs tar -cvJf as-is
  '

  if [[ $# -eq 1 ]] ; then
    tar -cvJf "${1%/}.tar.xz" "$1"
  else
    tar -cvJf "$@"
  fi
}
alias tar-xz-extract='tar -xvJf'
alias tar-xz-show-files='tar -tvJf'

function tar-gz-create () {
  : '
  Creates a .tar.gz archive.
  - Single argument:    tar-gz-create <dir>        → <dir>.tar.gz
  - Multiple arguments: tar-gz-create <output> ... → runs tar -cvzf as-is
  '

  if [[ $# -eq 1 ]] ; then
    tar -cvzf "${1%/}.tar.gz" "$1"
  else
    tar -cvzf "$@"
  fi
}
alias tar-gz-extract='tar -xvzf'
alias tar-gz-show-files='tar -tvzf'

function tar-bz2-create () {
  : '
  Creates a .tar.bz2 archive.
  - Single argument:    tar-bz2-create <dir>        → <dir>.tar.bz2
  - Multiple arguments: tar-bz2-create <output> ... → runs tar -cvjf as-is
  '

  if [[ $# -eq 1 ]] ; then
    tar -cvjf "${1%/}.tar.bz2" "$1"
  else
    tar -cvjf "$@"
  fi
}
alias tar-bz2-extract='tar -xvjf'
alias tar-bz2-show-files='tar -tvjf'

i-have unzip && alias unzip-cp932='unzip -O cp932'


# }}}
# Others {{{

alias date-simple='date "+%Y-%m-%d %H:%M"'
alias date-today='date +"%Y-%m-%d"'
alias today=date-today
alias date-tomorrow='date -v+1d +"%Y-%m-%d"'
alias tomorrow=date-tomorrow
alias date-now='date +%H:%M'
alias now=date-now
alias date-1min-after='date -v+1M +%H:%M'

# shellcheck disable=SC2139
alias mount-ntfs="sudo mount -o user=$(whoami),uid=$(id -u),gid=$(id -g),iocharset=utf8"
alias mount-vfat=mount-ntfs
alias mount-ext2='sudo mount -o iocharset=utf8'
alias mount-ext3=mount-ext2
alias mount-ext4=mount-ext2

function mount-smb2 () {
  local ip=$1
  local user=$2
  local password=$3
  local directory=$4
  local mount_point=$5
  mount_smbfs "//$user:$password@$ip/$directory" "$5"
}

if i-have jq ; then
  function url-encode () {
    echo "\"$1\"" | jq -r @uri
  }
  alias urlencode=url-encode
fi

if i-have luap ; then
  alias lua-repl=luap
elif i-have lua ; then
  alias lua-repl=lua
fi

alias notify-at-cancel-all='notify-at -l ; notify-at -l | drop 2 | cut -d" " -f1 | xargs -I {} notify-at -c {} ; notify-at -l'

# }}}
# Other shourthands {{{

# shellcheck disable=SC2139
alias e="$EDITOR"

alias la='ls -a --color=auto --group-directories-first'
alias ll='ls -l --color=auto --group-directories-first'
alias llh='ls -lh --color=auto --group-directories-first'
alias lla='ls -la --color=auto --group-directories-first'

alias ei=exit
alias t=vterminal
alias ki=kill-list

# alias ..='cd ../'
# alias ...='cd ../../'
# alias ....='cd ../../../'
# ...
function aliases::define_cd_to_parents () {
  local name dir
  for (( i = 2; i <= 10; ++i )) ; do
    name=$(eval "printf '.%.0s' {1..$i}")
    dir=$(eval "printf '../%.0s' {2..$i}")
    eval "alias $name='cd $dir'"
  done
}
aliases::define_cd_to_parents

i-have tmux && alias ta='tmux attach'
i-have krita && alias kra=krita

alias cdn=cd-to-node-root

# }}}

export PATH=$PATH:$HOME/.sh_generic/bin

# vim:foldmethod=marker
