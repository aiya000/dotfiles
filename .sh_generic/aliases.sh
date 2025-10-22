#!/bin/bash

# shellcheck disable=SC1090
source ~/.dotfiles/bash-toys/source-all.sh

#
# This file define the aliases and functions
# You can load this from .zshrc
# and you can load .bashrc or another shell rc file, maybe
#

# Override existed name {{{

alias cp='cp -i'
alias dd='dd status=progress'
alias df='df -h'
alias dmesg='dmesg --ctime'
alias free='free -h'
alias ls='ls --color=auto --group-directories-first'
alias mv='mv -i'
alias sl=ls
alias rm=rm-dust  # Aaaaaaaaaaaaaaaa!!
alias du='du -h'

if i_have batcat ; then
  alias batcat-with-default-options='batcat $DOTFILES_BATCAT_DEFAULT_OPTIONS'
  alias bat=batcat-with-default-options
  alias cat=batcat-with-default-options
fi

i_have btop && alias top=btop

# NOTE: Who did define the original - ?
function - () {
  # shellcheck disable=SC2164
  cd -
}

alias_of sudo 'sudo '  # Enable aliases on sudo
alias_of mysql 'mysql --pager="less -r -S -n -i -F -X"'
alias_of rg 'rg --color always --hidden'

# }}}
# Common functions {{{

# TODO: ` `がデリミタに指定されたときに、`  `や`   `も、統一的にデリミタにしたい。これはこのインターフェースと相違するので、slice-spacesコマンドというのを作ってもいいかも
# TODO: 完成したらbash-toysに移動する
function slice () {
  local delimiter=$1 field_from=$2 field_to=$3
  while read -r line ; do
    echo "$line" | cut -d "$delimiter" -f "$field_from-$field_to"
  done
}

# }}}
# Load ./aliases/** {{{

source ~/.sh_generic/aliases/build-tools.sh
source ~/.sh_generic/aliases/os-package-managers.sh
source ~/.sh_generic/aliases/neovim.sh

# }}}
# General Commands {{{

alias la='ls -a --color=auto --group-directories-first'
alias ll='ls -l --color=auto --group-directories-first'
alias llh='ls -lh --color=auto --group-directories-first'
alias lla='ls -la --color=auto --group-directories-first'

alias date-simple='date "+%Y-%m-%d %H:%M"'
alias date-today='date +"%Y-%m-%d"'
alias today=date-today

alias du-sum='du -hs'

function du-sort () {
  local paths=${1:-.}
  du -h -d 1 "$paths" | sort -h
}

# shellcheck disable=SC2139
alias mount4u.ntfs="sudo mount -o user=$(whoami),uid=$(id -u),gid=$(id -g),iocharset=utf8"
alias mount4u.vfat=mount4u.ntfs
alias mount4u.ext2='sudo mount -o iocharset=utf8'
alias mount4u.ext3=mount4u.ext2
alias mount4u.ext4=mount4u.ext2

alias ei=exit
alias t=vterminal
alias cdp=cd-finddir
alias ki=killing-art
i_have fdfind && alias fd='fdfind --hidden --ignore-case' # --hidden to include '.' prefixed files

function ff () {
  : Find a file by taken fuzzy name.
  f . "$1"
}

function f () {
  : Find a file on a directory by taken fuzzy name.
  local base_path=$1 fuzzy_name=$2

  if i_have fdfind ; then
    fdfind "$fuzzy_name" "$base_path"
  else
    find "$base_path" -name "*$fuzzy_name*"
  fi
}

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

i_have tmux && alias ta='tmux attach'
i_have nmcli && alias nmcli-connect-wifi='nmcli device wifi connect'
i_have unzip && alias unzip-cp932='unzip -O cp932'

if i_have notifu.exe && ! i_have notify-send ; then
  function notify-send () {
    notifu.exe /p WSL /m "$1" &> /dev/null
  }
fi

if i_have soundvolumeview ; then
  function soundvolumeview-list () {
    local tmpfile="/tmp/$RANDOM"
    soundvolumeview /stext "$tmpfile"
    cat "$tmpfile" | rg '^Device Name' | sort | uniq
    rm "$tmpfile" > /dev/null
  }

  function soundvolumeview-set-default () {
    : Exected: 'output' or 'input'
    : TODO: $'Currently, if the device of $device_name exists both as output and input, this function sets it to both as output and input. This maybe SoundVolumeView.exe\'s spec.'
    local device_type=$1
    local device_name=${2:-$(soundvolumeview-list | peco | awk '{print $3}')}

    : 0   - 'Console'
    : 1   - 'Multimedia (Speaker)'
    : 2   - 'Communications (Mic)'
    : all - 'Set all default types (Console, Multimedia, and Communications)'
    :
    : See https://www.nirsoft.net/utils/sound_volume_view.html
    case $device_type in
      output) local device_type_number=1 ;;
      input) local device_type_number=2 ;;
      *)
        echo "Unknown device type: $device_type"
        return 1
        ;;
    esac

    soundvolumeview /SetDefault "$device_name" "$device_type_number"
  }

  function soundvolumeview-get-default () {
    : Exected: 'output' or 'input'
    local device_type=$1
    case $device_type in
      output) local device_type_name='Render' ;;
      input) local device_type_name='Capture' ;;
      *)
        echo "Unknown device type: $device_type"
        return 1
        ;;
    esac

    local tmpfile="/tmp/$RANDOM"
    soundvolumeview /sjson "$tmpfile"
    iconv -f UTF-16LE -t UTF-8 "$tmpfile" | jq ".[] | select(.Default == \"$device_type_name\")" | jq '.["Device Name"]' -r
    rm "$tmpfile" > /dev/null
  }
fi

# }}}
# Editors {{{

# shellcheck disable=SC2139
alias e="$EDITOR"
alias g=gvim

# VS Code
if ! i_have code && i_have Code.exe ; then
  alias code=Code.exe
fi

# }}}
# Git {{{

if i_have git ; then
  git_taking_limit=100

  alias _gr='git reset'
  alias _grh='git reset --hard'
  alias _grh~='git reset --hard HEAD~'
  alias _grh~~='git reset --hard HEAD~~'
  alias _grh~~~='git reset --hard HEAD~~~'
  alias _grh~~~~='git reset --hard HEAD~~~~'
  alias _grs='git reset --soft'
  alias _grs~='git reset --soft HEAD~'
  alias _grs~~='git reset --soft HEAD~~'
  alias _grs~~~='git reset --soft HEAD~~~'
  alias _grs~~~~='git reset --soft HEAD~~~~'
  alias gs='git status'
  alias ga='git add'
  alias gaa='git add -A'
  alias gap='git add -p'
  alias gb='git branch'
  alias gba='git branch --all'
  alias gbd='git branch --delete'
  alias _gbd='git branch -D'
  alias _gbdf='git branch --delete --force'
  alias gbm='git branch -m'
  alias gbc='git branch --show-current'
  alias gc='git commit --verbose'
  alias gcam='git commit --verbose --amend'
  alias gcamm='git commit --verbose --amend -m'
  alias gcm='git commit -m'
  alias gcem='git commit --allow-empty -m'
  alias gcf='git commit --fixup'
  alias gaacm='git add -A && git commit -m'
  alias gco='git checkout'
  alias gsw='git switch'
  alias gswc='git switch --create'
  alias gswd='git switch --detach'
  alias gres='git restore'
  alias gres-select-ours-for-conflict='git restore --ours'
  alias gres-select-theirs-for-conflict='git restore --theirs'
  alias gd='git diff'
  alias gdh='git diff HEAD~..HEAD'
  alias gds='git diff --staged'
  alias gss='git stash save'
  alias gssp='git stash pop'
  alias gssd='git stash drop'
  alias gssl='git stash list'
  # shellcheck disable=SC2139
  alias gl="git log --name-only -$git_taking_limit"
  # shellcheck disable=SC2139
  alias glo="git log --oneline -$git_taking_limit"
  alias gr='git rebase'
  alias gra='git rebase --abort'
  alias grc='git rebase --continue'
  alias gri='git rebase --interactive --autosquash'
  alias gri~='git rebase --interactive --autosquash HEAD~'
  alias gri~~='git rebase --interactive --autosquash HEAD~~'
  alias gri~~~='git rebase --interactive --autosquash HEAD~~~'
  alias gri~~~~='git rebase --interactive --autosquash HEAD~~~~'
  alias gri~~~~~='git rebase --interactive --autosquash HEAD~~~~~'
  alias gri~~~~~~='git rebase --interactive --autosquash HEAD~~~~~~'
  alias gri~~~~~~~='git rebase --interactive --autosquash HEAD~~~~~~~'
  alias gri~~~~~~~~='git rebase --interactive --autosquash HEAD~~~~~~~~'
  alias gri~~~~~~~~~='git rebase --interactive --autosquash HEAD~~~~~~~~~'
  alias gri~~~~~~~~~~='git rebase --interactive --autosquash HEAD~~~~~~~~~~'
  alias gr-onto='git rebase --onto'
  alias grev='git revert'
  alias grev~='git revert HEAD'
  alias grev~~='git revert HEAD~'
  alias grev~~~='git revert HEAD~~'
  alias grev~~~~='git revert HEAD~~~'
  alias grev~~~~~='git revert HEAD~~~~'
  alias grev~~~~~~='git revert HEAD~~~~~'
  alias grev~~~~~~~='git revert HEAD~~~~~~'
  alias grev~~~~~~~~='git revert HEAD~~~~~~~'
  alias grev~~~~~~~~~='git revert HEAD~~~~~~~~'
  alias grev~~~~~~~~~~='git revert HEAD~~~~~~~~~'
  alias gmt='git mergetool'
  alias gmerge='git merge --no-ff'
  alias gsm='git submodule'
  alias gsma='git submodule add'
  alias gsmd='git submodule deinit'
  alias gsmu='git submodule update'
  alias gsmui='git submodule update --init'
  alias gsmuir='git submodule update --init --recursive'
  alias gch='git cherry-pick'
  alias gchc='git cherry-pick --continue'
  alias gcha='git cherry-pick --abort'
  alias gchs='git cherry-pick --skip'
  alias gp='git push'
  alias gpu='git push -u'
  alias gpuo='git push -u origin'
  alias gpf='git push --force-with-lease'
  alias gpull='git pull --rebase'
  alias greflog='git reflog'
  alias gshow='git show'
  alias gclone='git clone --recurse-submodules'
  alias gf='git fetch'
  alias gfo='git fetch origin'
  alias gfp='git fetch --prune'  # GitHub（など）の上に既にない`remotes/origin/xxxx`のようなリモート追跡ブランチを、ローカルから削除するやつ
  alias gtag='git tag'
  alias gtag-delete='git tag --delete'
  alias gtagd='git tag --delete'
  alias gw='git worktree'

  # git-stash系のaliasは、ほとんどは自分でも覚えられなかったので、わかりやすい名前にする
  alias git-stash-push-message='git stash push -m'
  function git-stash-save-patch-and-message () {
      git stash push --message "$1" --patch
  }
  alias git-diff-0='git diff stash@{0}'
  alias git-diff-1='git diff stash@{1}'
  alias git-diff-2='git diff stash@{2}'

  function gtag-add-m () {
    local tag_name=$1 message=$2
    git tag --annotate "$tag_name" --message "$message"
  }

  function gwa () {
    : Makes both the new branch and the new directory.
    target_branch=$1
    git worktree add "$target_branch" "${target_branch//\#/}"
  }

  function gwab () {
    : Makes the new branch basing on a base branch.
    base_branch=$1
    new_branch=$2
    git worktree add -b "$new_branch" "${new_branch//\#/}" "$base_branch"
  }

  function gwb () {
    local branch_name=$1
    git worktree add "$branch_name" -b "$branch_name"
  }

  alias gwl='git worktree list'
  alias gwp='git worktree prune'
  alias gw-erase-removed='git worktree prune'

  function git-branch-name () {
    git branch 2> /dev/null | grep '\*\s.*' | awk '{print $2}'
  }

  function git-submodule-remove () {
    local git_root
    git_root=$(git-root)

    for submodule_path in "$@" ; do
      if [[ ! -d $submodule_path ]] ; then
        echo "The path '$submodule_path' is not found or not a directory" > /dev/stderr
        return 1
      fi
      git submodule deinit "$submodule_path" || return 1
      echo "deinit done: $submodule_path"
    done

    echo "Don't forget that delete the submodule entry from:"
    echo "  $git_root/.gitmodules"
    echo "  $git_root/.git/config"
  }

  unset git_taking_limit

  function git-push-u-origin-branch () {
    git push -u origin "$(git branch --show-current)"
  }
  alias gpuob=git-push-u-origin-branch

  function ensure-git-wip-remote-existent () {
    if [[ $DOTFILES_GIT_REMOTE_NAME_TO_PUSH_WIP == '' ]] ; then
      # shellcheck disable=SC2016
      echo '$DOTFILES_GIT_REMOTE_NAME_TO_PUSH_WIP is not set' >&2
      return 1
    fi

    if [[ ! -d $DOTFILES_GIT_REMOTE_NAME_DIR ]] ; then
      echo "The directory '$DOTFILES_GIT_REMOTE_NAME_DIR' is not found or not a directory" >&2
      return 1
    fi

    return 0
  }

  function git-wip-remote-add () {
    ensure-git-wip-remote-existent || return 1
    git remote add "$DOTFILES_GIT_REMOTE_NAME_TO_PUSH_WIP" "$DOTFILES_GIT_REMOTE_NAME_DIR"
  }

  function git-wip-push-force () {
    : "First, run 'git-remote-add-wip' if you haven't yet."

    ensure-git-wip-remote-existent || return 1
    git push --force-with-lease "$DOTFILES_GIT_REMOTE_NAME_TO_PUSH_WIP" "$(git branch --show-current)" || return 1
  }

  function git-wip-push-all-force () {
    git add -A
    git commit -m 'wip'
    git-wip-push-force
  }

  # Set casual user.name and user.email at local
  alias git-set-casual-name='git config --local user.name aiya000 && git config --local user.email aiya000.develop@gmail.com ; git config --local user.name ; git config --local user.email'
  alias cdg=cd-to-git-root
fi

## GitHub {{{

function github-change-remote-from-git-to-https () {
  local remote https_url
  remote=${1:-origin}
  https_url=$( \
    git remote get-url "$remote" \
    | sed -r 's/git@([^:]+):([^\/]+)\/(.*)/https:\/\/\1\/\2\/\3/' \
    | sed -r 's/\.git$//' \
  )
  git remote set-url "$remote" "$https_url"
  git remote get-url "$remote"
}

# }}}
## GitLab {{{

function gitlab-clone () {
  if [[ -z $DOTFILES_GITLAB_ACCESS_TOKEN ]] ; then
    # shellcheck disable=SC2016
    {
      echo 'Please create a personal access token on https://gitlab.com/-/profile/two_factor_auth:'
      echo 'and add:'
      echo '  export DOTFILES_GITLAB_ACCESS_TOKEN_NAME=it'
      echo '  export DOTFILES_GITLAB_ACCESS_TOKEN_VALUE=it'
      echo 'into ~/.zshrc_private'
      echo '(Please also see ~/.dotfiles/.private/.zshrc_private)'
    } > /dev/stderr
    return 1
  fi

  if [[ $# -lt 1 ]] ; then
    {
      echo 'expected an argument like:'
      echo "  $0 aiya000/repository-name"
    } > /dev/stderr
    return 1
  fi

  gclone "https://$DOTFILES_GITLAB_ACCESS_TOKEN_NAME:$DOTFILES_GITLAB_ACCESS_TOKEN_VALUE@$1"
}

# }}}

# }}}
# Claude Code {{{

alias c=claude
alias cresume='claude --resume'
alias ccontinue='claude --continue'
alias cc=ccontinue

# }}}
# Eco Systems for Node.js, TypeScript, and JavaScript  {{{

alias cdn=cd-to-node-root

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

function kill-vue-lsp-servers () {
  ps aux | grep 'node\|volar' # TODO: なぜかこれを挟まないとkillできない。できるなら挟まないようにする
  ps aux | grep 'volar-server\|typescript-language-server' | grep -v grep | awk '{print $2 }' | xargs kill \
    && echo killed \
    || echo failed
}

# }}}
# Another Contextual Commands {{{

alias_of copilot 'copilot --allow-tool write --allow-tool "shell(notifu-respond)" --allow-tool "shell(notifu.exe)" --allow-tool "shell(git log)" --allow-tool "shell(git show)" --allow-tool "shell(git diff)" --allow-tool "shell(git status)" --allow-tool "shell(git reflog)"'

alias ctags-kotlin-auto="ctags-auto '--exclude=*.java' '--exclude=*.html' '--exclude=*.css'"
alias ctags-typescript-auto="ctags-auto '--exclude=*.js' '--exclude=*.json'"

alias_of yay 'yay --color always'

if i_have luap ; then
  alias lua-repl=luap
elif i_have lua ; then
  alias lua-repl=lua
fi

i_have krita && alias kra=krita

# }}}

export PATH=$PATH:$HOME/.sh_generic/bin
