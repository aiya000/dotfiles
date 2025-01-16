#!/bin/bash

# shellcheck disable=SC1090
source ~/.dotfiles/.sh_generic/premise.sh

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
alias z='cd -'

function du-sort () {
  local paths=${1:-.}
  du -h -d 1 "$paths" | sort -h
}

# NOTE: Who did define the original - ?
function - () {
  # shellcheck disable=SC2164
  cd -
}

alias_of sudo 'sudo '  # Enable aliases on sudo
alias_of mysql 'mysql --pager="less -r -S -n -i -F -X"'

i_have say || alias say=espeak

# }}}
# Load ./aliases/** {{{

for script in ~/.sh_generic/aliases/functions/*.sh ; do
  source "$script"
done
source ~/.sh_generic/aliases/build-tools.sh
source ~/.sh_generic/aliases/os-package-managers.sh
source ~/.sh_generic/aliases/vim.sh

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
  alias ga='git add'
  alias gaa='git add -A'
  alias gap='git add -p'
  alias gb='git branch'
  alias gba='git branch --all'
  alias gbd='git branch --delete'
  alias _gbd='git branch -D'
  alias _gbdf='git branch --delete --force'
  alias gbm='git branch -m'
  alias gc='git commit --verbose'
  alias gcam='git commit --verbose --amend'
  alias gcamm='git commit --verbose --amend -m'
  alias gcm='git commit -m'
  alias gcem='git commit --allow-empty -m'
  alias gcf='git commit --fixup'
  alias gaacm='git add -A && git commit -m'
  alias gaacamm='git add -A && git commit --amend -m'
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
  alias gdst0='git diff stash@{0}'
  alias gdst1='git diff stash@{1}'
  alias gdst2='git diff stash@{2}'
  # shellcheck disable=SC2139
  alias gl="git log --name-only -$git_taking_limit"
  # shellcheck disable=SC2139
  alias glo="git log --oneline -$git_taking_limit"
  alias gll='glo | head'
  # shellcheck disable=SC2139
  alias glp="git log --patch -$git_taking_limit"
  # shellcheck disable=SC2139
  alias glf="git log --name-only -$git_taking_limit"
  alias gmv='git mv'
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
  alias grm='git rm'
  alias grmc='git rm --cached'
  alias gs='git status'
  alias gss='git stash'
  alias gsssm='git stash push -m'
  alias gssp='git stash pop'
  function gsssp () {
      git stash push --message "$1" --patch
  }
  alias gssa='git stash apply'
  alias gssd='git stash drop'
  alias gssl='git stash list'
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
  alias _gclean='git clean -fd'
  alias gp='git push'
  alias gpu='git push -u'
  alias gpuo='git push -u origin'
  alias gpua='git push -u aiya000'
  alias gpf='git push --force-with-lease'
  alias gpull='git pull --rebase'
  alias greflog='git reflog'
  alias gshow='git show'
  alias gclone='git clone --recurse-submodules'
  alias gf='git fetch'
  alias gfo='git fetch origin'
  alias gbi='git bisect'
  alias gtag='git tag'
  alias gtagam='gtag-add-m'
  alias gtag-delete='git tag --delete'
  alias gtagd='git tag --delete'
  alias gtree='git log --graph --decorate --oneline'
  alias gtree-all='git log --graph --decorate --oneline --all'
  alias gw='git worktree'
  alias gaacmp='git add -A && gcmp'
  alias gaacmu='git add -A && gcmu'
  alias gaacmb='git add -A && gcmb'
  alias gaacmr='git add -A && gcmr'

  function gcmp () {
    git commit -m "$DOTFILES_GIT_COMMIT_PREFIX_IMPROVEMENT $*"
  }

  function gcmu () {
    git commit -m "$DOTFILES_GIT_COMMIT_PREFIX_UPDATE $*"
  }

  function gcmb () {
    git commit -m "$DOTFILES_GIT_COMMIT_PREFIX_BUGFIX $*"
  }

  function gcmr () {
    git commit -m "$DOTFILES_GIT_COMMIT_PREFIX_REFACTOR $*"
  }

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

  function _grs_n () {
    git reset --soft "HEAD~${1}"
  }

  function gdst_n () {
    git diff "stash@{${1}}"
  }

  function gri_n () {
    git rebase --interactive --autosquash "HEAD~${1}"
  }

  function grev_n () {
    git revert "HEAD~${1}"
  }

  function git-branch-name () {
    git branch 2> /dev/null | grep '\*\s.*' | awk '{print $2}'
  }

  function git-log-unpushed () {
    git log "$@" "origin/$(git-branch-name)..HEAD"
  }

  function gwb () {
    local branch_name=$1
    git worktree add "$branch_name" -b "$branch_name"
  }

  alias gwl='git worktree list'
  alias gwp='git worktree prune'
  alias gw-erase-removed='git worktree prune'

  unset git_taking_limit

  function git-push-u-origin-branch () {
    git push -u origin $(git branch --show-current)
  }
  alias gpuob=git-push-u-origin-branch

  function git-push-wip () {
    local wip_name=wip-current-unique-unique-yazawa-nico
    git branch -D "$wip_name"
    git checkout -b "$wip_name"
    git add -A
    git commit -m "Today's WIP"
    git push -f origin "$wip_name"
    git checkout -
    git cherry-pick "$wip_name"
  }

  function _gre () {
    echo -n "(at '$(git rev-parse --show-toplevel)') Do you really force reset the git index tree? (y/n)"
    if read -rq _  ; then
      echo
      git reset --hard HEAD
      echo 'done'
    fi
  }

  alias ginit='git init'

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

  # Set casual user.name and user.email at local
  alias git-set-casual-name='git config --local user.name aiya000 && git config --local user.email aiya000.develop@gmail.com ; git config --local user.name ; git config --local user.email'
  alias cdg=cd-to-git-root
fi

# }}}
# GitLab {{{

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
# Others {{{

alias la='ls -a --color=auto --group-directories-first'
alias ll='ls -l --color=auto --group-directories-first'
alias llh='ls -lh --color=auto --group-directories-first'
alias lla='ls -la --color=auto --group-directories-first'

alias date-simple='date "+%Y-%m-%d %H:%M"'

# shellcheck disable=SC2139
alias e="$EDITOR"
alias eS='ls -A $VIM_SESSION | peco | xargs -I {} vim -S $VIM_SESSION/{}'
alias g=gvim
alias gS='ls -A $VIM_SESSION | peco | xargs -I {} gvim -S $VIM_SESSION/{}'

alias m=mount
alias t=vterminal
alias um=umount
alias ei=exit
alias cdp=cd-finddir
alias ki=killing-art

function ff () {
  : Find a file by taken fuzzy name.
  f . "$1"
}

function f () {
  : Find a file on a directory by taken fuzzy name.
  local base_path=$1 fuzzy_name=$2

  if i_have fdfind ; then
    fdfind "$base_path" "$fuzzy_name"
  else
    find "$base_path" -name "*$fuzzy_name*"
  fi
}

alias ctags-kotlin-auto="ctags-auto '--exclude=*.java' '--exclude=*.html' '--exclude=*.css'"
alias ctags-typescript-auto="ctags-auto '--exclude=*.js' '--exclude=*.json'"

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

# shellcheck disable=SC2139
alias e-current-session="vim-current-session $EDITOR"
# shellcheck disable=SC2139
alias g-current-session="vim-current-session $MY_GUI_EDITOR"

alias_of yay 'yay --color always'

i_have tmux && alias ta='tmux attach'
i_have nmcli && alias nmcli-connect-wifi='nmcli device wifi connect'
i_have unzip && alias unzip-cp932='unzip -O cp932'
i_have krita && alias kra=krita
i_have batcat && alias bat=batcat
i_have fdfind && alias fd=fdfind
i_have gomi && alias rm=gomi || alias rm=dust  # no more cry

# shellcheck disable=SC2139
alias mount4u.ntfs="sudo mount -o user=$(whoami),uid=$(id -u),gid=$(id -g),iocharset=utf8"
alias mount4u.vfat=mount4u.ntfs
alias mount4u.ext2='sudo mount -o iocharset=utf8'
alias mount4u.ext3=mount4u.ext2
alias mount4u.ext4=mount4u.ext2

# }}}

export PATH=$PATH:$HOME/.sh_generic/bin
