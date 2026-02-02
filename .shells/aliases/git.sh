#!/bin/bash

if ! i-have git ; then
  return
fi

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

function git-diff-stash-nth () {
  : Example
  : git-diff-stash-nth 0 -- 'git diff stash@{0}'
  : git-diff-stash-nth 1 -- 'git diff stash@{1}'
  : git-diff-stash-nth -- 'error'

  local stash_index=$1
  if [[ ! $stash_index =~ ^[0-9]+$ ]] ; then
    echo "The first argument must be a stash index (a non-negative integer)." > /dev/stderr
    return 1
  fi

  git diff "stash@{$stash_index}"
}

function git-stash-apply-nth () {
  : Example
  : git-stash-apply-nth 0 -- 'git stash apply stash@{0}'
  : git-stash-apply-nth 1 -- 'git stash apply stash@{1}'
  : git-stash-apply-nth -- 'error'

  local stash_index=$1
  if [[ ! $stash_index =~ ^[0-9]+$ ]] ; then
    echo "The first argument must be a stash index (a non-negative integer)." > /dev/stderr
    return 1
  fi

  git stash apply "stash@{$stash_index}"
}

alias gssa=git-stash-apply-nth

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

alias git-checkout-all-theirs='git checkout --theirs .'

# Set casual user.name and user.email at local
alias git-set-casual-name='git config --local user.name aiya000 && git config --local user.email aiya000.develop@gmail.com ; git config --local user.name ; git config --local user.email'
alias cdg=cd-to-git-root

## GitHub

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

  function gh-workflow-run-all () {
: <<EOF
  ```yaml
  on:
    workflow_dispatch:
  ```
  に設定されている全てのGitHub Actions Workflowを実行する
EOF

  gh workflow list --json name -q '.[].name' | while read -r workflow ; do
    echo "Running: $workflow"
    gh workflow run "$workflow" --ref main
  done
}

## GitLab

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

  git clone "https://$DOTFILES_GITLAB_ACCESS_TOKEN_NAME:$DOTFILES_GITLAB_ACCESS_TOKEN_VALUE@$1"
}
