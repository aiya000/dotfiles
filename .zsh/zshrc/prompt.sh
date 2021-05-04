#!/usr/bin/env zsh

function zshrc::prompt::main () {
  # A maid represents a status of the exit code
  local feature="%(?.%{${fg_bold[green]}%}.%{${fg_bold[blue]}%})%(?!(*^-^)!(;^-^%))%{${reset_color}%}"
  local current_dir="%{$fg[yellow]%}%~%{$reset_color%}"

  export PROMPT="${feature} ${current_dir}%{$reset_color%} | $(zshrc::prompt::sub_status)
%{$fg[cyan]%}>>> %{$reset_color%}"
}

function zshrc::prompt::sub_status () {
  function get_git_changes () {
    # Subtract a head line minute
    local changes=$(( $(git status --short 2> /dev/null | wc -l) - 1 ))
    if [ "$changes" -ge 1 ] ; then
      echo "%{$bg[white]$fg[black]%}[change:${changes}]%{$reset_color%}"
    fi
  }

  function get_git_commits () {
    local commits
    commits=$(git status --short 2> /dev/null | head -1 | grep -o '\[.*\]')
    if [ "$?" -eq 0 ] ; then
      echo "%{$bg[red]$fg[black]%}${commits}%{$reset_color%}"
    fi
  }

  function get_git_stash_status () {
    local item_num=$({git stash list 2> /dev/null || echo -n ''} | wc -l)
    if [[ $item_num -ge 1 ]] ; then
      echo "%{$bg[cyan]$fg[black]%}[stash:${item_num}]%{$reset_color%}"
    fi
  }

  function get_git_branch_name () {
    local branches
    branches=$(git branch 2> /dev/null)
    if [ "$?" -ne 0 ] ; then
      echo '[NO REPO]'
      exit
    fi
    local branch_name=$(echo $branches | grep '\*\s.*' | awk '{print $2}')
    echo "%{$bg[green]$fg[black]%}[${branch_name}]%{$reset_color%}"
  }

  if [[ $ZSHRC_PROMPT_GIT_STATUS_DISABLE -ne 0 ]] ; then
    echo '[GitStatus Disabled]'
    return
  fi
  echo $(get_git_changes)$(get_git_commits)$(get_git_stash_status)$(get_git_branch_name)
}

# Run once to start up
zshrc::prompt::main
