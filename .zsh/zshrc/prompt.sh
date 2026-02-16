#!/usr/bin/env zsh

# Preferences
PROMPT_HEAD_CHAR=$

# State
git_state=""

function zshrc::prompt::vim_mode () {
  case ${KEYMAP:-viins} in
    vicmd)         echo "%{$bg[magenta]$fg[black]%}[N]%{$reset_color%}" ;;
    visual|viopp)  echo "%{$bg[yellow]$fg[black]%}[V]%{$reset_color%}" ;; # TODO: 動いてないので修正する
    *)             echo "%{$bg[cyan]$fg[black]%}[I]%{$reset_color%}" ;;
  esac
}

function zshrc::prompt::refresh_git_state () {
  git_state=$(zshrc::prompt::sub_status)
}

function zshrc::prompt::main () {
  # A maid represents a status of the exit code
  local feature="%(?.%{${fg_bold[green]}%}.%{${fg_bold[blue]}%})%(?!(*^-^)!(;^-^%))%{${reset_color}%}"
  local current_dir="%{$fg[yellow]%}%~%{$reset_color%}"

  export PROMPT="${feature} ${current_dir}%{$reset_color%} | $(zshrc::prompt::vim_mode) | ${git_state}
%{$fg[cyan]%}$PROMPT_HEAD_CHAR %{$reset_color%}"
}

function zshrc::prompt::sub_status () {
  if [[ $ZSHRC_PROMPT_GIT_DISABLE -ne 0 ]] ; then
    echo '[git on prompt is disabled] (env var)'
    return
  fi

  if declare -f should_not_prompt_show_git_status > /dev/null 2>&1 && should_not_prompt_show_git_status ; then
    echo '[git on prompt is disabled (func)]'
    return
  fi

  zshrc::prompt::sub_status::show
}

function zshrc::prompt::sub_status::show () {
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

  echo $(get_git_changes)$(get_git_commits)$(get_git_stash_status)$(get_git_branch_name)
}

function zshrc::prompt::preexec () {
  if [[ "$1" == git\ * ]] ; then
    _zshrc_prompt_git_cmd_ran=1
  fi
}

function zshrc::prompt::precmd () {
  if [[ -n "$_zshrc_prompt_git_cmd_ran" ]] ; then
    zshrc::prompt::refresh_git_state
    unset _zshrc_prompt_git_cmd_ran
  fi
}

chpwd_functions+=(zshrc::prompt::refresh_git_state)
preexec_functions+=(zshrc::prompt::preexec)
precmd_functions+=(zshrc::prompt::precmd)

zshrc::prompt::refresh_git_state
zshrc::prompt::main
