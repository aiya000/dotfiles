#!/usr/bin/env zsh

# Preferences
PROMPT_HEAD_CHAR=$

# State
git_state=""
_zshrc_prompt_async_fd=0

function _zshrc_prompt_vim_mode () {
  case ${KEYMAP:-viins} in
    vicmd)         echo "%{$bg[magenta]$fg[black]%}[N]%{$reset_color%}" ;;
    visual|viopp)  echo "%{$bg[yellow]$fg[black]%}[V]%{$reset_color%}" ;; # TODO: 動いてないので修正する
    *)             echo "%{$bg[cyan]$fg[black]%}[I]%{$reset_color%}" ;;
  esac
}

function _zshrc_prompt_refresh_git_state () {
  git_state=$(_zshrc_prompt_sub_status)
}

function _zshrc_prompt_main () {
  # A maid represents a status of the exit code
  local feature="%(?.%{${fg_bold[green]}%}.%{${fg_bold[blue]}%})%(?!(*^-^)!(;^-^%))%{${reset_color}%}"
  local current_dir="%{$fg[yellow]%}%~%{$reset_color%}"

  export PROMPT="${feature} ${current_dir}%{$reset_color%} | $(_zshrc_prompt_vim_mode) | ${git_state}
%{$fg[cyan]%}$PROMPT_HEAD_CHAR %{$reset_color%}"
}

function _zshrc_prompt_sub_status () {
  if [[ $ZSHRC_PROMPT_GIT_DISABLE -ne 0 ]] ; then
    echo '[git on prompt is disabled] (env var)'
    return
  fi

  if declare -f should_not_prompt_show_git_status > /dev/null 2>&1 && should_not_prompt_show_git_status ; then
    echo '[git on prompt is disabled (func)]'
    return
  fi

  _zshrc_prompt_sub_status_show
}

function _zshrc_prompt_sub_status_show () {
  local git_status
  git_status=$(git status --short --branch 2>/dev/null) || {
    echo '[NO REPO]'
    return
  }

  # Parse all lines at once (no repeated git calls)
  local -a lines=("${(@f)git_status}")
  local header=${lines[1]}
  local change_count=$(( ${#lines} - 1 ))

  # Changed files count
  local changes_str=''
  if [[ $change_count -ge 1 ]] ; then
    changes_str="%{$bg[white]$fg[black]%}[change:${change_count}]%{$reset_color%}"
  fi

  # Ahead/behind: extract [ahead N] / [behind N] etc. from header
  local commits_str=''
  if [[ $header =~ '\[.*\]' ]] ; then
    commits_str="%{$bg[red]$fg[black]%}${MATCH}%{$reset_color%}"
  fi

  # Stash count
  local stash_str=''
  local item_num
  item_num=$(( $(git stash list 2>/dev/null | wc -l) ))
  if [[ $item_num -ge 1 ]] ; then
    stash_str="%{$bg[cyan]$fg[black]%}[stash:${item_num}]%{$reset_color%}"
  fi

  # Branch name from header: "## main...origin/main [ahead 1]" → "main"
  local branch_name=${header#'## '}
  branch_name=${branch_name%%...*}
  branch_name=${branch_name%% *}

  echo "${changes_str}${commits_str}${stash_str}%{$bg[green]$fg[black]%}[${branch_name}]%{$reset_color%}"
}

# Called by ZLE when the background git refresh completes
function _zshrc_prompt_async_update() {
  local fd=$1
  zle -F "$fd" 2>/dev/null
  IFS= read -r -u "$fd" git_state
  exec {fd}<&-
  _zshrc_prompt_async_fd=0
  _zshrc_prompt_main
  zle reset-prompt 2>/dev/null
}

# Start async git state refresh (used in precmd)
function _zshrc_prompt_start_async_refresh() {
  # Cancel any previous pending refresh
  if (( _zshrc_prompt_async_fd )) ; then
    zle -F "$_zshrc_prompt_async_fd" 2>/dev/null
    exec {_zshrc_prompt_async_fd}<&-
    _zshrc_prompt_async_fd=0
  fi
  exec {_zshrc_prompt_async_fd}< <(_zshrc_prompt_sub_status)
  zle -F "$_zshrc_prompt_async_fd" _zshrc_prompt_async_update 2>/dev/null
}

precmd_functions+=(_zshrc_prompt_start_async_refresh)

_zshrc_prompt_refresh_git_state
_zshrc_prompt_main
