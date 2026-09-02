#!/usr/bin/env zsh

zmodload zsh/datetime 2>/dev/null

# Preferences
PROMPT_HEAD_CHAR=$

# Show a pull request of the current branch, and issues closed by it (requires `gh`).
# Set 1 to disable.
ZSHRC_PROMPT_GH_DISABLE=${ZSHRC_PROMPT_GH_DISABLE:-0}
# How long (seconds) a fetched result is reused before `gh` is called again
ZSHRC_PROMPT_GH_CACHE_TTL=${ZSHRC_PROMPT_GH_CACHE_TTL:-300}
ZSHRC_PROMPT_GH_CACHE_DIR=${ZSHRC_PROMPT_GH_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/zshrc-prompt-gh}

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

# Current epoch seconds. Falls back to date(1) when zsh/datetime is unavailable.
function _zshrc_prompt_now () {
  if [[ -n ${EPOCHSECONDS+x} ]] ; then
    REPLY=$EPOCHSECONDS
  else
    REPLY=$(date +%s)
  fi
}

# Sets $REPLY to a cache path prefix for the (repo, branch) pair
function _zshrc_prompt_gh_cache_base () {
  local key="$1#$2"
  REPLY="${ZSHRC_PROMPT_GH_CACHE_DIR}/${key//\//%}"
}

# Runs `gh` and stores the result. Meant to be called in a detached background process.
function _zshrc_prompt_gh_fetch () {
  local repo_root=$1 branch=$2

  local -a template=(
    '{{.state}}|{{.number}}|'
    '{{range $i, $e := .closingIssuesReferences}}{{if $i}},{{end}}{{$e.number}}{{end}}'
  )

  # '-' means "nothing to show". It is cached too, so that a branch without a PR
  # does not hit the network on every prompt.
  local result='-'
  if command -v gh > /dev/null 2>&1 ; then
    local raw
    raw=$(cd "$repo_root" && gh pr view "$branch" \
      --json state,number,closingIssuesReferences \
      --template "${(j..)template}" 2>/dev/null)
    if [[ ${raw%%|*} == 'OPEN' ]] ; then
      result=${raw#*|}
    fi
  fi

  _zshrc_prompt_gh_cache_base "$repo_root" "$branch"
  local data_file="${REPLY}.data"
  mkdir -p "${data_file:h}" 2>/dev/null || return
  print -r -- "$result" > "${data_file}.$$.new" 2>/dev/null \
    && mv -f "${data_file}.$$.new" "$data_file" 2>/dev/null
}

# Spawns a detached `gh` fetch when the cache expired.
# MUST NOT be called inside a command substitution that the prompt waits on,
# so that the spawned process never keeps the prompt's pipe open.
function _zshrc_prompt_gh_maybe_refresh () {
  local repo_root=$1 branch=$2

  _zshrc_prompt_gh_cache_base "$repo_root" "$branch"
  local stamp_file="${REPLY}.stamp"

  local last_try=0
  if [[ -r $stamp_file ]] ; then
    last_try=$(<"$stamp_file")
    [[ $last_try == <-> ]] || last_try=0
  fi

  _zshrc_prompt_now
  local now=$REPLY
  if (( now - last_try < ZSHRC_PROMPT_GH_CACHE_TTL )) ; then
    return
  fi

  # Stamp before forking, so concurrent prompts do not pile up `gh` processes
  mkdir -p "${stamp_file:h}" 2>/dev/null || return
  print -r -- "$now" > "$stamp_file" 2>/dev/null

  ( _zshrc_prompt_gh_fetch "$repo_root" "$branch" ) < /dev/null > /dev/null 2>&1 &
}

# Renders the cached result only. Never touches the network.
function _zshrc_prompt_gh_render () {
  local repo_root=$1 branch=$2

  _zshrc_prompt_gh_cache_base "$repo_root" "$branch"
  local data_file="${REPLY}.data"
  [[ -r $data_file ]] || return

  local cached
  cached=$(<"$data_file")
  [[ -n $cached && $cached != '-' ]] || return

  local pr_number=${cached%%|*}
  local issues=${cached#*|}
  [[ $pr_number == <-> ]] || return

  # 208 is orange of the 256 color palette ($bg has no orange)
  local out="%K{208}%F{black}[PR: #${pr_number}]%f%k"
  if [[ -n $issues ]] ; then
    local -a issue_numbers=( ${(s.,.)issues} )
    out+="%{$bg[blue]$fg[white]%}[Issue: #${(j.,#.)issue_numbers}]%{$reset_color%}"
  fi
  echo "$out"
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

  # Pull request / issue of the current branch, served from a cache that is
  # refreshed by a detached background process
  local gh_str=''
  if [[ $ZSHRC_PROMPT_GH_DISABLE -eq 0 && $branch_name != 'HEAD' ]] ; then
    local repo_root
    repo_root=$(git rev-parse --show-toplevel 2>/dev/null)
    if [[ -n $repo_root ]] ; then
      _zshrc_prompt_gh_maybe_refresh "$repo_root" "$branch_name"
      gh_str=$(_zshrc_prompt_gh_render "$repo_root" "$branch_name")
    fi
  fi

  echo "${changes_str}${commits_str}${stash_str}%{$bg[green]$fg[black]%}[${branch_name}]%{$reset_color%}${gh_str}"
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
