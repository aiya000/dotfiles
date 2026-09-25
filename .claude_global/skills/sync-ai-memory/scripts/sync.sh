#!/usr/bin/env bash
# Keeps ~/.ai-memory -- a git repository shared by the local machine and Claude
# Code cloud sessions -- level with its remote.
#
# Usage:
#   sync.sh prepare [CLONE_URL]     make ~/.ai-memory usable and pull it
#   sync.sh publish MESSAGE PATH... commit PATH... (relative to ~/.ai-memory) and push
#   sync.sh drop MESSAGE PATH...    git rm the tracked ones of PATH..., then commit and push
#
# prepare
#   - ~/.ai-memory is a symlink: pull it (rebase, autostash)
#   - it is missing and CLAUDE_CODE_REMOTE=true: clone $AI_MEMORY_REPO (owner/name)
#     into ${XDG_CACHE_HOME:-~/.cache}/ai-memory -- or from CLONE_URL when given --
#     and point ~/.ai-memory at the clone. The container is thrown away with the
#     session, so the cache is enough: whatever matters is pushed by `publish`
#   - otherwise: exit 3, and the caller asks the user to set it up
#
# publish
#   Pulls first, commits only the given paths, and pushes, retrying with a
#   rebase when the remote moved (2s, 4s, 8s, 16s). MEMORY-INDEX.md is expected
#   to have `merge=union` in .gitattributes, so two appends never conflict.
#
# The real target of ~/.ai-memory is never printed: it is kept out of logs on
# purpose (see AGENTS.global.md, "Memory Files").
set -euo pipefail

mem=~/.ai-memory

die() { printf 'sync-ai-memory: %s\n' "$*" >&2; exit "${code:-1}"; }

in_repo() ( cd "$mem" && "$@" )

has_upstream() { in_repo git rev-parse --abbrev-ref --symbolic-full-name '@{u}' >/dev/null 2>&1; }

pull() {
  has_upstream || { echo 'no upstream branch; skipped pull'; return 0; }
  in_repo git pull --rebase --autostash --quiet
}

check_union() {
  if ! in_repo git check-attr merge -- MEMORY-INDEX.md | grep -q ': merge: union$'; then
    echo 'warning: MEMORY-INDEX.md has no `merge=union` in .gitattributes;' \
      'appends from two machines will conflict' >&2
  fi
}

prepare() {
  if [ -L "$mem" ]; then
    [ -d "$mem/.git" ] || in_repo git rev-parse --git-dir >/dev/null 2>&1 ||
      code=3 die '~/.ai-memory is not a git repository'
    pull
    check_union
    echo 'ready: ~/.ai-memory'
    return
  fi
  [ -e "$mem" ] && code=3 die '~/.ai-memory exists but is not a symlink; ask the user'
  [ "${CLAUDE_CODE_REMOTE:-}" = true ] || code=3 die '~/.ai-memory is not set up; ask the user'
  [ -n "${AI_MEMORY_REPO:-}" ] ||
    code=3 die 'AI_MEMORY_REPO (owner/name) is not set in the cloud environment settings'

  local cache="${XDG_CACHE_HOME:-$HOME/.cache}/ai-memory"
  if [ ! -d "$cache/.git" ]; then
    mkdir -p "$(dirname "$cache")"
    git clone --quiet "${1:-https://github.com/$AI_MEMORY_REPO.git}" "$cache"
  fi
  ln -sfn "$cache" "$mem"
  pull
  check_union
  echo 'ready: ~/.ai-memory (cloud clone in the cache directory)'
}

publish() {
  [ $# -ge 2 ] || die 'usage: sync.sh publish MESSAGE PATH...'
  [ -L "$mem" ] || code=3 die '~/.ai-memory is not set up; run `sync.sh prepare` first'
  local message=$1
  shift
  in_repo git add -- "$@"
  commit_and_push "$message" "$@"
}

# Commits PATH... only -- whatever else is staged or dirty stays out -- and
# pushes, rebasing onto the remote and retrying when it moved.
commit_and_push() {
  local message=$1
  shift
  if in_repo git diff --cached --quiet -- "$@"; then
    echo 'nothing to commit'
  else
    in_repo git commit --quiet -m "$message" -- "$@"
  fi

  has_upstream || { echo 'committed; no upstream branch, so not pushed' >&2; return 0; }
  local wait
  for wait in 0 2 4 8 16; do
    sleep "$wait"
    in_repo git pull --rebase --autostash --quiet || { in_repo git rebase --abort 2>/dev/null; die 'rebase failed; resolve by hand'; }
    if in_repo git push --quiet; then
      echo 'pushed'
      return
    fi
  done
  die 'push failed 5 times; the commit is kept locally'
}

drop() {
  [ $# -ge 2 ] || die 'usage: sync.sh drop MESSAGE PATH...'
  [ -L "$mem" ] || code=3 die '~/.ai-memory is not set up; run `sync.sh prepare` first'
  local message=$1
  shift
  # Only the paths git knows: a marker that never existed (a handoff nobody
  # declined) is simply skipped, rather than failing the commit's pathspec.
  local tracked=()
  mapfile -t tracked < <(in_repo git ls-files -- "$@")
  if [ ${#tracked[@]} -eq 0 ]; then
    echo 'nothing to drop'
    return 0
  fi
  in_repo git rm --quiet -- "${tracked[@]}"
  commit_and_push "$message" "${tracked[@]}"
}

case "${1:-}" in
  prepare) shift; prepare "$@" ;;
  publish) shift; publish "$@" ;;
  drop) shift; drop "$@" ;;
  *) die 'usage: sync.sh prepare [CLONE_URL] | publish MESSAGE PATH... | drop MESSAGE PATH...' ;;
esac
