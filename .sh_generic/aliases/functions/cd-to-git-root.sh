#!/bin/bash

function cd-to-git-root () {
  local root
  root=$(git rev-parse --show-toplevel 2> /dev/null || return 1)
  # shellcheck disable=SC2164
  cd "$root"

  # Try recover with wslpath if simple is failed
  # shellcheck disable=SC2181
  if [[ $? -ne 0 ]] && command -v wslpath > /dev/null 2>&1 ; then
    root=$(wslpath "$root")
    echo "cd-to-git-root: Using wslpath: $root"
    cd "$root" || return 1
    return
  fi

  echo "$root"
}
