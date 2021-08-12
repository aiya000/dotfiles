#!/bin/bash

function cd-to-git-root () {
  local root simple
  root=$(git rev-parse --show-toplevel 2> /dev/null || return 1)
  cd "$root" 2>&1  # Don't use a variable to avoid executing in a sub shell

  # Try recover with wslpath if simple is failed
  if [[ $? -ne 0 ]] && command -v wslpath > /dev/null 2>&1 ; then
    root=$(wslpath "$root")
    echo "cd-to-git-root: Using wslpath: $root"
    cd "$root" || return 1
    return
  fi

  echo "$simple"
}
