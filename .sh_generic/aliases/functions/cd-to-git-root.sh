#!/bin/bash

function cd-to-git-root () {
  local root simple
  root=$(git rev-parse --show-toplevel 2> /dev/null || exit 1)

  # Try recover with wslpath if simple is failed
  if ! simple=$(cd "$root" 2>&1) && command -v wslpath > /dev/null 2>&1 ; then
    root=$(wslpath "$root")
    echo "cd-to-git-root: Using wslpath: $root"
    cd "$root" || exit 1
    return
  fi

  echo "$simple"
}
