#!/bin/bash

function cd-to-node-root::find-node-root-dir () {
  local current_dir=$1

  if [[ $(realpath "$current_dir") == / ]]  ; then
    return 1
  fi

  if ls "$current_dir/package.json" > /dev/null 2>&1 ; then
    echo "$current_dir"
    return 0
  fi

  cd-to-node-root::find-node-root-dir "$current_dir/.."
}

function cd-to-node-root () {
  : cd to a directory that have a package.json
  local root=$(cd-to-node-root::find-node-root-dir .)
  if [[ $root == '' ]] ; then
    echo 'No package.json found'
    return 1
  fi
  cd "$root"
}
