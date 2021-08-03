#!/bin/bash
function cd-finddir () {
  local depth
  depth=$([ -n "$1" ] && echo "$1" || echo 3)

  # Sort by the depth of the path
  find . -maxdepth "$depth" -type d \
    | awk '{print $1 " / " gsub("/", $1)}' \
    | sed -r 's/(.*) \/ (.*)/\2 \/ \1/g' \
    | sort -n \
    | sed -r 's/(.*) \/ (.*)/\2/g' \
    | peco --select-1 --initial-filter Fuzzy \
    | read target_dir

  # shellcheck disable=SC2181
  if [[ $? -eq 0 ]] ; then
    cd "$target_dir" || exit 1
  fi
}
