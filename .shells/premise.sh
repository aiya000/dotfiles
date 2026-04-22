#!/bin/bash

# `vars.sh`, `aliases.sh`から、前提として読み込まれるファイル

function dotshells::find_local_ip () {
  ifconfig 2>/dev/null | \
    awk '/inet / && !/127\.0\.0\.1/ && !/169\.254\./{
      ip = $2; sub(/^addr:/, "", ip); print ip; exit
    }'
}

function bashtoys-refresh-ntfy-serving-url () {
  export BASH_TOYS_NTFY_SERVING_URL
  BASH_TOYS_NTFY_SERVING_URL="http://$(dotshells::find_local_ip):${DOTSHELLS_NTFY_SERVING_PORT:-333535}"
}

function dotshells::define_ntfy_serving_url () {
  if [[ $BASH_TOYS_NTFY_SERVING_URL == '' ]] ; then
    bashtoys-refresh-ntfy-serving-url
  fi
}
