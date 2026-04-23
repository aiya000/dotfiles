#!/bin/bash

# `vars.sh`, `aliases.sh`から、前提として読み込まれるファイル

function dotshells::find_local_ip () {
  ifconfig 2>/dev/null | \
    awk '/inet / && !/127\.0\.0\.1/ && !/169\.254\./{
      ip = $2; sub(/^addr:/, "", ip); print ip; exit
    }'
}

local_ip=

function bashtoys-refresh-local-ip () {
  local_ip=$(dotshells::find_local_ip)
}

function bashtoys-refresh-ntfy-serving-url () {
  bashtoys-refresh-local-ip
  export BASH_TOYS_NTFY_SERVING_URL
  BASH_TOYS_NTFY_SERVING_URL="http://$local_ip:${DOTSHELLS_NTFY_SERVING_PORT:-333535}"
}

function dotshells::define_ntfy_serving_url () {
  if [[ $BASH_TOYS_NTFY_SERVING_URL == '' ]] ; then
    bashtoys-refresh-ntfy-serving-url
  fi
}

function bashtoys-refresh-ntfy-server-base-url () {
  if [[ $DOTSHELLS_NTFY_SERVER_PORT == '' ]] ; then
    echo "(bashtoys-refresh-ntfy-server-base-url) Error: DOTSHELLS_NTFY_SERVER_PORT is not set." >&2
    return 1
  fi

  bashtoys-refresh-local-ip
  export BASH_TOYS_NTFY_SERVER_BASE_URL
  BASH_TOYS_NTFY_SERVER_BASE_URL="http://$local_ip:$DOTSHELLS_NTFY_SERVER_PORT"
}

function dotshells::define_ntfy_server_base_url () {
  if [[ $BASH_TOYS_NTFY_SERVER_BASE_URL == '' ]] ; then
    bashtoys-refresh-ntfy-server-base-url
  fi
}
