#!/bin/bash

# https://unix.stackexchange.com/questions/252747/how-can-i-un-export-a-variable-without-losing-its-value
function force-unexport() {
    while [ "$#" -ne 0 ]; do
        eval "set -- \"\${$1}\" \"\${$1+set}\" \"\$@\""
        if [ -n "$2" ]; then
            unset "$3"
            eval "$3=\$1"
        fi
        shift; shift; shift
    done
}
