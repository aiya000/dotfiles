#!/bin/bash

if [[ -d ~/hdd/.ccache ]] ; then
    export USE_CCACHE=1
    export CCACHE_DIR=~/hdd/.ccache
    export set CC='ccache gcc'
    export set CXX='ccache g++'
fi

DUSTBOX=$HOME/.backup/dustbox/$(date +'%Y-%m-%d')
export DUSTBOX
