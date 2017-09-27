#!/bin/bash

function load-my-env () {
    local target_name="$1"

    case "$target_name" in
    all)
        load-my-env haskell
        load-my-env haskell-legacy
        load-my-env pkgsrc
        load-my-env ruby
        load-my-env zsh
        ;;
    haskell) if [ -d ~/.stack ] ; then
            PATH=$PATH:$HOME/.stack/programs/x86_64-linux/ghc-7.8.4/bin
        fi
        ;;
    haskell-legacy)
        if [ -d ~/.cabal ] ; then
            PATH=$PATH:$HOME/.cabal/bin
            PATH=$PATH:./.cabal-sandbox/bin
        fi
        ;;
    pkgsrc)
        if [ -d ~/pkg ] ; then
            PATH=$PATH:$HOME/pkg/bin:$HOME/pkg/sbin
        fi
        ;;
    ruby)
        if [ -d ~/.rbenv ] ; then
            PATH=$PATH:$HOME/.rbenv/bin
            PATH=$PATH:$HOME/.rbenv/versions/$(cat ~/.rbenv/version)/bin
            eval "$("$HOME/.rbenv/bin/rbenv" init -)"
        fi
        if [ -d ~/.rbenv/plugins/ruby-build/bin ] ; then
            PATH=$PATH:$HOME/.rbenv/plugins/ruby-build/bin
        fi
        ;;
    zsh)
        export ZAPACK_OPTIONS='--verbose'
        ;;
    ccache)
        export CCACHE_DISABLE=0
        export CCACHE_NODISABLE=1
        export CCACHE_BASEDIR=$HOME/hdd/tmp/ccache
        export CCACHE_DIR=$HOME/hdd/tmp/ccache
        ;;
    *)
        echo "You may haven't $target_name" > /dev/stderr
        ;;
    esac
}
