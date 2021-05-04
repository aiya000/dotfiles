#!/bin/bash

function load-my-env () {
  local target_name="$1"

  case "$target_name" in
    all)
      load-my-env stack
      load-my-env cabal
      load-my-env pkgsrc
      load-my-env rbenv
      load-my-env travis
      load-my-env ccache
      load-my-env linuxbrew
      ;;

    stack)
      if [[ -d ~/.stack ]] ; then
        PATH=$PATH:$HOME/.stack/programs/x86_64-linux/ghc-7.8.4/bin
      fi
      echo "$HOME/.stack is not found." > /dev/stderr
      ;;

    cabal)
      if [[ -d ~/.cabal ]] ; then
        PATH=$PATH:$HOME/.cabal/bin:./.cabal-sandbox/bin
      fi
      echo "$HOME/.cabal is not found." > /dev/stderr
      ;;

    pkgsrc)
      if [[ -d ~/pkg ]] ; then
        PATH=$PATH:$HOME/pkg/bin:$HOME/pkg/sbin
        return
      fi
      echo "$HOME/pkg is not found." > /dev/stderr
      ;;

    rbenv)
      if [[ -d ~/.rbenv ]] ; then
        PATH=$HOME/.rbenv/bin:$HOME/.rbenv/versions/$(cat ~/.rbenv/version)/bin:$PATH
        eval "$("$HOME/.rbenv/bin/rbenv" init -)"
      else
        echo "$HOME/.rbenv is not found." > /dev/stderr
      fi

      if [[ -d ~/.rbenv/plugins/ruby-build/bin ]] ; then
        PATH=$PATH:$HOME/.rbenv/plugins/ruby-build/bin
      else
        echo "$HOME/.rbenv/plugins/ruby-build/bin is not found." > /dev/stderr
      fi
      ;;

    travis)
      if [[ -f ~/.travis/travis.sh ]] ; then
        # shellcheck disable=SC1090
        source ~/.travis/travis.sh
        return
      fi
      echo "$HOME/.travis/travis.sh is not found." > /dev/stderr
      ;;

    ccache)
      export USE_CCACHE=1
      export CCACHE_DISABLE=0
      export set CC='ccache gcc'
      export set CXX='ccache g++'
      alias make-ccache='make CC="ccache gcc"'
      echo export USE_CCACHE=1
      echo export set CC='ccache gcc'
      echo export set CXX='ccache g++'
      echo alias make-ccache='make CC="ccache gcc"'

      if [[ -d ~/hdd/.ccache ]] ; then
        export CCACHE_DIR=~/hdd/.ccache
        echo export CCACHE_DIR=~/hdd/.ccache
      else
        export CCACHE_DIR=~/.ccache
        echo export CCACHE_DIR=~/.ccache
      fi
      ;;

    linuxbrew)
      eval "$(~/.linuxbrew/bin/brew shellenv)"
      ;;

    *)
      echo "You may haven't $target_name" > /dev/stderr
      ;;
  esac
}
