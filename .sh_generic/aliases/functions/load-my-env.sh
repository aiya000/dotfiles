#!/bin/bash

function load-my-env () {
  local target_name="$1"
  case "$target_name" in
    all)
      load-my-env stack
      load-my-env cabal
      load-my-env pkgsrc
      load-my-env ruby
      load-my-env zsh
      load-my-env bash
      load-my-env travis
      load-my-env ccache
      load-my-env linuxbrew
      ;;
    stack)
      if [ -d ~/.stack ] ; then
        PATH=$PATH:$HOME/.stack/programs/x86_64-linux/ghc-7.8.4/bin
      fi
      #FIXME: Avoid to load that is failure on the cygwin
      if [[ $(uname | grep -i cygwin) == '' ]] ; then
        # Use stack-completion
        if type stack > /dev/null 2>&1 ; then
          # This completion needs compinit and bashcompinit function
          # > autoload -U compinit     && compinit
          # > autoload -U bashcompinit && bashcompinit
          eval "$(stack --bash-completion-script stack)"
        fi
      fi
      ;;
    cabal)
      if [ -d ~/.cabal ] ; then
        PATH=$PATH:$HOME/.cabal/bin:./.cabal-sandbox/bin
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
      if [ -f /usr/share/git/completion/git-completion.zsh ] && [ ! -f "$ZDOTDIR/_git" ] ; then
        cp /usr/share/git/completion/git-completion.zsh "$ZDOTDIR/_git"
      fi
      ;;
    bash)
      if [ -f "$HOME/.bashfiles/git-completion.bash" ] ; then
        # shellcheck disable=SC1090
        source "$HOME/.bashfiles/git-completion.bash"
      fi
      ;;
    travis)
      if [ -f ~/.travis/travis.sh ] ; then
        # shellcheck disable=SC1090
        source ~/.travis/travis.sh
      fi
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
