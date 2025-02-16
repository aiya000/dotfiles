#!/bin/bash

function load-my-env () {
  local target_name="$1"

  case "$target_name" in
    # Also please update /.zsh/complete/_load-my-env when update here.
    help)
      echo 'Available:'

      echo '  - cabal'
      echo '  - cargo'
      echo '  - ccache'
      echo '  - conda'
      echo '  - docker'
      echo '  - drawio'
      echo '  - gcloud'
      echo '  - gradlew'
      echo '  - idris'
      echo '  - linuxbrew'
      echo '  - nvm'
      echo '  - pkgsrc'
      echo '  - rbenv'
      echo '  - stack'
      echo '  - travis'
      echo '  - virtualenv'

      ;;

    stack)
      alias si='stack install'
      alias srunghc='stack runghc --'
      alias sghci='stack ghci --'
      alias stack-haddock-gen='stack haddock .'
      alias shaddock-gen='stack haddock .'
      alias shaddock-gen-open='stack haddock --open .'
      alias stack-build-profile='stack build --profile'
      alias stack-make-new-package-yaml='cp ~/.dotfiles/Files/package.yaml .'
      alias stack-cabal-sdit='stack exec -- cabal sdist'

      function stack-cabal-upload() {
        stack exec -- cabal upload "$1"
      }

      function stack-new-default() {
        stack new "$1" simple
      }
      ;;

    cabal)
      alias ci='cabal new-install'

      if [[ -d ~/.cabal ]] ; then
        PATH=$PATH:$HOME/.cabal/bin:./.cabal-sandbox/bin
      else
        echo "$HOME/.cabal is not found." > /dev/stderr
      fi
      ;;

    idris)
      function run-idris() {
        if idris "$1" -o "/tmp/$1.idris-run-output" && "/tmp/$1.idris-run-output" ; then
          # shellcheck disable=SC2028
          # shellcheck disable=SC1117
          echo "\n>>> run-idris is succeed"
        else
          # shellcheck disable=SC2028
          # shellcheck disable=SC1117
          echo "\n>>> run-idris is failed"
        fi
        if [[ -f $1 ]] ; then
          rm "/tmp/$1.idris-run-output"
        fi
      }
      alias runidris=run-idris
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

    # NOTE:
    # This is slower a little.
    # For performance, add $HOME/.nvm/versions/node/<version>/bin into $PATH instead.
    nvm)
      if [[ -f $NVM_DIR/nvm.sh ]] ; then
        # shellcheck disable=SC1090
        source "$NVM_DIR/nvm.sh"
        return
      fi
      echo "$NVM_DIR/nvm.sh is not found." > /dev/stderr
      return 1
      ;;

    gcloud)
      if [[ -f $HOME/bin/google-cloud-sdk/path.zsh.inc ]] ; then
        # shellcheck disable=SC1090
        source "$HOME/bin/google-cloud-sdk/path.zsh.inc"
      fi
      if [[ -f $HOME/bin/google-cloud-sdk/completion.zsh.inc ]] ; then
        # shellcheck disable=SC1090
        source "$HOME/bin/google-cloud-sdk/completion.zsh.inc"
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
      if [[ -f ~/.linuxbrew/bin/brew ]] ; then
        eval "$(~/.linuxbrew/bin/brew shellenv)"
        return
      fi

      if [[ -f /home/linuxbrew/.linuxbrew/bin/brew ]] ; then
        eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
        return
      fi

      return 1
      ;;

    docker)
      alias d=docker
      alias da=docker-attach-menu.sh
      alias dki=docker-kill-menu.sh
      alias dkill='docker kill'
      alias dps='docker ps'
      alias drm='docker rm'
      alias drmi='docker rmi'
      alias drun='docker run'

      alias docker-force-remove-all-containers='docker rm -f $(docker ps -a -q)'
      alias docker-force-remove-all-images='docker rmi -f $(docker images -q)'
      # shellcheck disable=SC2142
      alias docker-force-clean-volumes='docker volume rm $(docker volume ls | awk "{print $2}")'

      function docker-force-remove-all-all () {
        docker-force-remove-all-containers
        docker-force-remove-all-images
        docker-force-clean-volumes
      }
      ;;

    drawio)
      alias drio=draw.io

      # NOTE: Before, draw.io exports foo.xml.png for foo.xml, but the backward compatility broke.
      function drio-export-png () {
        local xml_name
        xml_name=$1

        draw.io --embed-diagram --export --format png "$xml_name" --output "${xml_name}.png"
      }
      ;;

    gradlew)
      alias grwj='./gradlew jar'
      ;;

    virtualenv)
      function virtualenv-activate () {
        if [[ -f ./.venv/bin/activate ]] ; then
          # shellcheck disable=SC1091
          source ./.venv/bin/activate
          return
        fi

        echo './.venv/bin/activate was not found, please load it yourself...' > /dev/stderr
        return 1
      }
      ;;

    conda)
      __conda_setup=$("$HOME/.anaconda3/bin/conda" 'shell.zsh' 'hook' 2> /dev/null)
      if [[ $? -eq 0 ]] ; then
        eval "$__conda_setup"
      else
        if [[ -f $HOME/.anaconda3/etc/profile.d/conda.sh ]]; then
          source "$HOME/.anaconda3/etc/profile.d/conda.sh"
        else
          PATH="$HOME/.anaconda3/bin:$PATH"
        fi
      fi
      unset __conda_setup
      ;;

    cargo)
      if [[ -f ~/.cargo/env ]] ; then
        # shellcheck disable=SC1090
        source ~/.cargo/env
        return
      fi
      echo '~/.cargo/env is not found.' > /dev/stderr
      return 1
      ;;

    *)
      echo "Undefined env: $target_name" > /dev/stderr
      ;;
  esac
}
