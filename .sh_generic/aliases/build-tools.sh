#!/bin/bash

# TODO: Rename this file to x-load-build-tools.sh
# TODO: Rename xl function to x-load-build-tools and helper functions.
# TODO: `alias xl=x-load-build-tools`

# Defines aliases "x[bceirst]" at a runtime
# ```shell-session
# TODO: Write an example
# ```

function xl::echo () {
  echo "build-tools.sh>> $1"
}

function xl::define () {
  if i_have stack && [[ -f ./stack.yaml ]] ; then
    alias xb='stack build'
    alias xc='stack clean'
    alias xe='stack exec --'
    alias xi='stack install'
    alias xr='stack run --'
    alias xt='stack test'
    alias xx=stack
    xl::echo 'stack loaded.'
  elif i_have cabal && bash -c 'ls *.cabal' > /dev/null 2>&1 ; then
    alias xb='cabal new-build'
    alias xc='cabal clean'
    alias xi='cabal new-install'
    alias xs='cabal list'
    alias xx=cabal
    xl::echo 'cabal loaded.'
  elif i_have etlas && false ; then
    # TODO
    alias xb='etlas build'
    alias xc='etlas clean'
    alias xr='etlas run'
    alias xx=etlas
    xl::echo 'etlas loaded.'
  elif i_have bun && [[ -e package.json ]] && [[ -e bun.lockb || -e bun.lock ]] ; then
    function xi () {
      if [[ -z $1 ]] ; then
        bun install
      else
        bun add "$@"
      fi
    }
    alias xid='bun add --dev'
    alias xr='bun run'
    alias xb='bun run build'
    alias xt='bun run test'
    alias xx=bun
    xl::echo 'bun loaded.'
  elif i_have yarn && [[ -e package.json ]] && [[ -e yarn.lock ]] ; then
    alias xi='yarn add'
    alias xid='yarn add --dev'
    alias xr='yarn run'
    alias xb='yarn build'
    alias xt='yarn test'
    alias xx=yarn
    xl::echo 'yarn loaded.'
  elif i_have npm && [[ -e package.json ]] ; then
    alias xi='npm install'
    alias xid='npm install --save-dev'
    alias xr='npm run'
    alias xb='npm run build'
    alias xx=npm
    xl::echo 'npm loaded.'
  elif i_have gradle && false ; then
    # TODO
    alias xb='gradle build'
    alias xc='gradle clean'
    alias xi='gradle install'
    alias xr='gradle run'
    alias xx=gradle
    xl::echo 'gradle loaded.'
  elif [[ -e ./gradlew ]] ; then
    alias xb='./gradlew build'
    alias xc='./gradlew clean'
    alias xi='./gradlew install'
    alias xr='./gradlew run'
    alias xx=./gradlew
    xl::echo './gradlew loaded.'
  elif [[ -e ./Gemfile ]] ; then
    # alias xb=''
    # alias xc=''
    alias xi='bundle install'
    alias xr='bundle exec ruby'
    alias xe='bundle exec'
    alias xx=bundle
    xl::echo 'bundle loaded.'
  else
    xl::echo 'No build tools found.' > /dev/stderr
    return 1
  fi
}

function xl () {
  local git_root
  git_root=$(git rev-parse --show-toplevel 2> /dev/null)
  if [[ $git_root != '' ]] ; then
    pushd "$git_root" > /dev/null || exit 1
  fi

  xl::define

  if [[ $git_root != '' ]] ; then
    popd > /dev/null || exit 1
  fi
}

# The MIT License (MIT)
#
# Copyright (c) 2025- aiya000
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
