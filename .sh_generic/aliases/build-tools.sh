#!/bin/bash

function xl::echo () {
  echo "build-tools.sh>> $1"
}

# Defines aliases "x[bceirst]" at a runtime
function xl () {
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
