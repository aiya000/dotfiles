#!/bin/bash

function aliases::elem () {
	for unit in $1 ; do
		if [ "$unit" = "$1" ] ; then
			echo 1
			return
		fi
	done
	echo 0
}

function load-my-env () {
	local installed_plugins=( \
		haskell-stack \
		cabal \
		pkgsrc \
		rbenv \
		virtualenv \
	)
	local target_name="$1"

	if [ "$1" != '' -a $(aliases::elem "$installed_plugins" "$target_name") -ne 1 ] ; then
		echo "You may haven't $target_name"
		return 1
	fi

	case "$target_name" in
	haskell-stack)
		if [ -d ~/.stack ] ; then
			PATH=$PATH:$HOME/.stack/programs/x86_64-linux/ghc-7.8.4/bin
		fi
		;;
	cabal)
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
	rbenv)
		if [ -d ~/.rbenv ] ; then
			PATH=$PATH:$HOME/.rbenv/bin
			PATH=$PATH:$HOME/.rbenv/versions/$(cat ~/.rbenv/version)/bin
			eval "$($HOME/.rbenv/bin/rbenv init -)"
		fi
		if [ -d ~/.rbenv/plugins/ruby-build/bin ] ; then
			PATH=$PATH:$HOME/.rbenv/plugins/ruby-build/bin
		fi
		;;
	virtualenv)
		local x="$(which virtualenvwrapper.sh)"
		if [ -n "$x" ] ; then
			export WORKON_HOME=$HOME/.virtualenvs
			source $x
		fi
		;;
	*)
		load-my-env haskell-stack
		load-my-env cabal
		load-my-env pkgsrc
		load-my-env rbenv
		load-my-env virtualenv
		;;
	esac
}
