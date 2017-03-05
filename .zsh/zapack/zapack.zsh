#!/usr/bin/env zsh

# Execute `git submodule update --init` for a repository
function git_submudule_pull () {
	repo="$1"

	# Save directory stack
	cur_dir=$(pwd)
	prev_dir=$(cd - > /dev/null 2>&1; pwd; cd - > /dev/null 2>&1)

	#NOTE: Support recursive pull if it is needed
	# Pull submodules
	cd "$repo" > /dev/null
	git submodule update --init --recursive

	# Resume directory stack
	cd "$prev_dir"
	cd "$cur_dir"
}

# Get repository name of the directory
# Ex: $1='.zsh/zapack/repos/zapack.zsh' ==> zapack.zsh
function get_repo_name () {
	repo_dir="$1"
	echo "$repo_dir" | sed -r 's;^(.*/)*;;'
}

# Load a plugin's init script.
# For example:
#   zsh-shell-kawaii/zsh-shell-kawaii.zsh (the case of {name}.zsh)
#   auto-fu.zsh/auto-fu.zsh (the case of {name})
function load_init_script_if_available () {
	repo="$1"
	repo_name=$(get_repo_name "$repo")

	if [ -f "$repo/$repo_name.zsh" ] ; then
		source "$repo/$repo_name.zsh"
	elif [ -f "$repo/$repo_name" ] ; then
		source "$repo/$repo_name"
	fi
}

# Add repos to runtime paths
for repo in $(ls -d "$ZAPACK_HOME"/repos/*) ; do
	PATH=$PATH:$repo
	git_submudule_pull "$repo"
	load_init_script_if_available "$repo"
done
