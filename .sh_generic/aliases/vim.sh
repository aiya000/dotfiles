#!/bin/bash
# This makes the aliases with both vim and neovim

# Vim
alias vi='vim -u NONE --noplugin'
alias vime='vim -c ":bufdo tab split" +q'

# :SessionSaveInGitBranch compatibled command. (it is defined in dotfiles/.vim/plugin/vimrc.vim)
# Open the session which is associated the current git branch
function vim-current-session () {
    local sessions_dir=~/.backup/vim-backup/session repo_name session_name editor branch_name branch_name_ branch_name__

    editor=$([[ -n $1 ]] && echo "$1" || echo vim)
    branch_name="$(git branch 2> /dev/null | sort | tail -1 | awk '{print $2}')"
    # shellcheck disable=SC2181
    if [ "$?" -ne 0 ] ; then
        echo 'Is here the git branch ?' > /dev/stderr
        exit
    fi

    repo_name="$(git rev-parse --show-toplevel | sed -r 's;.*/(.*);\1;')"
    branch_name_="$(echo "$branch_name" | sed -r 's;/;-;g')"
    branch_name__="$(echo "$branch_name_" | sed -r 's;#;-;g')"

    session_name="${repo_name}-${branch_name__}.vim"
    "$editor" -S "$sessions_dir/$session_name"
}

# NeoVim
alias nvime='nvim -c ":bufdo tab split" +q'

## :terminal
alias vterminal='vim +"call vimrc#open_terminal_as(\"term-shell\", \"stay\", &shell)"'
alias gvterminal='gvim +"call vimrc#open_terminal_as(\"term-shell\", \"stay\", &shell)"'
alias nterminal='nvim +"call vimrc#open_terminal_as(\"term-shell\", \"stay\", &shell)"'

function vim-fix-plugins-for-dein-update () {
    local plugin_names plugin_dir
    plugin_names=("$@")
    for name in "${plugin_names[@]}" ; do
        cd ~/.vim/bundle/repos || exit
        plugin_dir=$(bash -c "find . -type d -name '$name'")
        echo "$name"
        cd "$plugin_dir" && git checkout master
    done
}

function vim-get-latest-git-hashes () {
    local plugin_names plugin_dir
    plugin_names=("$@")
    for name in "${plugin_names[@]}" ; do
        cd ~/.vim/bundle/repos || exit 1
        plugin_dir=$(bash -c "find . -type d -name '$name'")
        cd "$plugin_dir" || exit 1
        echo "$name: $(git log | head -1 | awk '{print $2}')"
    done
}

# See .vim/autoload/vimrc/autocmd.vim
function vimterm-quote-args() {
    for a in "$@" ; do
        echo ", \"$a\""
    done
}

function vimterm-open-parent-vim() {
    echo -e "\e]51;[\"call\", \"Tapi_Tabnew\", [\"$PWD\" $(vimterm-quote-args "$@")]]\x07"
}

# If I'm on a shell on Vim
if [[ $I_AM_ON_VIM -eq 1 ]] ; then
    alias vim=vimterm-open-parent-vim
fi
