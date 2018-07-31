#!/bin/bash
# This makes the aliases with both vim and neovim

# Vim
alias vi='vim -u NONE --noplugin'
alias vime='vim -c ":bufdo tab split" +q'

# :SessionSaveInGitBranch compatibled command. (it is defined in dotfiles/.vim/plugin/vimrc.vim)
# Open the session which is associated the current git branch
function vim-current-session () {
    local sessions_dir=~/.backup/vim_backup/session repo_name session_name editor branch_name branch_name_ branch_name__

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
    "$editor" -S "$sessions_dir/$session_name" +'bufdo CdGitRoot' +'execute "normal!" "\<C-o>"'
}

function vimman () {
    local editor
    editor=$([[ -n $1 ]] && echo "$1" || echo vim)
    "$editor" -c "Man ${1}" +only
}

# NeoVim
alias nvime='nvim -c ":bufdo tab split" +q'
alias nvim-current-session='vim-current-session nvim'
alias nvimman='vimman nvim'

## Twitter
alias twitter='vim +TweetVimHomeTimeline'
alias tweet='vim +TweetVimSay'
alias twitter-public='vim +TwitterPublic'
alias tweet-public='vim +TweetPublic'
function twitter-usertimeline() {
    vim -c "TweetVimUserTimeline ${1}"
}

function tweet-say() {
    yes | vim -c "TweetVimCommandSay ${1}" +q
}

function tweet-public-say() {
    yes | vim \
        -c "TweetVimSwitchAccount public_ai000ya" \
        -c "TweetVimCommandSay ${1}" \
        +q
}

## :terminal
alias vterminal='vim +terminal +"setf term-shell" +only'
alias nterminal='nvim +terminal +"setf term-shell"'
