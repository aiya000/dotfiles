#!/bin/bash

# shellcheck disable=SC1090
source ~/.sh_generic/premise.sh

#
# This file define the aliases and functions
# You can load this from .zshrc
# and you can load .bashrc or another shell rc file, maybe
#

# Override existed name {{{

alias ls='ls --color=auto --group-directories-first'
alias mv='mv -i'
alias cp='cp -i'
alias dd='dd status=progress'
alias df='df -h'
alias du='du -h'
alias_of sudo='sudo '  # Enable aliases on sudo
alias_of mysql='mysql --pager="less -r -S -n -i -F -X"'
alias_of hoe='stack exec --silent -- hoe'
alias_of hawk='stack exec -- hawk'
i_have yaourt && alias yaourt-noconfirm='yaourt --noconfirm'
i_have say || alias say=espeak

# }}}
# Load ./aliases/** {{{

for script in ~/.sh_generic/aliases/functions/*.sh ; do
    source "$script"
done
source ~/.sh_generic/aliases/vim.sh

# }}}
# Git {{{

if i_have git ; then
    git_taking_limit=100
    alias _gr='git reset'
    alias _grh='git reset --hard'
    alias _grs='git reset --soft'
    alias _grs_HEAD-='git reset --soft HEAD~'
    alias g='git'
    alias ga='git add'
    alias gaa='git add -A'
    alias gap='git add -p'
    alias gb='git branch'
    alias gbd='git branch --delete'
    alias _gbdf='git branch --delete --force'
    alias gbm='git branch -m'
    alias gc='git commit --verbose'
    alias gcam='git commit --verbose --amend'
    alias gcm='git commit -m'
    alias gcf='git commit --fixup'
    alias gco='git checkout'
    alias gcob='git checkout -b'
    alias gco-='git-checkout-to-a-revision-menu.sh'
    alias gd='git diff'
    alias gdh='git diff HEAD~..HEAD'
    alias gds='git diff --staged'
    # shellcheck disable=SC2139
    alias gl="git log --name-only -$git_taking_limit"
    # shellcheck disable=SC2139
    alias glo="git log --oneline -$git_taking_limit"
    alias gll='glo | head'
    # shellcheck disable=SC2139
    alias glp="git log --patch -$git_taking_limit"
    # shellcheck disable=SC2139
    alias glf="git log --name-only -$git_taking_limit"
    alias gmv='git mv'
    alias gr='git rebase'
    alias gra='git rebase --abort'
    alias grc='git rebase --continue'
    alias gri='git rebase --interactive --autosquash'
    alias gri_='git rebase --interactive'
    alias grm='git rm'
    alias grmc='git rm --cached'
    alias gs='git status'
    alias gss='git stash'
    alias gsss='git stash save'
    alias gssp='git stash pop'
    function gsssp () {
        git stash push --message "$1" --patch
    }
    alias gssa='git stash apply'
    alias gssd='git stash drop'
    alias gssl='git stash list | xargs -I {} echo {}'
    alias gmt='git mergetool'
    alias gmerge='git merge --ff'
    alias gsm='git submodule'
    alias gsma='git submodule add'
    alias gsmd='git submodule deinit'
    alias gsmu='git submodule update'
    alias gsmuir='git submodule update --init --recursive'
    alias gcp='git cherry-pick'
    alias _gclean='git clean -fd'
    alias gpush='git push'
    alias gpull='git pull --rebase'
    alias gtake='git stash push --message "This is saved by alias.take" && git pull --rebase && git stash pop'
    alias greflog='git reflog'
    alias gshow='git show'
    alias gbackstep='git reset --soft HEAD~'
    alias gclone='git clone --recursive'
    alias gtag='git tag | xargs echo'
    alias gf='git fetch'
    alias gbi='git bisect'
    alias gt='git tag | xargs echo'
    alias gre='git reset --hard HEAD'
    alias gtree='git log --graph --decorate --oneline'
    alias gtree-all='git log --graph --decorate --oneline --all'
    alias gw='git worktree'
    function gwb () {
        local branch_name=$1
        git worktree add "$branch_name" -b "$branch_name"
    }
    alias gwl='git worktree list'
    alias _gwp='git worktree prune'
    unset git_taking_limit

    # Set casual user.name and user.email at local
    alias git-set-casual-name='git config --local user.name aiya000 && git config --local user.email aiya000.develop@gmail.com ; git config --local user.name ; git config --local user.email'
    alias cd-git-root='cd "$(git rev-parse --show-toplevel)"'
    alias cdg=cd-git-root
fi

# }}}
# Others {{{

alias la='ls -a --color=auto --group-directories-first'
alias ll='ls -l --color=auto --group-directories-first'
alias llh='ls -lh --color=auto --group-directories-first'
alias lla='ls -la --color=auto --group-directories-first'
# shellcheck disable=SC2139
alias e="$EDITOR"
alias m=mount
alias um=umount
alias ei=exit
alias vt=vterminal
alias nt=nterminal
alias cdp=cd-finddir
alias ki=killing-art

function dustbox () {
    # shellcheck disable=SC2068
    mv $@ ~/.backup/dustbox
}

i_have tmux && alias t=tmux && alias ta='tmux attach'
i_have rsync && alias cp-with-progress='rsync --partial --progress'
i_have watch && alias wifi-hardware-check='watch -n1 rfkill list all'
i_have nmcli && alias nmcli-connect-wifi='nmcli device wifi connect'
i_have unzip && alias unzip-cp932='unzip -O cp932'
i_have xdg-open && alias x=xdg-open

function tags-auto () {
    local make_tags tag_option_to tags_options git_root tag_dest
    make_tags=$1  # This value is exptected to 'ctags', 'haskdogs', or like it
    tag_option_to=$2
    tags_options=${*:3:($#-1)}
    git_root=$(git rev-parse --show-toplevel)

    if [[ -d $git_root/.git ]] ; then
        tag_dest=$git_root/.git/tags
    elif [[ -f $git_root/.git ]] ; then
        # If here is a submodule
        tag_dest=$git_root/tags
    else
        echo 'an undefined condition is detected! X(' > /dev/stderr
        exit 1
    fi
    eval "$make_tags ${tags_options[*]} $tag_option_to $tag_dest"
}

if i_have ctags ; then
    alias ctags-auto='tags-auto ctags -f --tag-relative=yes --recurse --sort=yes'
    alias ctags-kotlin-auto='ctags-auto --exclude=\\\*.java'
fi

if i_have hasktags ; then
    alias hasktags-casual='hasktags . --ignore-close-implementation --tags-absolute --ctags -f'
    alias hasktags-auto='tags-auto hasktags-casual'
fi

if i_have haskdogs ; then
    function haskdogs-casual () {
        haskdogs --hasktags-args "--ignore-close-implementation --tags-absolute --ctags --file=${1}"
    }
    alias haskdogs-auto='tags-auto haskdogs-casual'
fi

if i_have stack ; then
    alias sb='stack build'
    alias se='stack exec --'
    alias st='stack test'
    alias si='stack install'
    alias sc='stack clean'
    alias sbp=stack-build-profile

    alias srunghc='stack runghc --'
    alias sghci='stack ghci --'
    alias shaddock-gen='stack haddock .'
    alias shaddock-gen-open='stack haddock --open .'
    function stack-new-default() {
        stack new "$1" simple
    }
    alias stack-build-profile='stack build --profile'
    alias cabal-sdit='stack exec -- cabal sdist'
    function cabal-upload() {
        stack exec -- cabal upload "$1"
    }
    alias make-new-package-yaml='cp ~/.dotfiles/Files/package.yaml .'
fi

if i_have etlas ; then
    alias et=etlas
    alias etb='etlas build'
    alias etr='etlas run'
    alias etc='etlas clean'
fi

if i_have idris ; then
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
fi

if i_have npm ; then
    alias nr='npm run'
fi

if i_have docker ; then
    alias docker-rm-all-containers='sudo docker rm `sudo docker ps -a -q`'
    alias dps='docker ps'
    alias da=docker-attach-menu.sh
    alias dkill='docker kill'
    alias dkilla=docker-kill-menu.sh
    alias drm='docker rm'
    alias drmi='docker rmi'
fi

# shellcheck disable=SC2139
alias mount4u.ntfs="sudo mount -o user=$(whoami),uid=$(id -u),gid=$(id -g),iocharset=utf8"
alias mount4u.vfat=mount4u.ntfs
alias mount4u.ext2='sudo mount -o iocharset=utf8'
alias mount4u.ext3=mount4u.ext2
alias mount4u.ext4=mount4u.ext2

# TODO: Add the completion
function cat-which () {
    cat "$(which "$1")"
}

function virtualenv-activate () {
    if [[ -f ./.venv/bin/activate ]] ; then
        # shellcheck disable=SC1091
        . ./.venv/bin/activate
    else
        echo './.venv/bin/activate was not found, please load it yourself...' > /dev/stderr
        return 1
    fi
}

# }}}

export PATH=$PATH:~/.sh_generic/bin
