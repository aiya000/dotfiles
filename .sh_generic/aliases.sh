#!/bin/bash

# shellcheck disable=SC1090
source ~/.dotfiles/.sh_generic/premise.sh

#
# This file define the aliases and functions
# You can load this from .zshrc
# and you can load .bashrc or another shell rc file, maybe
#

# Override existed name {{{

alias cp='cp -i'
alias dd='dd status=progress'
alias df='df -h'
alias dmesg='dmesg --ctime'
alias free='free -h'
alias ls='ls --color=auto --group-directories-first'
alias mv='mv -i'
alias rm=dust  # no more cry
alias sl=ls

function du-sort () {
    local paths=${1:-.}
    du -h -d 1 "$paths" | sort -h
}

# NOTE: Who did define the original - ?
function - () {
    # shellcheck disable=SC2164
    cd -
}

alias_of sudo='sudo '  # Enable aliases on sudo
alias_of mysql='mysql --pager="less -r -S -n -i -F -X"'

i_have say || alias say=espeak

# }}}
# Load ./aliases/** {{{

for script in ~/.sh_generic/aliases/functions/*.sh ; do
    source "$script"
done
source ~/.sh_generic/aliases/build-tools.sh
source ~/.sh_generic/aliases/os-package-managers.sh
source ~/.sh_generic/aliases/vim.sh

# }}}
# Git {{{

if i_have git ; then
    git_taking_limit=100
    alias _gr='git reset'
    alias _grh='git reset --hard'
    alias _grh~='git reset --hard HEAD~'
    alias _grh~~='git reset --hard HEAD~~'
    alias _grh~~~='git reset --hard HEAD~~~'
    alias _grh~~~~='git reset --hard HEAD~~~~'
    alias _grs='git reset --soft'
    alias _grs~='git reset --soft HEAD~'
    alias _grs~~='git reset --soft HEAD~~'
    alias _grs~~~='git reset --soft HEAD~~~'
    alias _grs~~~~='git reset --soft HEAD~~~~'
    alias ga='git add'
    alias gaa='git add -A'
    alias gap='git add -p'
    alias gb='git branch'
    alias gbd='git branch --delete'
    alias _gbd='git branch -D'
    alias _gbdf='git branch --delete --force'
    alias gbm='git branch -m'
    alias gc='git commit --verbose'
    alias gcam='git commit --verbose --amend'
    alias gcm='git commit -m'
    alias gcf='git commit --fixup'
    alias gco='git checkout'
    alias gsw='git switch'
    alias gswc='git switch --create'
    alias gswd='git switch --detach'
    alias gres='git restore'
    alias gd='git diff'
    alias gdh='git diff HEAD~..HEAD'
    alias gds='git diff --staged'
    alias gdst0='git diff stash@{0}'
    alias gdst1='git diff stash@{1}'
    alias gdst2='git diff stash@{2}'
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
    alias gri~='git rebase --interactive --autosquash HEAD~'
    alias gri~~='git rebase --interactive --autosquash HEAD~~'
    alias gri~~~='git rebase --interactive --autosquash HEAD~~~'
    alias gri~~~~='git rebase --interactive --autosquash HEAD~~~~'
    alias gri~~~~~='git rebase --interactive --autosquash HEAD~~~~~'
    alias gri~~~~~~='git rebase --interactive --autosquash HEAD~~~~~~'
    alias gri~~~~~~~='git rebase --interactive --autosquash HEAD~~~~~~~'
    alias gri~~~~~~~~='git rebase --interactive --autosquash HEAD~~~~~~~~'
    alias gri~~~~~~~~~='git rebase --interactive --autosquash HEAD~~~~~~~~~'
    alias gri~~~~~~~~~~='git rebase --interactive --autosquash HEAD~~~~~~~~~~'
    alias gr-onto='git rebase --onto'
    alias grev='git revert'
    alias grev~='git revert HEAD~'
    alias grev~~='git revert HEAD~~'
    alias grev~~~='git revert HEAD~~~'
    alias grev~~~~='git revert HEAD~~~~'
    alias grev~~~~~='git revert HEAD~~~~~'
    alias grev~~~~~~='git revert HEAD~~~~~~'
    alias grev~~~~~~~='git revert HEAD~~~~~~~'
    alias grev~~~~~~~~='git revert HEAD~~~~~~~~'
    alias grev~~~~~~~~~='git revert HEAD~~~~~~~~~'
    alias grev~~~~~~~~~~='git revert HEAD~~~~~~~~~~'
    alias grm='git rm'
    alias grmc='git rm --cached'
    alias gs='git status'
    alias gss='git stash'
    alias gsssm='git stash push -m'
    alias gssp='git stash pop'
    function gsssp () {
        git stash push --message "$1" --patch
    }
    alias gssa='git stash apply'
    alias gssd='git stash drop'
    alias gssl='git stash list'
    alias gmt='git mergetool'
    alias gmerge='git merge --no-ff'
    alias gsm='git submodule'
    alias gsma='git submodule add'
    alias gsmd='git submodule deinit'
    alias gsmu='git submodule update'
    alias gsmui='git submodule update --init'
    alias gsmuir='git submodule update --init --recursive'
    alias gch='git cherry-pick'
    alias _gclean='git clean -fd'
    alias gp='git push'
    alias gpu='git push -u'
    alias gpuo='git push -u origin'
    alias gpua='git push -u aiya000'
    alias gpf='git push --force-with-lease'
    alias gpull='git pull --rebase'
    alias greflog='git reflog'
    alias gshow='git show'
    alias gclone='git clone --recursive'
    alias gtag-list='git tag | xargs echo'
    alias gtag-add='git tag -a'
    alias gf='git fetch'
    alias gfo='git fetch origin'
    alias gbi='git bisect'
    alias gtag='git tag | xargs echo'
    alias gtree='git log --graph --decorate --oneline'
    alias gtree-all='git log --graph --decorate --oneline --all'
    alias gw='git worktree'
    alias gwa='git worktree add'

    function git-branch-name () {
        git branch 2> /dev/null | grep '\*\s.*' | awk '{print $2}'
    }

    function git-log-unpushed () {
        git log "$@" "origin/$(git-branch-name)..HEAD"
    }

    function gwb () {
        local branch_name=$1
        git worktree add "$branch_name" -b "$branch_name"
    }

    alias gwl='git worktree list'
    alias gwp='git worktree prune'
    alias gw-erase-removed='git worktree prune'
    unset git_taking_limit

    function git-push-wip () {
        local wip_name=wip-current-unique-unique-yazawa-nico
        git branch -D "$wip_name"
        git checkout -b "$wip_name"
        git add -A
        git commit -m "Today's WIP"
        git push -f origin "$wip_name"
        git checkout -
        git cherry-pick "$wip_name"
    }
    alias gp-wip=git-push-wip
    alias gpw=gp-wip

    function _gre () {
        echo -n "(at '$(git rev-parse --show-toplevel)') Do you really force reset the git index tree? (y/n)"
        if read -rq _  ; then
            echo
            git reset --hard HEAD
            echo 'done'
        fi
    }

    alias ginit='git init'

    function github-change-remote-from-git-to-https () {
        local remote https_url
        remote=${1:-origin}
        https_url=$(git remote get-url "$remote" \
            | sed -r 's/git@([^:]+):([^\/]+)\/(.*)/https:\/\/\1\/\2\/\3/' \
            | sed -r 's/\.git$//')
        git remote set-url "$remote" "$https_url"
        git remote get-url "$remote"
    }

    # Set casual user.name and user.email at local
    alias git-set-casual-name='git config --local user.name aiya000 && git config --local user.email aiya000.develop@gmail.com ; git config --local user.name ; git config --local user.email'
    alias cd-git-root='cd "$(git rev-parse --show-toplevel)"'
    alias cdg=cd-git-root
fi

# }}}
# Docker {{{

if i_have docker ; then
    alias d=docker
    alias da=docker-attach-menu.sh
    alias dis='docker images'
    alias dki=docker-kill-menu.sh
    alias dkill='docker kill'
    alias dps='docker ps'
    alias drm='docker rm'
    alias drmi='docker rmi'
    alias drun='docker run'

    function docker-run-bash () {
        local image_name
        image_name=$1
        docker run -it "$image_name" /bin/bash
    }
fi


# }}}
# Others {{{

alias la='ls -a --color=auto --group-directories-first'
alias ll='ls -l --color=auto --group-directories-first'
alias llh='ls -lh --color=auto --group-directories-first'
alias lla='ls -la --color=auto --group-directories-first'

# shellcheck disable=SC2139
alias e="$EDITOR"
alias eS='ls -fA $VIM_SESSION | peco | xargs -I {} vim -S $VIM_SESSION/{}'
alias g=gvim
alias gS='ls -fA $VIM_SESSION | peco | xargs -I {} gvim -S $VIM_SESSION/{}'

alias m=mount
alias t=vterminal
alias um=umount
alias ei=exit
alias cdp=cd-finddir
alias ki=killing-art

# alias ..='cd ../'
# alias ...='cd ../../'
# alias ....='cd ../../../'
# ...
function aliases::define_cd_to_parents () {
    local name
    for (( i = 2; i <= 20; ++i )) ; do
        name=$(eval "printf '.%.0s' {1..$i}")
        dir=$(eval "printf '../%.0s' {2..$i}")
        eval "alias $name='cd $dir'"
    done
}
aliases::define_cd_to_parents

# shellcheck disable=SC2139
alias e-current-session="vim-current-session $EDITOR"
# shellcheck disable=SC2139
alias g-current-session="vim-current-session $MY_GUI_EDITOR"

i_have tmux && alias tm=tmux && alias ta='tmux attach'
i_have rsync && alias cp-with-progress='rsync --partial --progress'
i_have watch && alias wifi-hardware-check='watch -n1 rfkill list all'
i_have nmcli && alias nmcli-connect-wifi='nmcli device wifi connect'
i_have unzip && alias unzip-cp932='unzip -O cp932'
i_have xdg-open && alias x=xdg-open

if i_have ctags ; then
    function ctags-auto () {
        local root
        root=$(git rev-parse --show-toplevel 2> /dev/null || pwd)
        dest=$([[ -d $root/.git ]] && echo "$root/.git/tags" || echo "$root/tags")
        ctags -f "$dest" --tag-relative=never --recurse --sort=yes "$@"
    }

    alias ctags-kotlin-auto="ctags-auto '--exclude=*.java' '--exclude=*.html' '--exclude=*.css'"
    alias ctags-typescript-auto="ctags-auto '--exclude=*.js' '--exclude=*.json'"
fi

if i_have yay ; then
    alias yay='yay --color always'
fi

if i_have stack ; then
    alias si='stack install'
    alias srunghc='stack runghc --'
    alias sghci='stack ghci --'
    alias stack-haddock-gen='stack haddock .'
    alias shaddock-gen='stack haddock .'
    alias shaddock-gen-open='stack haddock --open .'
    function stack-new-default() {
        stack new "$1" simple
    }
    alias stack-build-profile='stack build --profile'
    alias make-new-package-yaml='cp ~/.dotfiles/Files/package.yaml .'

    alias cabal-sdit='stack exec -- cabal sdist'
    function cabal-upload() {
        stack exec -- cabal upload "$1"
    }
fi

if i_have cabal ; then
    alias ci='cabal new-install'
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

if [[ -e ./gradlew ]] ; then
    alias grwj='./gradlew jar'
fi

if i_have krita ; then
    alias kra=krita
fi

if i_have draw.io ; then
    alias drio=draw.io

    # NOTE: Before, draw.io exports foo.xml.png for foo.xml, but the backward compatility broke.
    function drio-export-png () {
        local xml_name
        xml_name=$1

        draw.io --embed-diagram --export --format png "$xml_name" --output "${xml_name}.png"
    }
fi

# shellcheck disable=SC2139
alias mount4u.ntfs="sudo mount -o user=$(whoami),uid=$(id -u),gid=$(id -g),iocharset=utf8"
alias mount4u.vfat=mount4u.ntfs
alias mount4u.ext2='sudo mount -o iocharset=utf8'
alias mount4u.ext3=mount4u.ext2
alias mount4u.ext4=mount4u.ext2

function virtualenv-activate () {
    if [[ -f ./.venv/bin/activate ]] ; then
        # shellcheck disable=SC1091
        . ./.venv/bin/activate
    else
        echo './.venv/bin/activate was not found, please load it yourself...' > /dev/stderr
        return 1
    fi
}

function grepall () {
    find . -type f -print0 | xargs -0 -I {} bash -c "grep $* {} && (echo '{}' ; echo '- - - - - - - - - -')"
}

# }}}

export PATH=$PATH:~/.sh_generic/bin
