#!/usr/bin/env zsh

function zshrc::prompt::main () {
    # A maid represents a status of the exit code
    local feature="%(?.%{${fg_bold[green]}%}.%{${fg_bold[blue]}%})%(?!(*^-^)!(;^-^%))%{${reset_color}%}"
    local current_dir="%{$fg[yellow]%}%~%{$reset_color%}"

    export PROMPT="${feature} ${current_dir}%{$reset_color%} | $(zshrc::prompt::sub_status)
%{$fg[cyan]%}>>> %{$reset_color%}"
}

function zshrc::prompt::sub_status () {
    function get_git_changes () {
        # Subtract a head line minute
        local changes=$(( $(git status --short 2> /dev/null | wc -l) - 1 ))
        if [ "$changes" -ge 1 ] ; then
            echo "%{$bg[white]$fg[black]%}[change:${changes}]%{$reset_color%}"
        fi
    }

    function get_git_commits () {
        local commits=$(git status --short 2> /dev/null | head -1 | grep -o '\[.*\]')
        if [ "$?" -eq 0 ] ; then
            echo "%{$bg[red]$fg[black]%}${commits}%{$reset_color%}"
        fi
    }

    function get_git_stash_status () {
        local item_num=$({git stash list 2> /dev/null || echo -n ''} | wc -l)
        if [[ $item_num -ge 1 ]] ; then
            echo "%{$bg[cyan]$fg[black]%}[stash:${item_num}]%{$reset_color%}"
        fi
    }

    function get_git_branch_name () {
        local branch=$(git branch --show-current 2> /dev/null)
        if [ "$?" -ne 0 ] ; then
            echo '[NO REPO]'
            exit
        fi
        echo "%{$bg[green]$fg[black]%}[${branch}]%{$reset_color%}"
    }

    function get_zle_mode () {
        local expected_normal_mode='vicmd'
        local keymap_name="$(echo $KEYMAP | sed -r 's/^(.)/\U\1/')"
        local color ; [[ $KEYMAP == $expected_normal_mode ]] && color=red || color=blue
        echo "%{$bg[${color}]%}[${keymap_name}]%{$reset_color%}"
    }

    function get_virtualenv_availability () {
        if [ -n "$VIRTUAL_ENV" ] ; then
            local env_name=$(echo "$VIRTUAL_ENV" | sed -r 's;^/.*/(.*)/\.venv$;\1;')
            echo "%{$bg[yellow]$fg[black]%}[${env_name}]%{$reset_color%}"
        elif [ -d "$(pwd)/.venv" ] ; then
            echo "%{$bg[red]$fg[black]%}[./.venv was found]%{$reset_color%}"
        fi
    }

    local git_statuses
    if [[ $ZSHRC_PROMPT_GIT_STATUS_DISABLE -ne 0 ]] ; then
        git_statuses='[GitStatus Disabled]'
    else
        git_statuses=$(get_git_changes)$(get_git_commits)$(get_git_stash_status)$(get_git_branch_name)
    fi

    echo "${git_statuses}$(get_zle_mode)$(get_virtualenv_availability)"
}

# Run once to start up
zshrc::prompt::main
