#!/bin/bash
branches=$(git branch --list --all | cut -d ' ' -f 2- | awk '{print $1}')
tags=$(git tag)

git checkout "$(echo "$branches" "$tags" | fzf)"
