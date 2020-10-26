#!/bin/bash
export BROWSER=open

alias x=open
alias poweroff='sudo shutdown -h now'
alias sudoedit='sudo -e'
alias docker-force-remove-containers-data='rm -rf ~/Library/Containers/com.docker.docker/Data/*'

i_have htop && alias top=htop
i_have gsed && alias sed=gsed
