#!/bin/bash
export BROWSER=open

alias x=open
alias ps='ps -e -o pid,lstart,command | sort -k2,6'  # Show pid, started time, and the command. Sort by start time
alias docker-force-remove-containers-data='rm -rf ~/Library/Containers/com.docker.docker/Data/*'

i_have gsed && alias sed=gsed
