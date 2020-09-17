#!/bin/bash
export BROWSER=open

alias x=open
alias poweroff='sudo shutdown -h now'
alias sudoedit='sudo -e'

i_have htop && alias top=htop
i_have gsed && alias sed=gsed
