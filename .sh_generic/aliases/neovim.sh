#!/bin/bash

alias vterminal=$'nvim +"lua require(\'nvim\').termopen_shell()"'

# See ~/.config/nvim/lua/helper.lua for $NEOVIM_TERMINAL
if [[ $NEOVIM_TERMINAL != '' ]] ; then
  export PAGER=cat
fi
