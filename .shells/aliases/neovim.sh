#!/bin/bash

alias vterminal=$'nvim +"lua require(\'nvim\').termopen_shell()"'

# See ~/.config/nvim/lua/helper.lua for $NEOVIM_TERMINAL
if [[ $NEOVIM_TERMINAL != '' ]] ; then
  export PAGER=cat
  alias nvim=nvim-parent-tabnew
  alias s=nvim-parent-split
  alias v=nvim-parent-vsplit
fi
