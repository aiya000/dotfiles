#!/bin/bash

alias vterminal=$'nvim +"lua require(\'nvim\').termopen_shell()"'

# Use NVIM_PARENT_ADDRESS (always set by Neovim) instead of NEOVIM_TERMINAL
# (NEOVIM_TERMINAL is intentionally nil on Windows filesystem to avoid a terminal-close bug)
if [[ $NVIM_PARENT_ADDRESS != '' ]] ; then
  export PAGER=cat
  alias nvim=nvim-parent-tabnew
  alias s=nvim-parent-split
  alias v=nvim-parent-vsplit
fi
