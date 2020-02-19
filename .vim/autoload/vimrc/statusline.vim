scriptencoding utf-8
scriptversion 3

function vimrc#statusline#is_yankround_active() abort
  return dein#is_sourced('yankround.vim') && yankround#is_active()
endfunction
