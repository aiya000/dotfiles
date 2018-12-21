let b:undo_ftplugin = 'setl ' . join([
  \ 'expandtab<',
  \ 'shiftwidth<',
  \ 'tabstop<',
\ ])

setl tabstop=4 shiftwidth=4 expandtab

nnoremap <buffer> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('', 'horizontal', 'yarn build', v:false)<CR>

if !get(g:vimrc, 'language_client_neovim', {'enabled': v:false}).enabled
  LanguageClientStart
endif
