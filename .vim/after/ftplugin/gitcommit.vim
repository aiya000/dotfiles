let b:undo_ftplugin = 'setl ' .. join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'textwidth<',
\ ])
setl tabstop=4 shiftwidth=4 textwidth=0

normal! gg
