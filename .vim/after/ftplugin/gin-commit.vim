let b:undo_ftplugin = 'setl ' .. join([
  \ 'number<',
  \ 'relativenumber<',
\ ])
setl number relativenumber

normal! gg
