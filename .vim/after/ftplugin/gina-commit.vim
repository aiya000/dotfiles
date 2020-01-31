let b:undo_ftplugin = 'setl ' . join([
  \ 'number<',
  \ 'relativenumber<',
  \ 'syntax=gina-commit<',
  \ 'filetype=markdown<',
\ ])

setl number relativenumber syntax=gina-commit filetype=markdown

normal! gg
