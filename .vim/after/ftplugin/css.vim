let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<'
\ ])

setl tabstop=2 shiftwidth=2 expandtab

" Disable at first
if !exists('b:ale_enabled')
  let b:ale_enabled = v:false
endif
