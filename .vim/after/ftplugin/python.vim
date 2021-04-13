let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<',
\ ])

setl ts=4 sw=4 et

augroup FtpluginPython
  autocmd!
  autocmd BufWritePre * LspDocumentFormatSync
augroup END
