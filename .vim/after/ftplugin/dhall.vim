let b:undo_ftplugin = 'setl ' . join([
  \ 'ts<',
  \ 'sw<',
  \ 'et<',
  \ 'commentstring<',
\ ])

setl ts=4 sw=4 et conceallevel=0
let &commentstring = ' -- %s'

augroup FtpluginDhall
  autocmd!
  autocmd BufWritePre *.dhall Autoformat
augroup END
