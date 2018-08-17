let b:undo_ftplugin = 'setl ' . join([
    \ 'tabstop<',
    \ 'shiftwidth<',
    \ 'expandtab<',
    \ 'commentstring<',
\ ])

let &commentstring = ' /*%s*/'
setl tabstop=2 shiftwidth=2 expandtab
let &errorformat = '[%t%.%#] %f:%l:%m'

nnoremap <buffer><silent> <localleader><localleader>w :<C-u>QuickfixRunSbtCompileWatch<CR>
nnoremap <buffer><silent> <localleader><localleader>W :<C-u>QuickfixStopSbtCompileWatch<CR>

augroup FtpluginScala
    autocmd!
    " Clear past results for :QuickfixRunSbtCompileWatch
    autocmd BufWritePost *.scala CClear
augroup END
