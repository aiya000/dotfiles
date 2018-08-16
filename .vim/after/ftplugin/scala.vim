let b:undo_ftplugin = 'setl ' . join([
    \ 'tabstop<',
    \ 'shiftwidth<',
    \ 'expandtab<',
    \ 'commentstring<',
\ ])

let &commentstring = ' /*%s*/'
setl tabstop=2 shiftwidth=2 expandtab

nnoremap <buffer><silent> <localleader><localleader>n :<C-u>NailgunScalafmtStart<CR>

command! -bar NailgunScalafmtStart call s:open_ng_scalafmt()

function! s:open_ng_scalafmt() abort
    call vimrc#open_terminal_as('none', 'horizontal', 'scalafmt_ng', v:false)
    hide
    echo 'scalafmt_ng started'
endfunction

augroup FtpluginScala
    autocmd!
    "autocmd BufWritePre *.scala Autoformat
augroup END
