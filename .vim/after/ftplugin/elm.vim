let b:undo_ftplugin = 'setl ' . join([
    \ 'ts<',
    \ 'sw<',
    \ 'et<',
    \ 'conceallevel<'
\])

setl ts=2 sw=2 et conceallevel=0
let &commentstring = ' -- %s'

"nnoremap <buffer><silent> <localleader>r :<C-u>ElmMake<CR>
nnoremap <buffer><silent> <localleader>S :<C-u>Aref elm-search <C-r>=expand('<cword>')<CR><CR>
nnoremap <buffer><silent> <localleader>o :<C-u>call vimrc#open_terminal_as('term-elm-repl', 'vertical', 'elm repl')<CR>

let g:ftplugin_elm_elm_make_all_cmd =
    \   'elm-make $(find . -type f -regex "^.+\\.elm$" | grep -v "elm-stuff") --warn --output /tmp/vim-terminal-elm-all.html'
    \ . '&& firefox /tmp/vim-terminal-elm-all.html'
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('scratch', 'horizontal', g:ftplugin_elm_elm_make_all_cmd)<CR>

augroup FtpluginElm
    autocmd!
    autocmd BufWritePre *.elm ElmFormat
augroup END
