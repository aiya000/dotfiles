let s:Job = vital#vimrc#new().import('System.Job')

let b:undo_ftplugin = 'setl ' . join([
    \ 'ts<',
    \ 'sw<',
    \ 'et<',
    \ 'conceallevel<',
\])

setl ts=2 sw=2 et conceallevel=0
let &commentstring = ' -- %s'

function! s:idris(cmd) abort
    let all_output = []
    function! s:aggregate_stdout(_, data, __) abort closure
        call add(all_output, a:data[0])
    endfunction
    function! s:aggregate_stderr(_, data, __) abort closure
        call add(all_output, a:data[0])
    endfunction

    function! s:see_result(_, __, ___) abort closure
        let lines = join(all_output, "\n")

        if substitute(lines, '\(\n\|\r\)', '', 'g') ==# ''
            echo printf('no result of "%s" is existent', a:cmd)
            return
        elseif len(all_output) <= 2
            echo a:cmd . '> ' . lines
            return
        endif

        "TODO: Use :pedit
        new | setl noswapfile buftype=nofile filetype=ftplugin-idris
        setl modifiable noreadonly

        1put!=lines
        normal! G"_ddgg

        setl nomodifiable
    endfunction

    call s:Job.start(printf('idris --client "%s"', a:cmd), {
        \ 'on_stdout': function('s:aggregate_stdout'),
        \ 'on_stderr': function('s:aggregate_stdout'),
        \ 'on_exit':   function('s:see_result'),
    \})
endfunction

function! s:with_cword(f, cmd) abort
    let cword = expand('<cword>')
    call a:f(printf('%s %s', a:cmd, cword))
endfunction

function! s:idris_cword(cmd) abort
    call s:with_cword(function('s:idris'), a:cmd)
endfunction

function! s:idris_clnum_word(cmd, word) abort
    let [_, lnum, __, ___, ____] = getcurpos()
    call s:idris(a:cmd . ' ' . lnum . ' ' . a:word)
endfunction

nnoremap <buffer><silent> <localleader>o :<C-u>vsp<CR>:IdrisRepl <C-r>=expand('%:p')<CR><CR>
nnoremap <buffer><silent> <localleader>R :<C-u>call <SID>idris_cword(':reload')<CR>
nnoremap <buffer><silent> <localleader>T :<C-u>call <SID>idris_cword(':type')<CR>
nnoremap <buffer><silent> <localleader>d :<C-u>call <SID>idris_cword(':doc')<CR>
nnoremap <buffer><silent> <localleader>m :<C-u>call <SID>idris_cword(':missing')<CR>
nnoremap <buffer><silent> <localleader>p :<C-u>call <SID>idris_clnum_word(':proofsearch', '<C-r>=expand('<cword>')[1:]<CR>')<CR>
