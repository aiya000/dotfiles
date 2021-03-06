let b:undo_ftplugin = 'setl ' . join([
    \ 'nonumber',
    \ 'norelativenumber',
    \ 'nolist',
\])

setl nonumber norelativenumber nolist

" Virtual
nmap <buffer><silent> <Plug>(term-weechat-open-say-buffer) :<C-u>call <SID>open_say_buffer()<CR>i

" Open say buffer
nmap <buffer> s <Plug>(term-weechat-open-say-buffer)

" In and out
nnoremap <buffer> <localleader>c A<C-e><C-u>/connect<Space>
nnoremap <buffer> <localleader>j A<C-e><C-u>/join<Space>

" Easy moves
nnoremap <buffer> J     i<C-e><C-u><M-Down><C-\><C-n>
nnoremap <buffer> K     i<C-e><C-u><M-Up><C-\><C-n>
nnoremap <buffer> <C-f> i<C-e><C-u><PageDown><C-\><C-n>
nnoremap <buffer> <C-b> i<C-e><C-u><PageUp><C-\><C-n>
nnoremap <buffer> x     i<C-e><C-u><C-x><C-\><C-n>
nnoremap <buffer> dd    i<C-e><C-u>/close<CR><C-\><C-n>
nnoremap <buffer> ghc   i<C-e><C-u>/quit<CR>


function! s:open_say_buffer() abort
    let s:weechat_bufnr = winbufnr('.')
    botright new
    call s:set_default_buffer_prefs()
    call s:define_default_keymaps()
    normal! i
endfunction

function! s:set_default_buffer_prefs() abort
    setl filetype=weechat_say buftype=nofile noreadonly modifiable
    setl tabstop=4 shiftwidth=4 expandtab
    setl syntax=markdown
    setl completefunc=github_complete#complete omnifunc=github_complete#complete
    resize 5
endfunction

function! s:define_default_keymaps() abort
    nnoremap <buffer> <C-m> :<C-u>call <SID>say()<CR>
endfunction

function! s:say() abort
    " Copy comment detail to l:detail
    let l:z = @z
    normal! gg"zyG
    let [l:detail, @z] = [@z, l:z]
    let l:detail .= "\<CR>"

    " Put detail to buffer
    execute 'buffer' s:weechat_bufnr
    put=l:detail

    normal! i
    quit
endfunction
