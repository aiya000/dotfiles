nnoremap <buffer> <C-j> :<C-u>call gina#action#call('diff:preview:bottom')<CR>
nnoremap <buffer> o     :<C-u>call gina#action#call('edit')<CR>

nnoremap <buffer><silent> ? :<C-u>call <SID>show_help_in_scratch_buffer()<CR>
nnoremap <buffer><silent> <C-r> :<C-u>Gina status<CR>
nnoremap <buffer><silent> Q  :<C-u>bdelete!<CR>
nnoremap <buffer><silent> cc :<C-u>Gina commit --verbose<CR>
nnoremap <buffer><silent> ca :<C-u>Gina commit --verbose --amend<CR>
nnoremap <buffer><silent> gf :<C-u>e <cfile><CR>

function! s:show_help_in_scratch_buffer() abort
    let l:a = execute('normal ' . "\<Plug>(gina-builtin-help)")
    new
    setl buftype=nofile noreadonly modifiable ft=scratch
    put=l:a
    normal! ggdddd0
endfunction
