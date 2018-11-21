let b:undo_ftplugin = 'setl ' . join([
  \ 'nonumber<',
  \ 'norelativenumber<',
  \ 'nolist<',
\ ])

setl nonumber norelativenumber nolist

nnoremap <buffer> <localleader>r i<End><C-u>:reload<CR>
nnoremap <buffer> s :<C-u>call <SID>open_say_buffer()<CR>i

function! s:open_say_buffer() abort
    let s:ghci_bufnr = winbufnr('.')
    botright new
    call s:set_default_buffer_prefs()
    call s:define_default_keymaps()
    normal! i
endfunction

function! s:set_default_buffer_prefs() abort
    setl filetype=haskell buftype=nofile noreadonly modifiable
    setl tabstop=2 shiftwidth=2 expandtab
    setl syntax=haskell
    let b:ale_enabled = v:false
    resize 5
endfunction

function! s:define_default_keymaps() abort
    nnoremap <buffer> <C-m> :<C-u>call <SID>say()<CR>
endfunction

function! s:say() abort
    let r = @"
    let z = @z
    normal! gg"zyG
    let [detail, @z, @"] = [@z, z, r]
    let detail .= "\<CR>"

    " Put detail to buffer
    execute 'buffer' s:ghci_bufnr
    put=detail
    quit
endfunction
