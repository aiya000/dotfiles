" Enable neocomplete or deoplete
function! vimrc#autocmd#enable_input_completion() abort " {{{
    if has('nvim')
        call deoplete#enable()
    else
        NeoCompleteUnlock
    endif
endfunction " }}}
