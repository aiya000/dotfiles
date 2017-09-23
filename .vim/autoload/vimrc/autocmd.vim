" Enable neocomplete or deoplete
function! vimrc#autocmd#enable_input_completion() abort " {{{
    if has('nvim')
        call deoplete#enable()
    else
        NeoCompleteUnlock
    endif
endfunction " }}}

" Yank posted gist to clipboard
function! vimrc#autocmd#yank_gista_posted_url() abort " {{{
    let l:gistid = g:gista#avars.gistid
    execute printf('Gista browse --yank --gistid=%s', l:gistid)
    let @+ = @"
endfunction " }}}

" Auto set cursor position in the file
function! vimrc#autocmd#visit_past_position() " {{{
    let l:past_posit = line("'\"")
    if l:past_posit > 0 && l:past_posit <= line('$')
        execute 'normal! g`"'
    endif
endfunction " }}}
