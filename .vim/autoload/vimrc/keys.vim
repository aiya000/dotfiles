let s:V    = vital#vimrc#new()
let s:HTML = s:V.import('Web.HTML')
let s:Msg  = s:V.import('Vim.Message')

" If list has elem, return v:true
" otherwise return v:false
function! s:contains(list, elem) abort " {{{
    for l:x in a:list
        if l:x ==# a:elem
            return v:true
        endif
    endfor
    return v:false
endfunction " }}}

"-------------------"

" Compress continuous space
function! vimrc#keys#compress_spaces() " {{{
    let l:recent_pattern = @/
    try
        substitute/\s\+/ /g
        normal! ==
    finally
        let @/ = l:recent_pattern
    endtry
    nohlsearch
endfunction " }}}

" Clear all lines end space
function! vimrc#keys#clear_ends_space() " {{{
    let l:recent_pattern = @/
    let l:curpos = getcurpos()
    try
        %substitute/\s*\?$//g
    catch /E486/
        echo 'nothing todo'
    finally
        let @/ = l:recent_pattern
        call setpos('.', l:curpos)
    endtry
endfunction " }}}

" Toggle diffthis - diffoff
function! vimrc#keys#toggle_diff() " {{{
    if &diff
        diffoff
        "NOTE: This restores a [c and ]c keymaps of .vimrc,
        "NOTE: please see [c, ]c, [ale-previous], and [ale-next] at .vimrc.
        nmap <buffer> [c [ale-previous]
        nmap <buffer> ]c [ale-next]
    else
        diffthis
        " Get the origin
        nnoremap <buffer> [c [c
        nnoremap <buffer> ]c ]c
    endif
    set diff?
endfunction " }}}

" If you has nofile buffer, close it.
function! vimrc#keys#bufclose_filetype(filetypes) " {{{
    let l:closed = 0
    for l:w in range(1, winnr('$'))
        let l:buf_ft = getwinvar(l:w, '&filetype')
        if s:contains(a:filetypes, l:buf_ft)
            execute ':' . l:w . 'wincmd w'
            execute ':quit'
            let l:closed = 1
        endif
    endfor
    return l:closed
endfunction " }}}

" Toggle open netrw explorer ( vertical split )
function! vimrc#keys#toggle_netrw_vexplorer() " {{{
    let l:closed = vimrc#keys#bufclose_filetype(['netrw'])
    if !l:closed
        call vimrc#keys#netrw_wrapper('vertical')
    endif
endfunction " }}}

" Avoid a behavior that netrw cannot be opened on :terminal buffer
function! vimrc#keys#netrw_wrapper(open_method) abort " {{{
    let open = a:open_method ==# 'stay'       ? 'enew'
        \    : a:open_method ==# 'horizontal' ? 'new'
        \    : a:open_method ==# 'vertical'   ? 'vertical new'
        \    : a:open_method ==# 'tabnew'     ? 'tabnew'
        \    : s:Msg.error(printf("'%s' is not expected", a:open_method))
    execute open
    execute 'tcd' fnameescape(getcwd())
    Explore
endfunction " }}}

" Toggle showing indent-guides with variable
function! vimrc#keys#toggle_indent_guides() " {{{
    let g:vimrc#keys#indent_guides_enable = !g:vimrc#keys#indent_guides_enable
    IndentGuidesToggle
endfunction " }}}

" Get a detail of <title> from + register
function! vimrc#keys#get_webpage_title() abort " {{{
    return substitute(s:HTML.parseURL(@+).find('title').value(), '·', '-', 'g')
endfunction " }}}

" Toggle keymapping <leader>V (and etc) to :terminal or vimshell
function! vimrc#keys#toggle_shell_mode() "{{{
    " Base properties
    let l:SHELL_MODE = {
    \   'vimshell' : ':VimShell',
    \   'terminal' : ':terminal'
    \} | lockvar! l:SHELL_MODE

    " Toggle values
    let g:vimrc#keys#shell_mode =
    \   get(g:, 'vimrc#keys#shell_mode', l:SHELL_MODE.vimshell) ==# l:SHELL_MODE.terminal
    \       ? l:SHELL_MODE.vimshell
    \       : l:SHELL_MODE.terminal

    " Toggle keymappings
    if g:vimrc#keys#shell_mode ==# l:SHELL_MODE.vimshell
        nnoremap <silent> <leader>v         :<C-u>call vimrc#open_terminal_as('term-shell', 'vertical', &shell)<CR>
        nnoremap <silent> <leader><leader>v :<C-u>call vimrc#open_terminal_as('term-shell', 'horizontal', &shell)<CR>
        nnoremap <silent> <leader>V         :<C-u>call vimrc#open_terminal_as('term-shell', 'stay', &shell)<CR>
        nnoremap <silent> <leader><leader>V :<C-u>call vimrc#open_terminal_as('term-shell', 'tabnew', &shell)<CR>
        echo 'shell mode ' . l:SHELL_MODE.terminal
    elseif g:vimrc#keys#shell_mode ==# l:SHELL_MODE.terminal
        nnoremap <silent> <leader>v         :<C-u>VimShell -split-command=vsp -toggle<CR>
        nnoremap <silent> <leader><leader>v :<C-u>VimShell -split-command=sp  -toggle<CR>
        nnoremap <silent> <leader>V         :<C-u>VimShellBufferDir -create<CR>
        nnoremap <silent> <leader><leader>V :<C-u>VimShell -split-command=tabnew -create<CR>
        echo 'shell mode ' . l:SHELL_MODE.vimshell
    endif
endfunction "}}}

" :quit if only a window is existent,
" :hide otherwise
function! vimrc#keys#hide_or_quit() abort " {{{
    let tabnum = tabpagenr('$')
    let winnum = tabpagewinnr(tabpagenr(), '$')
    if tabnum is 1 && winnum is 1
        quit
    else
        hide
    endif
endfunction " }}}

" Toggle b:ale_enabled
function! vimrc#keys#toggle_ale_at_buffer() abort
    let b:ale_enabled = get(b:, 'ale_enabled', 0)
    " Refresh the state
    ALEToggle
    ALEToggle
endfunction
