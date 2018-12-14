" Clone dein.vim to target dir
function! vimrc#fetch_dein(install_dirname) " {{{
    if executable('git')
        echo 'dein.vim was not installed yet.'
        echo 'Installing dein.vim now.'
        execute '!git clone https://github.com/Shougo/dein.vim' a:install_dirname
    else
        call vimrc#echo_error('Sorry, You do not have git command.')
        call vimrc#echo_error('I cannot introduce dein.vim.')
        throw 'FALIED: cloning deim.vim failed.'
    endif
endfunction " }}}

" Execute :terminal and :setf to it, and
" `a:command` will be opened in current window's directory.
" Also this resorbs :terminal's defference of vim and neovim
"
"   open_mode: 'vertical' | 'horizontal' | 'stay' | 'tabnew'
function! vimrc#open_terminal_as(filetype, open_mode, command, from_this_buffer) abort " {{{
    " NOTE: %:p:h may not be a valid directory, e.g :terminal's buffer has term://.//xxxxx:/bin/zsh
    " Open at this buffer, or open at the current directory
    let path = a:from_this_buffer && isdirectory(expand('%:p:h'))
        \ ? expand('%:p:h')
        \ : getcwd()

    " TODO: Vim should cd at a tab
    let tcd = has('nvim') ? ':tcd' : ':cd'

    if a:open_mode ==# 'vertical'
        vnew
    elseif a:open_mode ==# 'horizontal'
        new
    elseif a:open_mode ==# 'stay'
        enew!
    elseif a:open_mode ==# 'tabnew'
        tabnew
    else
        throw 'undefined open_mode is detected: ' . string(a:open_mode)
    endif

    let current = getcwd()
    execute tcd fnameescape(path)
    if has('nvim')
        execute 'terminal' a:command
    else
        execute 'terminal' '++curwin' a:command
    endif
    execute tcd current

    execute 'setf' a:filetype
endfunction " }}}
