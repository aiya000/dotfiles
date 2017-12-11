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
function! vimrc#open_terminal_as(filetype, open_mode, command) abort " {{{
    let buf_dir = fnameescape(expand('%:p:h'))
    " If it is not valid directory (e.g :terminal's buffer has term://.//xxxxx:/bin/zsh)
    if !isdirectory(buf_dir)
        let buf_dir = getcwd()
    endif

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

    execute ':tcd' buf_dir
    if has('nvim')
        execute 'terminal' a:command
    else
        execute 'terminal' '++curwin' a:command
    endif

    execute 'setf' a:filetype
endfunction " }}}
