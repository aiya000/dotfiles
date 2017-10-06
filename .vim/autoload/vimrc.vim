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

" Execute :terminal and :setf to it,
" for resorbing :terminal's defference of vim and neovim
"   open_mode: 'vertical' | 'horizontal' | 'stay' | 'tabnew'
function! vimrc#open_terminal_as(filetype, open_mode, command) abort " {{{
    if a:open_mode ==# 'vertical'
        if has('nvim')
            " NeoVim doesn't split vertically by 'vertical'
            vsp
        endif
        execute 'vertical' 'terminal' a:command
    elseif a:open_mode ==# 'horizontal'
        if has('nvim')
            sp
        endif
        execute 'terminal' a:command
    elseif a:open_mode ==# 'stay'
        if has('nvim')
            execute 'terminal' a:command
        else
            execute 'terminal' '++curwin' a:command
        endif
    elseif a:open_mode ==# 'tabnew'
        tabnew
        execute 'terminal' a:command
        only
    else
        throw 'undefined open_mode is detected: ' . string(a:open_mode)
    endif
    execute 'setf' a:filetype
endfunction " }}}
