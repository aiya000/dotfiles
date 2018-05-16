execute 'source' (g:vimrc['vim_home'] . '/after/ftplugin/haskell.vim')

let b:undo_ftplugin = 'setl ' . join([
    \ 'commentstring<',
\])

let &commentstring = '{- %s -}'
setl ts=2 sw=2 et tw=0

vnoremap <buffer><silent> i{ :Alignta => {<CR>gv:Alignta => }<CR>

filetype indent off

augroup FtpluginHappy
    autocmd!
    autocmd BufWrite *.y
        \  if g:vimrc.filetype_haskell.please_tell_me_compile_errors
            \| echo 'stack build is started'
            \| QuickRun stack_test
        \| endif
augroup END
