execute 'source' (g:vimrc['vim_home'] . '/after/ftplugin/haskell.vim')

let b:undo_ftplugin = 'setl ' . join([
    \ 'commentstring<',
\])

let &commentstring = '{- %s -}'
setl ts=2 sw=2 et tw=0

filetype indent off

vnoremap <buffer><silent> i{ :Alignta => {<CR>gv:Alignta => }<CR>

augroup FtpluginHappy
    autocmd!
    " v for :StackWatchExec
    autocmd BufWritePost *.y CClear
augroup END

"NOTE: Why this on/off is needed for setting syntax
syntax off
syntax on
setl syntax=haskell
