execute 'source' (g:vimrc['vim_home'] . '/after/ftplugin/haskell.vim')

let b:undo_ftplugin = 'setl ' . join([
    \ 'commentstring<',
\])

let &commentstring = '-- %s'
setl ts=2 sw=2 et tw=0

vnoremap <buffer><silent> i{ :Alignta => {<CR>gv:Alignta => }<CR>
