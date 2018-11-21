execute 'source' (g:vimrc['vim_home'] . '/after/ftplugin/term-stack-ghci.vim')
nnoremap <buffer> <localleader>S :<C-u>Aref stackage <C-r>=expand('<cword>')<CR><CR>
