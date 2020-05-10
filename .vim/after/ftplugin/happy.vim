execute 'source' (g:vimrc['vim_home'] . '/after/ftplugin/haskell.vim')

let b:undo_ftplugin = 'setl ' . join([
  \ 'commentstring<',
\ ])

let &commentstring = '{- %s -}'
setl ts=2 sw=2 et tw=0

vnoremap <buffer><silent> i{ :<C-u>call <SID>align()<CR>

function! s:align() abort
  const begin = 'Alignta <<1:0 {'
  const fix = 's/{\([^%]\)/{ \1/'
  const end = 'Alignta <<1 }'

  execute ":'<,'>" . begin
  execute ":'<,'>" . fix
  execute ":'<,'>" . end
endfunction
