execute 'source' (g:vimrc['vim_home'] . '/after/ftplugin/haskell.vim')

let b:undo_ftplugin = 'setl ' . join([
  \ 'commentstring<',
\ ])

let &commentstring = '{- %s -}'
setl ts=2 sw=2 et tw=0

vnoremap <buffer><silent> i{ :<C-u>call <SID>align()<CR>

function! s:align() abort
  let begin = 'Alignta => \s{\s'
  let end = 'Alignta => \s}'
  let s = 's/\s{\s\+\(.\+\)\s\s}/{ \1 }/'

  execute ":'<,'>" . begin
  execute ":'<,'>" . end
  execute ":'<,'>" . s
endfunction
