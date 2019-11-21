execute 'source' (g:vimrc['vim_home'] . '/after/ftplugin/stack_build.vim')

nnoremap <buffer><silent> <C-r> :<C-u>call <SID>reopen_stack_test()<CR>

function s:reopen_stack_test() abort
  let bufnr = winbufnr('.')
  call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test --fast', {'path': g:vimrc.path_at_started, 'noclose': v:true})
  execute 'bdelete' bufnr
endfunction
