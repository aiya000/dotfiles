execute 'source' (g:vimrc['vim_home'] . '/after/ftplugin/term-shell.vim')

nnoremap <buffer> Q :<C-u>call <SID>gina_commit_close()<CR>

function s:gina_commit_close() abort
  call term_sendkeys(bufnr('%'), "exit\<CR>")

  if vimrc#has_two_or_more_tabpages()
    tabclose
  else
    call vimrc#open_scratch_buffer()
    put!='Nice commit!'
    normal! jdd
    write
    only
  endif
endfunction
