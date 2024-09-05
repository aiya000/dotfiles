let b:undo_ftplugin = 'setl ' . join([
  \ 'number<',
  \ 'relativenumber<',
\ ])
  " \ 'syntax=gina-commit',
  " \ 'filetype=markdown',

setl number relativenumber
" filetype=markdown syntax=gina-commit

normal! gg

nnoremap <buffer><silent> ZZ <Cmd>call <SID>close()<CR>

function s:close() abort
  let gina_commit_very_verbose = get(b:, 'gina_commit_very_verbose', v:false)
  wq

  if !gina_commit_very_verbose
    quit
    return
  endif

  if vimrc#has_two_or_more_tabpages()
    tabclose
    return
  endif

  only
endfunction
