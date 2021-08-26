" params:
"   bufnr: int
"     A buffer number to insert `git status` result
"   stdout: Array<string>
"   stderr: Array<string>
function s:show_git_status_to(bufnr, stdout, stderr) abort
  echomsg a:stdout
  throw 'Not implemented yet (gitstatusviewer#git_status_viewer)'
endfunction

function gitstatusviewer#git_status_viewer(args) abort
  enew!
  setl buftype=nofile
  set filetype=gitstatusviewer

  const bufnr = v:null  " TODO

  call vimrc#job#start_simply(
    \ ['git', 'status', '--short', a:args],
    \ function('s:show_git_status_to', [bufnr]),
  \ )
endfunction
