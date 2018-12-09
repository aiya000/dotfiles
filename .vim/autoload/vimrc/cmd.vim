let s:Msg = vital#vimrc#new().import('Vim.Message')

" Rename the file of current buffer
function! vimrc#cmd#rename_to(new_name) abort " {{{
  let this_file = fnameescape(expand('%'))
  let new_name  = fnameescape(a:new_name)

  if fnamemodify(this_file, ':t') ==# new_name
    call vimrc#echo_error('New name is same old name, operation abort')
    return
  endif

  let file_editing = &modified
  if file_editing
    call vimrc#echo_error('Please :write this file')
    return
  endif

  let new_file = fnamemodify(this_file, ':h') . '/' . new_name
  let failed   = rename(this_file, new_file)
  if failed
    call s:Msg.error(printf('Rename %s to %s is failed', this_file, new_file))
    return
  endif

  execute ':edit' new_file
  silent write
  silent execute ':bdelete' this_file

  echo printf('Renamed %s to %s', this_file, new_file)
endfunction " }}}

" Make session_name from git repository
" and Save current session by :UniteSessionSave
function! vimrc#cmd#git_branch_session_save() abort " {{{
  let repo_path = fnameescape(system('git rev-parse --show-toplevel'))

  let repo_name  = fnamemodify(repo_path, ':t')
  let repo_name_ = substitute(repo_name, '\n', '', '')  " Remove tail line break

  let branch_name  = system(printf("cd %s ; git branch | sort | tail -1 | awk '{print $2}'", repo_path))  " Don't use double quote in awk
  let branch_name_ = substitute(branch_name, '\n', '', '')  " Remove tail line break

  let session_name  = repo_name_ . '-' . branch_name_
  let session_name_ = substitute(session_name, '/', '-', 'g')
  let session_name__ = substitute(session_name_, '#', '-', 'g')  "NOTE: '#' shouldn't be used as a file name

  execute 'mksession!' (g:vimrc['sessiondir'] . '/' . session_name__ . '.vim')
endfunction " }}}

" Generate decompress css from compressed css to temporary buffer
function! vimrc#cmd#decompress_to_buffer() abort " {{{
    " Yank detail
    let lines = getline('^', '$')
    " Output detail as pretty style css to new buffer
    new
    for line in reverse(lines)
        1put!=line
    endfor
    execute '%s/}/\r}\r\r/g'
    execute '%s/{/ {\r/g'
    execute '%s/@/\r@/g'
    execute '%s/;/;\r/g'
    execute '%s/,/,\r/g'
    execute 'normal! gg=Ggg'
    " Set options
    setl noswapfile
    \    nomodifiable
    \    buftype=nofile
    \    filetype=css
endfunction " }}}

" Define cnoreabbr with cmd completion
function! vimrc#cmd#cmd_cnoreabbr(...) abort " {{{
    let UNUSED_VALUE = 'NOP'
    let cmd_name     = a:1
    let cmd_detail   = join(a:000[1:], ' ')
    execute 'cnoreabbr' cmd_name cmd_detail
    execute 'command!'  cmd_name UNUSED_VALUE
endfunction " }}}

" Remove CmdCnoreabbr
function! vimrc#cmd#un_cmd_cnoreabbr(name) abort " {{{
    execute 'cunabbr' a:name
    execute 'delcommand' a:name
endfunction " }}}
