let s:Msg = vital#vimrc#new().import('Vim.Message')

" Rename the file of current buffer
function! vimrc#cmd#rename_to(new_name) abort " {{{
    let l:this_file = fnameescape(expand('%'))
    let l:new_name  = fnameescape(a:new_name)

    if fnamemodify(l:this_file, ':t') ==# l:new_name
        call vimrc#echo_error('New name is same old name, operation abort')
        return
    endif

    let l:file_editing = &modified
    if l:file_editing
        call vimrc#echo_error('Please :write this file')
        return
    endif

    let l:new_file = fnamemodify(l:this_file, ':h') . '/' . l:new_name
    let l:failed   = rename(l:this_file, l:new_file)
    if l:failed
        call s:Msg.error(printf('Rename %s to %s is failed', l:this_file, l:new_file))
        return
    endif

    execute ':edit' l:new_file
    silent write
    silent execute ':bdelete' l:this_file

    echo printf('Renamed %s to %s', l:this_file, l:new_file)
endfunction " }}}

" Make session_name from git repository
" and Save current session by :UniteSessionSave
function! vimrc#cmd#git_branch_session_save() abort " {{{
    let sessions_dir = $HOME . '/.backup/vim_backup/session'

    let repo_path = fnameescape(system('git rev-parse --show-toplevel'))

    let repo_name  = fnamemodify(repo_path, ':t')
    let repo_name_ = substitute(repo_name, '\n', '', '')  " Remove tail line break

    let branch_name  = system(printf("cd %s ; git branch | sort | tail -1 | awk '{print $2}'", repo_path))  " Don't use double quote in awk
    let branch_name_ = substitute(branch_name, '\n', '', '')  " Remove tail line break

    let session_name  = repo_name_ . '-' . branch_name_
    let session_name_ = substitute(session_name, '/', '-', 'g')
    let session_name__ = substitute(session_name_, '#', '-', 'g')  "NOTE: '#' shouldn't be used as a file name

    execute 'mksession!' (sessions_dir . '/' . session_name__ . '.vim')
endfunction " }}}

" Generate decompress css from compressed css to temporary buffer
function! vimrc#cmd#decompress_to_buffer() abort " {{{
    " Yank detail
    let l:lines = getline('^', '$')
    " Output detail as pretty style css to new buffer
    new
    for l:line in reverse(l:lines)
        1put!=l:line
    endfor
    %s/}/\r}\r\r/g
    %s/{/ {\r/g
    %s/@/\r@/g
    %s/;/;\r/g
    %s/,/,\r/g
    execute 'normal! gg=Ggg'
    " Set options
    setl noswapfile
    \    nomodifiable
    \    buftype=nofile
    \    filetype=css
endfunction " }}}
