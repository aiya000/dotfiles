" script local functions

" Do a:cmd to a:bufnr without changing the buffer of current window
function! s:buffer_do(bufnr, cmd) abort " {{{
	let l:current_buf = winbufnr('.')
	silent! execute 'buffer' s:created_buf
	silent! execute a:cmd
	silent! execute 'buffer' l:current_buf
endfunction " }}}

" Create scratch buffer
" Return its buffer number
" not like :bufdo
function! s:create_scratch_buf(filetype) abort " {{{
	new
	setl buftype=nofile
	execute 'setl filetype=' . a:filetype
	return winbufnr('.')
endfunction " }}}

"#-=- -=- -=- -=- -=- -=- -=- -=- -=-#"


" Define cnoreabbr with cmd completion
function! vimrc#cmd#cmd_cnoreabbr(...) abort " {{{
	let l:UNUSED_VALUE = 'NOP'
	let l:cmd_name     = a:1
	let l:cmd_detail   = join(a:000[1:], ' ')
	execute 'cnoreabbr' l:cmd_name l:cmd_detail
	execute 'command!'  l:cmd_name l:UNUSED_VALUE
endfunction " }}}

" Reverse ranged lines
function! vimrc#cmd#reverse_line() abort range " {{{
	if a:firstline is a:lastline
		return
	endif

	let l:lines = []
	let l:posit = getpos('.')

	let l:z = @z
	for l:line in range(a:firstline, a:lastline)
		execute 'normal! "zdd'
		call add(l:lines, @z)
	endfor

	for l:r in l:lines
		let @z = l:r
		execute 'normal! "zP'
	endfor
	let @z = l:z

	call setpos('.', l:posit)
endfunction " }}}

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
		call vimrc#echo_error(printf('Rename %s to %s is failed', l:this_file, l:new_file))
		return
	endif

	execute ':edit' l:new_file
	silent write
	silent execute ':bdelete' l:this_file

	echo printf('Renamed %s to %s', l:this_file, l:new_file)
endfunction " }}}

"@Bugs('The exception on :RedirToVar @" highlight')
" Substitute result to a variable easily
function! vimrc#cmd#redir_to_var(bang, args_str) abort " {{{
	let l:args        = split(a:args_str, '\s')
	let l:var_name    = escape(l:args[0], '"')  " bug, boooon.
	let l:expr        = join(l:args[1:], ' ')
	" redir to register or variable
	let l:is_register = stridx(l:var_name, '@') > -1
	let l:direction   = l:is_register ? l:var_name : ('=> g:' . l:var_name)

	if a:bang && !l:is_register
		execute 'unlet! g:' . l:var_name
	endif
	if exists('g:' . l:var_name)
		call vimrc#echo_error('That variable exists.')
		call vimrc#echo_error('If you want to overwrite variable, call with bang.')
		return
	endif

	execute printf('redir %s | silent %s | redir END', l:direction, l:expr)
endfunction " }}}

" Make session_name from git repository
" and Save current session by :UniteSessionSave
function! vimrc#cmd#git_branch_session_save() abort " {{{
	let repo_path = system('git rev-parse --show-toplevel')

	let repo_name  = fnamemodify(repo_path, ':t')
	let repo_name_ = substitute(repo_name, '\n', '', '')  " Remove tail line break

	let branch_name  = system(printf("cd %s ; git branch | sort | tail -1 | awk '{print $2}'", repo_path))  " Don't use double quote in awk
	let branch_name_ = substitute(branch_name, '\n', '', '')  " Remove tail line break

	let session_name  = repo_name_ . '-' . branch_name_
	let session_name_ = substitute(session_name, '/', '-', 'g')  "NOTE: '.' cannot be used as session name

	execute 'UniteSessionSave' session_name_
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
	\	nomodifiable
	\	buftype=nofile
	\	filetype=css
endfunction " }}}

" Return <title>\(.*\)</title> from web
function! vimrc#cmd#pull_webpage_title(target_url) abort " {{{
	let l:precmd = 'curl -sS %s'
	\            . '| grep "<title>.*</title>"'
	\            . '| head'
	\            . '| sed "s;<title>\(.*\)</title>;\1;g"'
	let l:cmd    = printf(l:precmd, a:target_url)
	let l:result = vimrc#system(l:cmd)
	return substitute(l:result, "\r\n", '', 'g')
endfunction " }}}

" read! to created buffer or new buffer
function! vimrc#cmd#read_bang_to_buf(cmd) abort " {{{
	let s:created_buf = get(s:, 'created_buf', v:null)
	let l:read_cmd    = 'read!' . a:cmd

	if !buflisted(s:created_buf)
		let s:created_buf = s:create_scratch_buf('read-bang')
	endif
	call s:buffer_do(s:created_buf, 'normal! ggdG')
	call s:buffer_do(s:created_buf, l:read_cmd)
endfunction " }}}
