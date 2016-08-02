" script local functions

function! s:find_git_repository_name(current_dir, depth) abort " {{{
	if a:depth is 10
		throw new '>> not found git repository'
	endif
	let l:found_git_dir = strlen(finddir(a:current_dir, '.git')) isnot 0
	if l:found_git_dir
		return fnamemodify(a:current_dir, ':p:h:t')
	endif
	return s:find_git_repository_name(a:current_dir . '/..', a:depth + 1)
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
	let l:repository_name = s:find_git_repository_name('.', 0)
	let l:branch_name     = matchstr(gita#statusline#format('%lb'), '(\zs.*\ze)')
	let l:branch_name0    = substitute(l:branch_name, '/', '-', 'g')
	let l:session_name    = l:repository_name . '-' . l:branch_name0
	execute 'UniteSessionSave' l:session_name
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
