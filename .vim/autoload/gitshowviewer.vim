function! gitshowviewer#git_show_viewer(args)
	new
	setl buftype=nofile
	call s:read_git_show(a:args)
	set  filetype=gitshowviewer
	set  foldmethod=expr
	set  foldexpr=FoldExprOfGitShow(v:lnum)
endfunction

function! s:read_git_show(args)
	let s:system = exists('*vimproc#system') ? function('vimproc#system')
	\                                        : function('system')
	let l:z = @z
	let @z  = s:system('git show ' . a:args)
	normal! "zP
	let @z  = l:z
	normal! gg
endfunction

function! FoldExprOfGitShow(lnum)
	return getline(a:lnum)     =~# '^@@'   ? '>1'
	\    : getline(a:lnum + 1) =~# '^diff' ? '<1'
	\    : getline(a:lnum + 1) =~# '^@@'   ? '<1'
	\                                      : '='
endfunction
