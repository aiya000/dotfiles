function! gitdiffviewer#git_diff_viewer(args)
	new
	setl buftype=nofile
	call s:read_git_diff(a:args)
	set  filetype=gitdiffviewer
	set  foldmethod=expr
	set  foldexpr=FoldExprOfGitDiff(v:lnum)
endfunction

function! s:read_git_diff(args)
	let s:system = exists('*vimproc#system') ? function('vimproc#system')
	\                                        : function('system')
	let l:z = @z
	let @z  = s:system('git diff ' . a:args)
	let @z  = @z ==# '' ? '>> Difference is Nothing!' : @z
	normal! "zP
	let @z  = l:z
	normal! gg
endfunction

function! FoldExprOfGitDiff(lnum)
	return getline(a:lnum)     =~# '^@@'   ? '>1'
	\    : getline(a:lnum + 1) =~# '^diff' ? '<1'
	\    : getline(a:lnum + 1) =~# '^@@'   ? '<1'
	\                                      : '='
endfunction
