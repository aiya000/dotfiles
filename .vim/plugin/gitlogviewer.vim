" Inspired by ujihisa's vimrc
" And deris's code (http://deris.hatenablog.jp/entry/2013/05/10/003430)
function! s:GitLogViewer()
	if !exists(':VimProcRead')
		echohl ERROR
		echo   "You don't have vimproc.vim or Your vimproc.vim is invalid ."
		echo   "This plugin required vimproc.vim ."
		echohl NONE
	endif

	new
	setl buftype=nofile
	VimProcRead git log -u 'ORIG_HEAD..HEAD'
	execute 'normal! gg"_dd'
	set filetype=gitlogviewer-diff
	setlocal syntax=diff
	setlocal foldmethod=expr
	setlocal foldexpr=getline(v:lnum)=~'^commit'?'>1':getline(v:lnum+1)=~'^commit'?'<1':'='
	setlocal foldtext=FoldTextOfGitLog()
endfunction
command! GitLogViewer call s:GitLogViewer()

function! FoldTextOfGitLog() "{{{
	let month_map = {
	\	'Jan' : '01',
	\	'Feb' : '02',
	\	'Mar' : '03',
	\	'Apr' : '04',
	\	'May' : '05',
	\	'Jun' : '06',
	\	'Jul' : '07',
	\	'Aug' : '08',
	\	'Sep' : '09',
	\	'Oct' : '10',
	\	'Nov' : '11',
	\	'Dec' : '12',
	\}

	if getline(v:foldstart) !~ '^commit'
		return getline(v:foldstart)
	endif
	
	if getline(v:foldstart + 1) =~ '^Author:'
		let author_lnum = v:foldstart + 1
	elseif getline(v:foldstart + 2) =~ '^Author:'
		" commitの次の行がMerge:の場合があるので
		let author_lnum = v:foldstart + 2
	else
		" commitの下2行がどちらもAuthor:で始まらなければ諦めて終了
		return getline(v:foldstart)
	endif

	let date_lnum = author_lnum + 1
	let message_lnum = date_lnum + 2

	let author = matchstr(getline(author_lnum), '^Author: \zs.*\ze <.\{-}>')
	let date = matchlist(getline(date_lnum), ' \(\a\{3}\) \(\d\{1,2}\) \(\d\{2}:\d\{2}:\d\{2}\) \(\d\{4}\)')
	let message = getline(message_lnum)

	let month = date[1]
	let day = printf('%02s', date[2])
	let time = date[3]
	let year = date[4]

	let datestr = join([year, month_map[month], day], '-')

	return join([datestr, time, author, message], ' ')
endfunction "}}}
