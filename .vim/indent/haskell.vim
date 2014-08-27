if exists('b:did_indent')
	finish
endif

setlocal autoindent
setlocal indentexpr=GetHaskellIndent()
setlocal indentkeys=!^F,o,O

setlocal expandtab
setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2

let b:undo_indent = 'setlocal '.join([
\	'autoindent<',
\	'expandtab<',
\	'indentkeys<',
\	'shiftwidth<',
\	'softtabstop<',
\	'tabstop<',
\])

function! GetHaskellIndent()
	let plnum = prevnonblank(v:lnum -1)
	if getline(plnum) =~#'\v^\s*(class|instance|where|let|in|then)'
		return indent(plnum) + &l:shiftwidth
	else
		return -1
	endif
endfunction

let b:did_indent = 1
