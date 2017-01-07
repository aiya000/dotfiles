if exists('b:did_indent')
	finish
endif

set indentexpr=MyHaskellIndent()

function! MyHaskellIndent() abort
	let l:TOKENS = [
	\	'=',
	\	'if',
	\	'case',
	\	'do',
	\	'->'
	\] | lockvar! l:TOKENS

	let l:prev_valid_lnum = prevnonblank(v:lnum - 1)
	let l:prev_indent     = indent(l:prev_valid_lnum)
	let l:next_indent     = l:prev_indent

	let l:tokens_regex = printf('\(%s\)', join(l:TOKENS, '\|'))
	if getline(l:prev_valid_lnum) =~# l:tokens_regex
		let l:next_indent += &sw
		echo l:prev_indent
	endif

	return l:next_indent
endfunction
