if exists('b:did_indent')
	finish
endif

set indentexpr=MyHaskellIndent()

function! MyHaskellIndent() abort
	let l:IGNORED_TOKENS = [
	\	'import',
	\] | lockvar! l:IGNORED_TOKENS

	let l:INSERTED_TOKENS = [
	\	'->',
	\	'=',
	\	'case',
	\	'do',
	\	'if',
	\	'where',
	\] | lockvar! l:INSERTED_TOKENS

	let l:prev_valid_lnum = prevnonblank(v:lnum - 1)
	let l:prev_indent     = indent(l:prev_valid_lnum)
	let l:next_indent     = l:prev_indent

	let l:ignored_tokens_regex = printf('\(%s\)', join(l:IGNORED_TOKENS, '\|'))
	if getline(l:prev_valid_lnum) =~# l:ignored_tokens_regex
		return l:next_indent
	endif

	let l:inserted_tokens_regex = printf('\(%s\)', join(l:INSERTED_TOKENS, '\|'))
	if getline(l:prev_valid_lnum) =~# l:inserted_tokens_regex
		let l:next_indent += &sw
	endif

	return l:next_indent
endfunction
