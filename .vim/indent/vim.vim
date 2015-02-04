if exists('b:did_indent')
	finish
endif

source $VIMRUNTIME/indent/vim.vim
setlocal indentexpr=GetVimIndentOverwrite()

function! GetVimIndentOverwrite()
	let lnum = prevnonblank(v:lnum - 1)

	if getline(v:lnum) !~ '^\s*\\'
		while lnum > 0 && getline(lnum) =~ '^\s*\\'
			let lnum = lnum - 1
		endwhile
	endif

	if lnum == 0
		return 0
	endif

	let ind = indent(lnum)
	if getline(v:lnum) =~ '^\s*\\' && v:lnum > 1 && getline(lnum) !~ '^\s*\\'
		if exists("g:vim_indent_cont")
			let ind = ind + g:vim_indent_cont
		else
			"let ind = ind + &sw * 3
		endif
	elseif getline(lnum) =~ '\(^\||\)\s*\(if\|wh\%[ile]\|for\|try\|cat\%[ch]\|fina\%[lly]\|fu\%[nction]\|el\%[seif]\)\>'
		let ind = ind + &sw
	elseif getline(lnum) =~ '^\s*aug\%[roup]' && getline(lnum) !~ '^\s*aug\%[roup]\s*!\=\s\+END'
		let ind = ind + &sw
	endif

	let line = getline(lnum)
	let i = match(line, '[^\\]|\s*\(ene\@!\)')
	if i > 0 && line !~ '^\s*au\%[tocmd]'
		if !has('syntax_items') || synIDattr(synID(lnum, i + 2, 1), "name") !~ '\(Comment\|String\)$'
			let ind = ind - &sw
		endif
	endif


	if getline(v:lnum) =~ '^\s*\(ene\@!\|cat\|fina\|el\|aug\%[roup]\s*!\=\s\+END\)'
		let ind = ind - &sw
	endif

	return ind
endfunction

let b:did_indent = 1
