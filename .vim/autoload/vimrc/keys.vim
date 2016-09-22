" If list has elem, return v:true
" otherwise return v:false
function! s:contains(list, elem) abort " {{{
	for l:x in a:list
		if l:x ==# a:elem
			return v:true
		endif
	endfor
	return v:false
endfunction " }}}

"-------------------"

" Compress continuous space
function! vimrc#keys#compress_spaces() " {{{
	let l:recent_pattern = @/
	try
		substitute/\s\+/ /g
		normal! ==
	finally
		let @/ = l:recent_pattern
	endtry
	nohlsearch
endfunction " }}}

" Clear all lines end space
function! vimrc#keys#clear_ends_space() " {{{
	let l:recent_pattern = @/
	let l:curpos = getcurpos()
	try
		%substitute/\s*\?$//g
	catch /E486/
		echo 'nothing todo'
	finally
		let @/ = l:recent_pattern
		call setpos('.', l:curpos)
	endtry
endfunction " }}}

" Move cursor to topmost of this indent
function! vimrc#keys#cursor_up_to_lid() " {{{
	while 1
		let l:p = virtcol('.')
		normal! k

		let l:indent_changed = l:p isnot virtcol('.')
		if l:indent_changed || line('.') is 1  " top line
			if l:indent_changed
				normal! j
			endif
			break
		endif
	endwhile
endfunction " }}}

" Move cursor to bottommost of this indent
function! vimrc#keys#cursor_down_to_ground() " {{{
	let l:last_line = line('$')
	while 1
		let l:p = virtcol('.')
		execute 'normal! j'

		let l:indent_changed = l:p isnot virtcol('.')
		if l:indent_changed || line('.') is l:last_line
			if l:indent_changed
				execute 'normal! k'
			endif
			break
		endif
	endwhile
endfunction " }}}

" Toggle foldmethod marker or syntax
function! vimrc#keys#toggle_foldmethod() " {{{
	if &foldmethod !=# 'syntax'
		setl foldmethod=syntax
	else
		setl foldmethod=marker
	endif
	setl foldmethod?

	if foldlevel('.') > 0 && foldclosed('.') isnot -1
		normal! zO
	endif
endfunction " }}}

" Toggle diffthis - diffoff
function! vimrc#keys#toggle_diff() " {{{
	if &diff
		diffoff
		nunmap <buffer> {
		nunmap <buffer> }
	else
		diffthis
		nnoremap <buffer> { [c
		nnoremap <buffer> } ]c
	endif
	set diff?
endfunction " }}}

" If you has nofile buffer, close it.
function! vimrc#keys#bufclose_filetype(filetypes) " {{{
	let l:closed = 0
	for l:w in range(1, winnr('$'))
		let l:buf_ft = getwinvar(l:w, '&filetype')
		if s:contains(a:filetypes, l:buf_ft)
			execute ':' . l:w . 'wincmd w'
			execute ':quit'
			let l:closed = 1
		endif
	endfor
	return l:closed
endfunction " }}}

" Toggle open netrw explorer ( vertical split )
function! vimrc#keys#toggle_netrw_vexplorer() " {{{
	let l:closed = vimrc#keys#bufclose_filetype('netrw')
	if !l:closed
		Vexplore
	endif
endfunction " }}}

" Do :bufdo without changing current buffer
function! vimrc#keys#motionless_bufdo(cmd) abort " {{{
	NewOverridden
	setl buftype=nofile
	execute 'bufdo' a:cmd
	quit
endfunction " }}}

" Toggle showing indent-guides with variable
function! vimrc#keys#toggle_indent_guides() " {{{
	let g:vimrc#keys#indent_guides_enable = !g:vimrc#keys#indent_guides_enable
	IndentGuidesToggle
endfunction " }}}

" Wrap vimrc#cmd#pull_webpage_title() for insertion
function! vimrc#keys#insert_webpage_title() abort " {{{
	let l:result = vimrc#cmd#pull_webpage_title(@+)
	return l:result
endfunction " }}}
