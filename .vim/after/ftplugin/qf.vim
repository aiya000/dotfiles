let b:undo_ftplugin = 'setl ' . join([
\	'cursorline<',
\	'statusline<'
\])

setl cursorline
setl statusline+=\ %L


"#-=- -=- -=- -=- -=- -=- -=- -=- -=-#"

" dict Zipper {{{

function! s:zipper_shift() dict
	let l:under = self.under
	let l:over  = self.over

	let l:item = remove(l:under, -1)

	call add(l:over, l:item)

	return l:item
endfunction

function! s:zipper_unshift() dict
	let l:under = self.under
	let l:over  = self.over

	let l:item = remove(l:over, 0)
	call add(l:under, l:item)

	return l:item
endfunction

function! s:zipper_clear_under() dict
	let self.under = []
endfunction

function! s:zipper_clear_over() dict
	let self.over = []
endfunction

function! s:zipper_take_current() dict
	return self.under[0]
endfunction

function! s:zipper_get_zipper() dict
	return {'under' : self.under, 'over' : self.over}
endfunction

function! s:zipper_new(list) dict
	return extend({'under' : a:list}, self, 'keep')
endfunction

function! s:zipper_add_under(item) dict
	call add(self.under, a:item)
endfunction


let s:Zipper = {
\	'new'          : function('s:zipper_new'),
\	'add_under'    : function('s:zipper_add_under'),
\	'shift'        : function('s:zipper_shift'),
\	'unshift'      : function('s:zipper_unshift'),
\	'clear_under'  : function('s:zipper_clear_under'),
\	'clear_over'   : function('s:zipper_clear_over'),
\	'take_current' : function('s:zipper_take_current'),
\	'get_zipper'   : function('s:zipper_get_zipper'),
\	'under'        : [],
\	'over'         : []
\}


" }}}

"#--- --- ---#"


" Suppress an exception
if !exists('*s:quickfix_del_entry')
	" Delete a quickfix item
	function s:quickfix_del_entry() range
		let l:quickfix   = getqflist()
		let w:qf_history = get(w:, 'qf_history', s:Zipper.new([]))

		call w:qf_history.clear_over()
		call w:qf_history.add_under(copy(l:quickfix))

		unlet! l:quickfix[a:firstline - 1 : a:lastline - 1]

		call setqflist(l:quickfix, 'r')
		execute a:firstline
	endfunction
endif

if !exists('*s:quickfix_undo_del_entry')
	" Undo delete
	function s:quickfix_undo_del_entry()
		let l:pos        = getpos('.')
		let w:qf_history = get(w:, 'qf_history', s:Zipper.new([]))

		if !empty(w:qf_history.under)
			call setqflist(w:qf_history.shift(), 'r')
			call setpos('.', l:pos)
		else
			echo 'quickfix undo stack is empty'
		endif
	endfunction
endif


if !exists('*s:quickfix_redo_del_entry')
	"@Imcomplete('')
	" Redo delete
	function s:quickfix_redo_del_entry()
		let l:pos        = getpos('.')
		let w:qf_history = get(w:, 'qf_history', s:Zipper.new([]))

		if !empty(w:qf_history.over)
			call setqflist(w:qf_history.unshift(), 'r')
			call setpos('.', l:pos)
		else
			echo 'quickun redo stack is empty'
		endif
	endfunction
endif


"#-=- -=- -=- -=- -=- -=- -=- -=- -=-#"


nnoremap         <buffer> <C-j> <CR>zz<C-w>p
nnoremap <silent><buffer> dd    :call <SID>quickfix_del_entry()<CR>
nnoremap <silent><buffer> u     :<C-u>call <SID>quickfix_undo_del_entry()<CR>
nnoremap <silent><buffer> <C-r> :<C-u>call <SID>quickfix_redo_del_entry()<CR>
nnoremap <silent><buffer> Q     :<C-u>bdelete<CR>

vnoremap <silent><buffer> d     :call <SID>quickfix_del_entry()<CR>
