let b:undo_ftplugin = 'setl ' . join([
\	'cursorline<',
\	'statusline<'
\])

setl cursorline
setl statusline+=\ %L

nnoremap         <buffer> <C-j> <CR>zz<C-w>p
nnoremap <silent><buffer> dd    :call <SID>quickfix_del_entry()<CR>
nnoremap <silent><buffer> u     :<C-u>call <SID>quickfix_undo_del_entry()<CR>
nnoremap <silent><buffer> <C-r> :<C-u>call <SID>quickfix_redo_del_entry()<CR>
nnoremap <silent><buffer> Q     :<C-u>bdelete<CR>

vnoremap <silent><buffer> d     :call <SID>quickfix_del_entry()<CR>

"#-=- -=- -=- -=- -=- -=- -=- -=- -=-#"

let s:undoer_loaded = get(s:, 'undoer_loaded', 0)
if s:undoer_loaded
	finish
endif

" undoer

"{{{

function! s:undoer_new(...) dict
	if empty(a:000)
		return deepcopy(self)
	endif

	let  l:instance = deepcopy(self)
	call l:instance.push(a:1)
	return l:instance
endfunction

function! s:undoer_push(elem) dict
	call add(self.data, a:elem)
	let self.stash = []
endfunction

function! s:undoer_undo() dict
	let elem = remove(self.data, -1)
	call add(self.stash, deepcopy(elem))
	return elem
endfunction

function! s:undoer_redo() dict
	let elem = remove(self.stash, 0)
	call add(self.data, deepcopy(elem))
	return elem
endfunction

function! s:undoer_can_undo() dict
	return !empty(self.data)
endfunction

function! s:undoer_can_redo() dict
	return !empty(self.stash)
endfunction

let s:Undoer = {
\	'data'     : [],
\	'stash'    : [],
\	'new'      : function('s:undoer_new'),
\	'push'     : function('s:undoer_push'),
\	'undo'     : function('s:undoer_undo'),
\	'redo'     : function('s:undoer_redo'),
\	'can_undo' : function('s:undoer_can_undo'),
\	'can_redo' : function('s:undoer_can_redo')
\}
lockvar! s:Undoer

"}}}

"#--- --- ---#"

" Delete a quickfix item
function! s:quickfix_del_entry() range
	let l:quickfix   = getqflist()
	let w:qf_history = get(w:, 'qf_history', s:Undoer.new())
	call w:qf_history.push(deepcopy(l:quickfix))
	VimConsoleLog 'del >>'
	VimConsoleLog w:qf_history

	unlet! l:quickfix[a:firstline - 1 : a:lastline - 1]
	call setqflist(l:quickfix, 'r')
	execute a:firstline
endfunction

" Undo delete
function! s:quickfix_undo_del_entry()
	let l:pos        = getpos('.')
	let w:qf_history = get(w:, 'qf_history', s:Undoer.new())

	if w:qf_history.can_undo()
		let  l:prev = w:qf_history.undo()
		call setqflist(l:prev, 'r')
		call setpos('.', l:pos)
		VimConsoleLog 'undo >>'
		VimConsoleLog w:qf_history
	else
		echo 'quickfix undo stack is empty'
	endif
endfunction


" Redo delete
"@Incomplete('what?')
function! s:quickfix_redo_del_entry()
	let l:pos        = getpos('.')
	let w:qf_history = get(w:, 'qf_history', s:Undoer.new())

	if w:qf_history.can_redo()
		VimConsoleLog 'redo >>'
		let  l:next = w:qf_history.redo()
		call setqflist(l:next, 'r')
		call setpos('.', l:pos)
		VimConsoleLog w:qf_history
	else
		echo 'quickun redo stack is empty'
	endif
endfunction

"#-=- -=- -=- -=- -=- -=- -=- -=- -=-#"

let s:undoer_loaded = 1
