let g:vimshell_hereis_file = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vim'))

let s:command = {
\	'name' : 'edit_places',
\	'kind' : 'internal',
\	'description' : 'simply open places file - g:vimshell_hereis_file'
\}

function! vimshell#commands#edit_places#define()
	return s:command
endfunction


" --- --- --- "


function! s:command.execute(args, context)
	let l:file = g:vimshell_hereis_file
	if !filereadable(l:file)
		echoerr 'no such places file'
	else
		call vimshell#interactive#send('vim ' . l:file)
	endif
endfunction
