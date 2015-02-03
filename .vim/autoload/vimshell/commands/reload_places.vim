let g:vimshell_hereis_file = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vim'))

let s:command = {
\	'name' : 'reload_places',
\	'kind' : 'internal',
\	'description' : 'reload paths that was defined by hereis'
\}

function! vimshell#commands#reload_places#define()
	return s:command
endfunction


" --- --- --- "


function! s:command.execute(args, context)
	let l:file = g:vimshell_hereis_file
	if !filereadable(l:file)
		echoerr 'no such places file'
	else
		call vimshell#interactive#send('vimsh ' . l:file)
	endif
endfunction
