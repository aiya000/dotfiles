let g:vimshell_hereis_file = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vim'))

let s:command = {
\	'name' : 'places',
\	'kind' : 'internal',
\	'description' : 'view define paths by hereis'
\}

function! vimshell#commands#places#define()
	return s:command
endfunction


" --- --- --- "


function! s:command.execute(args, context)
	let l:file = g:vimshell_hereis_file
	if !filereadable(l:file)
		echoerr 'no such places file'
		return
	endif

	for l:place in readfile(l:file)
		call vimshell#interactive#send('echo ' . l:place)
	endfor
endfunction
