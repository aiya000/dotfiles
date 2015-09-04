let g:vimshell_hereis_file = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vim'))

let s:command = {
\	'name'        : 'edit_places',
\	'kind'        : 'internal',
\	'description' : 'simply open places file ( g:vimshell_hereis_file )'
\}

function! vimshell#commands#edit_places#define()
	return s:command
endfunction


" --- --- --- "


function! s:command.execute(args, context)
	if !filereadable(g:vimshell_hereis_file)
		call vimshell#interactive#send('echo "!!!>> no such places file!!!"')
		call vimshell#interactive#send('echo "!!!>> please check value of g:vimshell_hereis_file!!!"')
	else
		call vimshell#interactive#send('vim ' . g:vimshell_hereis_file)
		call vimshell#interactive#send('echo "place file opened"')
	endif
endfunction
