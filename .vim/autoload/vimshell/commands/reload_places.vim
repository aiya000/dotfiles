let g:vimshell_hereis_file = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vim'))

let s:command = {
\	'name' : 'reload_places',
\	'kind' : 'internal',
\	'description' : 'reload paths that was defined by hereis - g:vimshell_hereis_file'
\}

function! vimshell#commands#reload_places#define()
	return s:command
endfunction


" --- --- --- "


function! s:command.execute(args, context)
	if !filereadable(g:vimshell_hereis_file)
		call vimshell#interactive#send('echo "!!!>> no such places file!!!"')
		call vimshell#interactive#send('echo "!!!>> please check value of g:vimshell_hereis_file!!!"')
	else
		call vimshell#interactive#send('vimsh ' . g:vimshell_hereis_file)
		call vimshell#interactive#send('echo "place file reloaded"')
	endif
endfunction
