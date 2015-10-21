let g:vimshell_hereis_file = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vimsh'))

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
		execute 'normal! o' . '!!!>> no such places file!!!'
		execute 'normal! o' . '!!!>> please check value of g:vimshell_hereis_file!!!'
	else
		execute ':tabnew' g:vimshell_hereis_file
		stopinsert
	endif
endfunction
