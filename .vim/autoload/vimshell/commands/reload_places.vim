let g:vimshell_hereis_file = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vimsh'))

let s:command = {
\	'name'        : 'reload_places',
\	'kind'        : 'internal',
\	'description' : 'reload paths in g:vimshell_hereis_file that was defined by hereis.vim'
\}

function! vimshell#commands#reload_places#define()
	return s:command
endfunction

" --- --- --- "

function! s:command.execute(args, context)
	if !filereadable(g:vimshell_hereis_file)
		execute 'normal! o' . '!!!>> no such places file!!!'
		execute 'normal! o' . '!!!>> please check value of g:vimshell_hereis_file!!!'
	else
		call vimshell#interactive#send('vimsh ' . g:vimshell_hereis_file)
		execute 'normal! o' . '>> place file reloaded'
	endif
endfunction
