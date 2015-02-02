let g:vimshell_hereis_file = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vim'))

let s:command = {
\	'name' : 'hereis',
\	'kind' : 'internal',
\	'description' : 'hereis is cd util'
\}

function! vimshell#commands#hereis#define()
	return s:command
endfunction


" --- --- --- "


function! s:command.execute(args, context)
	let l:name = substitute(a:args[0], '\s', '', 'g')
	let l:body = printf("alias %s='%s'", l:name, getcwd())
	let l:new_alias =
	\	['call vimshell#interactive#send("' . l:body . '")']

	call writefile(l:new_alias, g:vimshell_hereis_file, 'a')
endfunction
