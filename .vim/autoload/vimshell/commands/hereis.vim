let g:vimshell_hereis_file         = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vim'))
let g:vimshell_hereis_alias_prefix = get(g:, 'vimshell_hereis_alias_prefix', 'place')

let s:command = {
\	'name'        : 'hereis',
\	'kind'        : 'internal',
\	'description' : 'hereis {place_name} - hereis is cd util'
\}

function! vimshell#commands#hereis#define()
	return s:command
endfunction


" --- --- --- "


function! s:command.execute(args, context)
	let l:name      = substitute(a:args[0], '\s', '', 'g')
	let l:new_alias = printf( "alias %s-%s='cd \"%s\"'\<CR>"
	\                       , g:vimshell_hereis_alias_prefix, l:name, getcwd())

	call writefile([l:new_alias], g:vimshell_hereis_file, 'a')
	call vimshell#interactive#send('vimsh ' . g:vimshell_hereis_file)
	call vimshell#interactive#send('echo "place file reloaded"')
endfunction
