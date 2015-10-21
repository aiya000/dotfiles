let g:vimshell_hereis_file         = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vimsh'))
let g:vimshell_hereis_alias_prefix = get(g:, 'vimshell_hereis_alias_prefix', 'place_')

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
	let l:place_name = substitute(a:args[0], '\s', '', 'g')
	let l:place_path = getcwd()
	let l:alias_name = g:vimshell_hereis_alias_prefix . l:place_name
	let l:new_alias  = printf("alias %s='cd \"%s\"'\<CR>", l:alias_name, l:place_path)
	let l:var_name   = g:vimshell_hereis_alias_prefix . l:place_name
	let l:new_var    = printf('let $%s = "%s"', l:var_name, l:place_path)
	let l:new_lines  = [l:new_alias, l:new_var]

	call writefile(l:new_lines, g:vimshell_hereis_file, 'a')
	call vimshell#interactive#send('vimsh ' . g:vimshell_hereis_file)
	execute 'normal! o' . '>> place file reloaded'
endfunction
